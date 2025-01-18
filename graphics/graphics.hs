{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Strict #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (graphics)
where

import Codec.Picture                as JP (Image, PixelRGBA8 (..), withImage, encodePng)
import Control.Exception.Safe       (finally, throwString, MonadThrow)
import Control.Monad.IO.Class       (MonadIO(..))
import Control.Monad.Trans.Maybe    (MaybeT(..))
import Control.Monad.Trans.Resource (MonadResource(..), allocate, runResourceT)
import Data.Bits                    (zeroBits, (.|.), (.&.))
import Data.ByteString.Lazy         as BSL (writeFile)
import Data.Foldable                (Foldable(toList), for_, maximumBy)
import Data.List                    (partition)
import Data.Maybe                   (catMaybes)
import Data.Ord                     (comparing)
import Data.Text                    as T (Text, pack)
import Data.Text.Encoding           (decodeUtf8)
import Data.Vector                  as Vec (fromList, filter, indexed, (!?))
import Data.Word                    (Word32, Word64)
import Foreign.Marshal.Array        (peekArray)
import Foreign.Ptr                  (Ptr, castFunPtr, plusPtr)
import Foreign.Storable             (sizeOf)
import Say                          (sayErr)

import Vulkan.CStruct.Extends
import Vulkan.CStruct.Utils          (FixedArray, lowerArrayPtr)
import Vulkan.Core10                 as Vk hiding (withBuffer, withImage)
import Vulkan.Core10                 as CommandBufferBeginInfo (CommandBufferBeginInfo(..))
import Vulkan.Core10                 as CommandPoolCreateInfo (CommandPoolCreateInfo(..))
import Vulkan.Core10                 as PipelineLayoutCreateInfo (PipelineLayoutCreateInfo(..))
import Vulkan.Core10.DeviceInitialization as DI
import Vulkan.Dynamic                (DeviceCmds(..), InstanceCmds(..))
import Vulkan.Extensions.VK_EXT_debug_utils
import Vulkan.Extensions.VK_EXT_validation_features
import Vulkan.Utils.Debug (debugCallbackPtr)
import Vulkan.Utils.ShaderQQ.GLSL.Glslang (comp)
import Vulkan.Zero                   (Zero(..))
import VulkanMemoryAllocator         as VMA hiding (getPhysicalDeviceProperties)
import VulkanMemoryAllocator         as AllocationCreateInfo (AllocationCreateInfo(..))

----------------------------------------------------------------
-- The program
----------------------------------------------------------------

graphics :: IO ()
graphics = runResourceT $ do
  -- Create Instance, PhysicalDevice, Device and Allocator
  inst <- Main.createInstance

  (_, devs) <- enumeratePhysicalDevices inst
  (pdi, phys) <- maximumBy (comparing fst) . catMaybes <$>
    sequence [fmap (, d) <$> physicalDeviceInfo d | d <- toList devs]

  sayErr . ("Using device: " <>) . decodeUtf8 . deviceName
    =<< getPhysicalDeviceProperties phys
  let deviceQueueCreateInfo = 
        zero{
          queueCreateInfos = [
            SomeStruct zero{queueFamilyIndex = pdiComputeQueueFamilyIndex pdi, queuePriorities  = [1]}
          ]
        }
  (_, dev) <- withDevice phys deviceQueueCreateInfo Nothing allocate
  (_, allocator)   <- withAllocator
    zero
      { flags            = zero
      , physicalDevice   = physicalDeviceHandle phys
      , device           = deviceHandle dev
      , instance'        = instanceHandle inst
      , vulkanApiVersion = myApiVersion
      , vulkanFunctions  = Just $ case inst of
        Instance _ InstanceCmds {..} -> case dev of
          Device _ DeviceCmds {..} -> zero
            { vkGetInstanceProcAddr = castFunPtr pVkGetInstanceProcAddr
            , vkGetDeviceProcAddr   = castFunPtr pVkGetDeviceProcAddr
            }
      }
    allocate

  -- Run our application
  -- Wait for the device to become idle before tearing down any resourecs.
  (`finally` deviceWaitIdle dev)
    $ do
        image <- render dev pdi allocator
        sayErr "Writing file"
        liftIO $ BSL.writeFile "julia.png" (JP.encodePng image)

-- Render the Julia set
render :: (MonadResource io, MonadThrow io, MonadFail io)
  => Device -> PhysicalDeviceInfo -> Allocator -> io (JP.Image PixelRGBA8)
render device pdi allocator = do
  -- Some things to reuse, make sure these are the same as the values in the
  -- compute shader. TODO: reduce this duplication.
  let width, height, workgroupX, workgroupY :: Int
      width      = 512
      height     = width
      workgroupX = 32
      workgroupY = 4

  -- Create a buffer into which to render
  --
  -- Use ALLOCATION_CREATE_MAPPED_BIT and MEMORY_USAGE_GPU_TO_CPU to make sure
  -- it's readable on the host and starts in the mapped state
  (_, (buffer, bufferAllocation, bufferAllocationInfo)) <- withBuffer
    allocator
    zero { size  = fromIntegral $ width * height * 4 * sizeOf (0 :: Float)
         , usage = BUFFER_USAGE_STORAGE_BUFFER_BIT
         }
    zero { AllocationCreateInfo.flags = ALLOCATION_CREATE_MAPPED_BIT
         , usage = MEMORY_USAGE_GPU_TO_CPU
         }
    allocate

  -- Create a descriptor set and layout for this buffer
  (descriptorSet, descriptorSetLayout) <- do
    (_, descriptorPool) <- withDescriptorPool device zero
      { maxSets   = 1
      , poolSizes = [DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_BUFFER 1]
      }
      Nothing
      allocate
    (_, descriptorSetLayout) <- withDescriptorSetLayout device zero
      { bindings = [ zero { binding         = 0
                          , descriptorType  = DESCRIPTOR_TYPE_STORAGE_BUFFER
                          , descriptorCount = 1
                          , stageFlags      = SHADER_STAGE_COMPUTE_BIT
                          }
                   ]
      }
      Nothing
      allocate

    -- Allocate a descriptor set from the pool with that layout
    -- Don't use `withDescriptorSets` here as the set will be cleaned up when
    -- the pool is destroyed.
    [descriptorSet] <- allocateDescriptorSets device zero
      { descriptorPool = descriptorPool
      , setLayouts     = [descriptorSetLayout]
      }
    pure (descriptorSet, descriptorSetLayout)

  -- Assign the buffer in this descriptor set
  updateDescriptorSets device
    [ SomeStruct zero { dstSet          = descriptorSet
                      , dstBinding      = 0
                      , descriptorType  = DESCRIPTOR_TYPE_STORAGE_BUFFER
                      , descriptorCount = 1
                      , bufferInfo = [DescriptorBufferInfo buffer 0 WHOLE_SIZE]
                      }
    ]
    []

  -- Create our shader and compute pipeline
  shader              <- createShader device
  (_, pipelineLayout) <- withPipelineLayout
    device
    zero{ PipelineLayoutCreateInfo.setLayouts = [descriptorSetLayout] }
    Nothing
    allocate
  let pipelineCreateInfo :: ComputePipelineCreateInfo '[]
      pipelineCreateInfo = zero { layout             = pipelineLayout
                                , stage              = shader
                                , basePipelineHandle = zero
                                }
  (_, (_, [computePipeline])) <-
    withComputePipelines device zero [SomeStruct pipelineCreateInfo] Nothing allocate

  -- Create a command buffer
  (_, commandPool) <- withCommandPool
    device
    zero{CommandPoolCreateInfo.queueFamilyIndex = pdiComputeQueueFamilyIndex pdi}
    Nothing
    allocate

  (_, [commandBuffer]) <-
    withCommandBuffers
      device
      zero{commandPool = commandPool, level = COMMAND_BUFFER_LEVEL_PRIMARY, commandBufferCount = 1}
      allocate

  -- Fill command buffer
  useCommandBuffer commandBuffer zero{CommandBufferBeginInfo.flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT} $
    do
    cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_COMPUTE computePipeline
    cmdBindDescriptorSets commandBuffer PIPELINE_BIND_POINT_COMPUTE pipelineLayout 0 [descriptorSet] []
    cmdDispatch
      commandBuffer
      (ceiling (realToFrac width / realToFrac @_ @Float workgroupX))
      (ceiling (realToFrac height / realToFrac @_ @Float workgroupY))
      1

  -- Create a fence so we can know when render is finished
  (_, fence) <- withFence device zero Nothing allocate
  -- Submit the command buffer and wait for it to execute
  let submitInfo =
        zero { commandBuffers = [commandBufferHandle commandBuffer] }
  computeQueue <- getDeviceQueue device (pdiComputeQueueFamilyIndex pdi) 0
  queueSubmit computeQueue [SomeStruct submitInfo] fence
  let fenceTimeout = 1e9 -- 1 second
  waitForFences device [fence] True fenceTimeout >>= \case
    TIMEOUT -> throwString "Timed out waiting for compute"
    _       -> pure ()

  -- If the buffer allocation is not HOST_COHERENT this will ensure the changes
  -- are present on the CPU.
  invalidateAllocation allocator bufferAllocation 0 WHOLE_SIZE

  -- TODO: speed this bit up, it's hopelessly slow
  let pixelAddr :: Int -> Int -> Ptr (FixedArray 4 Float)
      pixelAddr x y = plusPtr (mappedData bufferAllocationInfo)
        (((y * width) + x) * 4 * sizeOf (0 :: Float))
  liftIO $ JP.withImage
    width
    height
    (\x y -> do
      let ptr = pixelAddr x y
      [r, g, b, a] <- fmap (\f -> round (f * 255))
        <$> peekArray 4 (lowerArrayPtr ptr)
      pure $ JP.PixelRGBA8 r g b a
    )

-- | Create a compute shader
createShader :: MonadResource io => Device -> io (SomeStruct PipelineShaderStageCreateInfo)
createShader device = do
  let compCode = [comp|
        #version 450
        #extension GL_ARB_separate_shader_objects : enable

        const int width = 512;
        const int height = width;
        const int workgroup_x = 32;
        const int workgroup_y = 4;

        // r^2 - r = |c|
        const vec2 c = vec2(-0.8, 0.156);
        const float r = 0.5 * (1 + sqrt (4 * dot(c,c) + 1));

        layout (local_size_x = workgroup_x, local_size_y = workgroup_y, local_size_z = 1 ) in;
        layout(std140, binding = 0) buffer buf
        {
           vec4 imageData[];
        };


        // From https://iquilezles.org/www/articles/palettes/palettes.htm
        //
        // Traditional Julia blue and orange
        vec3 color(const float t) {
          const vec3 a = vec3(0.5);
          const vec3 b = vec3(0.5);
          const vec3 c = vec3(8);
          const vec3 d = vec3(0.5, 0.6, 0.7);
          return a + b * cos(6.28318530718 * (c * t + d));
        }

        // complex multiplication
        vec2 mulC(const vec2 a, const vec2 b) {
          return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
        }

        vec2 f(const vec2 z) {
          return mulC(z,z) + c;
        }

        // Algorithm from https://en.wikipedia.org/wiki/Julia_set
        void main() {
          vec2 z = vec2
            ( float(gl_GlobalInvocationID.y) / float(height) * 2 * r - r
            , float(gl_GlobalInvocationID.x) / float(width) * 2 * r - r
            );

          uint iteration = 0;
          const int max_iteration = 1000;

          while (dot(z,z) < dot(r,r) && iteration < max_iteration) {
            z = f(z);
            iteration++;
          }

          const uint i = width * gl_GlobalInvocationID.y + gl_GlobalInvocationID.x;
          if (iteration == max_iteration) {
            imageData[i] = vec4(0,0,0,1);
          } else {
            imageData[i] = vec4(color(float(iteration) / float(max_iteration)),1);
          }
        }
      |]
  (_, compModule) <- withShaderModule device zero { code = compCode } Nothing allocate
  let compShaderStageCreateInfo = zero { stage   = SHADER_STAGE_COMPUTE_BIT
                                       , module' = compModule
                                       , name    = "main"
                                       }
  pure $ SomeStruct compShaderStageCreateInfo

----------------------------------------------------------------
-- Initialization
----------------------------------------------------------------

myApiVersion :: Word32
myApiVersion = API_VERSION_1_0

-- | Create an instance with a debug messenger
createInstance :: MonadResource m => m Instance
createInstance = do
  availableExtensionNames <-
    toList . fmap extensionName . snd
    <$> enumerateInstanceExtensionProperties Nothing
  availableLayerNames <-
    toList . fmap layerName . snd
    <$> enumerateInstanceLayerProperties

  extensions <-
    partitionOptReq
      "extension"
      availableExtensionNames
      [EXT_VALIDATION_FEATURES_EXTENSION_NAME]
      [EXT_DEBUG_UTILS_EXTENSION_NAME]
  layers <- 
    partitionOptReq
      "layer"
      availableLayerNames
      ["VK_LAYER_KHRONOS_validation"]
      []

  let debugMessengerCreateInfo = zero
        { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                              .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
        , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                            .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                            .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
        , pfnUserCallback = debugCallbackPtr
        }
      instanceCreateInfo =
        zero
          { applicationInfo = Just zero{applicationName = Nothing, apiVersion = myApiVersion}
          , enabledLayerNames     = Vec.fromList layers
          , enabledExtensionNames = Vec.fromList extensions
          }
          ::& debugMessengerCreateInfo
          :&  ValidationFeaturesEXT [VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT] []
          :&  ()
  (_, inst) <- withInstance instanceCreateInfo Nothing allocate
  _ <- withDebugUtilsMessengerEXT inst debugMessengerCreateInfo Nothing allocate
  pure inst

----------------------------------------------------------------
-- Physical device tools
----------------------------------------------------------------

-- | The Ord instance prioritises devices with more memory
data PhysicalDeviceInfo = PhysicalDeviceInfo
  { pdiTotalMemory             :: Word64
  , pdiComputeQueueFamilyIndex :: Word32
    -- ^ The queue family index of the first compute queue
  }
  deriving (Eq, Ord)

physicalDeviceInfo :: MonadIO m => PhysicalDevice -> m (Maybe PhysicalDeviceInfo)
physicalDeviceInfo phys = runMaybeT $ do
  pdiTotalMemory <- do
    heaps <- memoryHeaps <$> getPhysicalDeviceMemoryProperties phys
    pure $ sum (DI.size <$> heaps)
  pdiComputeQueueFamilyIndex <- do
    queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties phys
    let isComputeQueue q = (QUEUE_COMPUTE_BIT .&. queueFlags q /= zeroBits) && (queueCount q > 0)
        computeQueueIndices = fromIntegral . fst <$> Vec.filter
          (isComputeQueue . snd)
          (Vec.indexed queueFamilyProperties)
    MaybeT (pure $ computeQueueIndices Vec.!? 0)
  pure PhysicalDeviceInfo { .. }

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

partitionOptReq :: (Show a, Eq a, MonadIO m) => Text -> [a] -> [a] -> [a] -> m [a]
partitionOptReq type' available optional required = do
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
  for_ optMissing
    $ \n -> sayErr $ "Missing optional " <> type' <> ": " <> (T.pack . show) n
  case reqMissing of
    []  -> pure ()
    [x] -> sayErr $ "Missing required " <> type' <> ": " <> (T.pack . show) x
    xs  -> sayErr $ "Missing required " <> type' <> "s: " <> (T.pack . show) xs
  pure (reqHave <> optHave)
