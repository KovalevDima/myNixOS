Source code was moved to https://github.com/KovalevDima/KovalevDima

# myNixOS

```text
├home/
├system/
└systems/
 ├*/
 | ├default.nix
 | ├hardware.nix
 | └disks.nix
 └dmitry-secrets.yaml
```

## modules/home/
Home manager modules

## modules/system/
System modules

## systems/*/

**systems/*/default.nix**: Full system configuration

**systems/*/disks.nix**: System-specific disks configuration

**systems/*/hardware.nix**: System-specific hardware configuration

## Remote machine NixOS installation
```bash
nix run nixpkgs#nixos-anywhere -- \
    --flake .#homeserver \
    --generate-hardware-config nixos-generate-config ./systems/dmitry-homeserver/hardware.nix \
    root@192.168.0.211
```
