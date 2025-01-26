## Deploy to remote NixOS machine after installation

```bash
nixos-rebuild --flake .#laptop --target-host root@192.168.0.147 switch
nixos-rebuild --flake .#server --target-host root@boot.directory switch
```

## Remote machine NixOS installation

```bash
export PASS= #Enter your password here
nix run nixpkgs#nixos-anywhere -- \
    --disk-encryption-keys /tmp/disk.key <(echo -n "$PASS") \
    --flake .#server \
    --generate-hardware-config nixos-generate-config ./systems/server/hardware.nix \
    root@192.168.0.210
```
