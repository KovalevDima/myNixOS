## Deploy to remote NixOS machine after installation

```bash
nixos-rebuild \
    --flake .#homeserver \
    --target-host root@192.168.0.213 \
    switch
```

```bash
nixos-rebuild \
    --flake .#laptop \
    --target-host root@192.168.0.147 \
    switch
```

## Remote machine NixOS installation
```bash
export PASS= #Enter your password here
nix run nixpkgs#nixos-anywhere -- \
    --disk-encryption-keys /tmp/disk.key <(echo -n "$PASS") \
    --flake .#homeserver \
    --generate-hardware-config nixos-generate-config ./systems/homeserver/hardware.nix \
    root@192.168.0.210
```
