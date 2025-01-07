## Remote machine NixOS installation
```bash
nix run nixpkgs#nixos-anywhere -- \
    --flake .#homeserver \
    --generate-hardware-config nixos-generate-config ./systems/dmitry-homeserver/hardware.nix \
    root@192.168.0.211
```
