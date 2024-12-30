# myNixOS

### Remote machine nixos installation example
```bash
nix run nixpkgs#nixos-anywhere -- \
    --flake .#homeserver \
    --generate-hardware-config nixos-generate-config ./systems/dmitry-homeserver/hardware-configuration.nix \
    root@192.168.0.211
```
