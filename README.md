# KovalevDima

My personal page located at:\
https://kovalevdima.github.io/KovalevDima/

In repository you can find:

1. Source code for personal page
2. My NixOS machines configuration
3. 

```text
├personal-page/      Personal page source code
├modules/
|├home/              Home manager modules
|└system/            System modules
└systems/
 ├*/                 System-specific configurations
 |├default.nix       Full system configuration
 |├hardware.nix      Hardware configuration
 |└disks.nix         Disks configuration
 └secrets.yaml
```

## Remote machine NixOS installation
```bash
nix run nixpkgs#nixos-anywhere -- \
    --flake .#homeserver \
    --generate-hardware-config nixos-generate-config ./systems/dmitry-homeserver/hardware.nix \
    root@192.168.0.211
```
