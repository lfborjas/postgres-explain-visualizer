{system ? builtins.currentSystem}:
# NOTE(luis)
# All packages listed here will be built from source, as they're not
# 'blessed' in our pinned nix version. The sources themselves _are_
# obtained from cache.nixos.org, but need to be rebuilt regardless.
# Exercise restraint!
let
  dontCheck   = (import ./packages.nix{inherit system;}).haskell.lib.dontCheck;
  doJailbreak = (import ./packages.nix{inherit system;}).haskell.lib.doJailbreak;
in (super: {
  #servant = super.servant_0_18;
  #servant-server = super.servant-server_0_18;
  fused-effects  = dontCheck super.fused-effects;
  
  optics = super.optics_0_4;
  optics-th = super.optics-th_0_4;
  optics-extra = super.optics-extra_0_4;
  optics-core= super.optics-core_0_4;
})
