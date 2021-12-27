final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = prev.lib.composeExtensions (old.overrides or (_: _: {}))
    (hfinal: hprev: {
      rainwater-collector = hfinal.callCabal2nix "rainwater-collector" (./.) { };
        });
  });
}
