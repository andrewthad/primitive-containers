{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  primitive-containers = (
    with rec {
      primitive-containersSource = pkgs.lib.cleanSource ../.;
      primitive-containersBasic  = self.callCabal2nix "primitive-containers" primitive-containersSource { };
    };
    overrideCabal primitive-containersBasic (old: {
    })
  );
}
