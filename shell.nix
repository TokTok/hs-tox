# Build and test with hTox's Haskell packages: `nix-shell --run 'cabal test'`.
let
  hatter = import ../htox/nix/hatter.nix;
  inherit (hatter) pkgs;
  hpkgs = (import "${hatter.src}/nix/hpkgs.nix" { inherit pkgs; }).extend
    (pkgs.lib.composeExtensions (import ../htox/nix/packages.nix pkgs) (self: super: {
      tox = self.callCabal2nix "tox" ./. { };
    }));
in
hpkgs.shellFor {
  packages = ps: [ ps.tox ];
  nativeBuildInputs = [ pkgs.cabal-install ];
}
