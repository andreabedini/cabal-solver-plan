{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = inputs@{ flake-parts, haskell-nix, ... }:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        systems = [ "x86_64-linux" ];
        perSystem = { self', system, pkgs, ... }:
          let
            project = pkgs.haskell-nix.cabalProject' {
              compiler-nix-name = "ghc94";
              src = ./.;
              shell.tools = {
                cabal = "latest";
                fourmolu = "0.14.0.0";
                haskell-language-server = "latest";
                hlint = "latest";
              };
            };

            flake = project.flake (
              pkgs.lib.attrsets.optionalAttrs
                (system == "x86_64-linux")
                { crossPlatforms = p: [ p.musl64 ]; }
            );
          in
          {
            _module.args.pkgs = haskell-nix.legacyPackages.${system};

            inherit (flake) apps checks devShells;
            packages = flake.packages // rec {
              cabal-solver-plan = flake.packages."cabal-solver-plan:exe:cabal-solver-plan";
              default = cabal-solver-plan;
            };
          };
      };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
