{
  nixConfig.extra-substituters = "https://simmsb-calamity.cachix.org";
  nixConfig.trusted-public-keys =
    "simmsb-calamity.cachix.org-1:CQsXXpwKsjSVu0BJFT/JSvy1j6R7rMSW2r3cRQdcuQM=";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        # Typically, you just want a single project named "default". But
        # multiple projects are also possible, each using different GHC version.
        haskellProjects.default = {
          overrides = self: super:
            let hlib = pkgs.haskell.lib;
            in { di-core = hlib.dontCheck super.di-core; };
        };

        packages.default = self'.packages.example;
      };
    };
}
