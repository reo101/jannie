{
  description = "A Haskell project";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    hix = {
      url = "github:tek/hix";
      inputs.nixpkgs.url = "nixpkgs";
    };
  };

  outputs =
  { nixpkgs
  , hix
  , ...
  }:
  let
    inherit (nixpkgs) lib;
  in
  hix ({config, ...}:
  let pkgs = nixpkgs.legacyPackages.${config.system};
  in
  {
    envs.dev = {
      ghc.compiler = "ghc946";
      hls.enable = true;

      buildInputs = with pkgs; [
        mktemp
      ];
    };

    overrides = {hackage, unbreak, ...}: {
      discord-haskell = unbreak;
    };

    hackage.versionFile = "ops/version.nix";

    cabal = {
      license = "MIT";
      license-file = "LICENSE";
      author = "reo101";
      ghc-options = [
        "-Wall"
        "-Wunused-type-patterns"
        "-Wunused-packages"
        "-Wmissing-deriving-strategies"
        "-Wpartial-fields"
        "-Wredundant-constraints"
        "-Widentities"
        "-Wmissing-export-lists"
        "-Wno-name-shadowing"
      ];
    };

    packages.jannie = {
      src = ./.;
      cabal.meta.synopsis = "A discord bot";

      library = {
        enable = true;
        dependencies = [
          "discord-haskell == 1.15.6"
          "text"
          "unliftio"
          "dotenv >= 0.11"
          "regex-tdfa"
          "mtl"
        ];
      };

      executable.enable = true;

      test = {
        enable = true;
        dependencies = [
          "hedgehog >= 1.1 && < 1.3"
          "tasty ^>= 1.4"
          "tasty-hedgehog >= 1.3 && < 1.5"
        ];
      };
    };
  });
}
