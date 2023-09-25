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
    hls = {
      url = "github:haskell/haskell-language-server?ref=2.2.0.0";
    };
  };

  outputs =
  { nixpkgs
  , hix
  , hls
  , ...
  }:
  let
    inherit (nixpkgs) lib;
  in
  hix ({config, ...}: {
    envs.main = {
      ghc.compiler = "ghc902";

      buildInputs = with nixpkgs.legacyPackages.${config.system}; [
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
      ghc-options = ["-Wall"];
    };

    packages.jannie = {
      src = ./.;
      cabal.meta.synopsis = "A discord bot";

      library = {
        enable = true;
        dependencies = [
          "containers"
          "emojis == 0.1.3"
          "discord-haskell == 1.15.6"
          "bytestring"
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
