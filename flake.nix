{
  description = "A Haskell project";

  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
    hix = {
      url = "github:tek/hix";
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

      buildInputs = [];

      services.postgres = {
        enable = false;
      };

      # hls.hls.package = hls.packages.${config.system}.haskell-language-server-925;
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
