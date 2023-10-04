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
  let
    pkgs = nixpkgs.legacyPackages.${config.system};
    postgresqlPackage = pkgs.postgresql_14;
  in
  {
    envs.dev =
    let
      basePort = 10000;
      pgPort = 5432;

      PGUSER = "jannie";
      PGPASSWORD ="asdf";
      PGHOST = "localhost";
      PGPORT = basePort + 5432;
      PGDATABASE = "jannie";
    in {
      ghc.compiler = "ghc946";
      hls.enable = true;
      inherit basePort;

      env = {
        inherit
          PGUSER
          PGPASSWORD
          PGHOST
          PGPORT
          PGDATABASE;
      };

      services.postgres = {
        enable = true;
        config = {
          package = postgresqlPackage;
          name = PGDATABASE;
          # note that this automatically gets added to basePort by hix
          port = pgPort;
          creds = {
            user = PGUSER;
            password = PGPASSWORD;
          };
        };
      };

      buildInputs = with pkgs; [
        postgresqlPackage
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
      component.language = "GHC2021";
      default-extensions = [
        "DerivingStrategies"
        "DeriveAnyClass"
        "DataKinds"
        "BlockArguments"
        "LambdaCase"
        "ExplicitNamespaces"
        "RecordWildCards"
        "OverloadedRecordDot"
        "OverloadedStrings"
        "UndecidableInstances"
        "TypeFamilies"
      ];
      ghc-options = [
        "-Wall"
        "-Wunused-type-patterns"
        "-Wunused-packages"
        "-Wmissing-deriving-strategies"
        "-Wredundant-constraints"
        "-Widentities"
        "-Wmissing-export-lists"
        "-Wno-name-shadowing"
      ];
    };

    packages.jannie = {
      src = ./.;
      cabal.meta.synopsis = "A discord bot";
      override = {nodoc, ...}: nodoc;

      library = {
        enable = true;
        dependencies = [
          "aeson"
          "discord-haskell == 1.15.6"
          "text"
          "persistent"
          "persistent-postgresql"
          "mtl"
          "monad-logger"
          "optparse-generic"
          "unliftio"
          "bytestring"
          "dotenv >= 0.11"
          "regex-tdfa"
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
