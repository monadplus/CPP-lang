{ compiler ? "ghc8104" }:

let

  # bultins.fetchGit is slow
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore.nix";
    rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
    sha256 = "sha256:06j7wpvj54khw0z10fjyi31kpafkr6hi1k0di13k1xp8kywvfyx8";
  };

  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  nixpkgs = builtins.fetchTarball {
    url =
      "https://github.com/NixOS/nixpkgs/archive/29b0d4d0b600f8f5dd0b86e3362a33d4181938f9.tar.gz";
    sha256 = "10cafssjk6wp7lr82pvqh8z7qiqwxpnh8cswnk1fbbw2pacrqxr1";
  };

  config = { };

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages."${compiler}".override (old: {
          overrides = let
            packageSources =
              self.haskell.lib.packageSourceOverrides { "CPP" = gitignoreSource ./.; };

            manualOverrides = haskellPackagesNew: haskellPackagesOld:
              {
                # Diff =
                #   pkgs.haskell.lib.dontCheck haskellPackagesOld.Diff;
              };

            default = old.overrides or (_: _: { });

          in self.lib.fold self.lib.composeExtensions default [
            packageSources
            manualOverrides
          ];
        });
      };
    };
  };

  pkgs = import nixpkgs {
    inherit config;
    overlays = [ overlay ];
  };

in {
  inherit (pkgs.haskell.packages."${compiler}") CPP;

  shell = (pkgs.haskell.packages."${compiler}".CPP).env;

  inherit (pkgs.releaseTools) aggregate;
}
