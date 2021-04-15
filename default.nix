{ nixpkgs     ? import ./nix/nixpkgs.nix {}
, compiler    ? "ghc8104"
, doBenchmark ? false
}:

let

  inherit (nixpkgs) pkgs;

  baseHaskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  readDirectory = import ./nix/readDirectory.nix;

  haskellPackages =
    let
      manualOverrides = haskellPackagesNew: haskellPackagesOld: {
         # Add manual overrides.
         # Example:
         #   Diff = pkgs.haskell.lib.dontCheck haskellPackagesOld.Diff;
      };
    in
      baseHaskellPackages.override {
        overrides = pkgs.lib.composeExtensions ( readDirectory ./nix/sources ) manualOverrides;
      };

  doBench  = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  doStatic = pkgs.haskell.lib.justStaticExecutables;
  CPP = doStatic (haskellPackages.callCabal2nix "CPP" ./. {});
in
  {
    inherit CPP haskellPackages;
    shell = CPP.env;
  }
