{ nixpkgs     ? import ./nix/nixpkgs.nix {}
, compiler    ? "ghc884"
, doBenchmark ? false
}:
let
  inherit (nixpkgs) pkgs;
  default = import ./default.nix { inherit nixpkgs compiler doBenchmark; };
  haskellPackages = default.haskellPackages;
  shell = default.shell;
in
  shell.overrideAttrs (oldAttrs: {
    buildInputs =
        oldAttrs.buildInputs ++ (with haskellPackages; [
          #cabal-install
          #ghcid
          haskell-language-server
        ]);
  })
