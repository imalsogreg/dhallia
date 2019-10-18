{ nixpkgs_rev ?
  "fba9144f92f532c5a525c468b474d1d81025d501"
  # Should be at at least as recent as 2019-10-07 master
, compiler ? "default"
, doBenchmark ? false }:

let

  nixpkgs =
    import
    (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgs_rev}.tar.gz") {};
  inherit (nixpkgs) pkgs;

  dhallSrc = pkgs.fetchFromGitHub {
    owner     = "dhall-lang";
    repo      = "dhall-haskell";
    rev       = "65710954cf8f2217b9959cd73a5d8a40d03aab39";
    sha256    = "10zwkr4wrw0ilplawp959dz9k9yndcdyfa87agcnjkyp1vgzm8rg";
  };

  f = { mkDerivation, algebraic-graphs, base, bytestring, containers, dhall, dhall-json, stdenv
      , text, row-types
      }:
      mkDerivation {
        pname = "dhallcalk";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ algebraic-graphs base bytestring containers dhall dhall-json row-types text ];
        doHaddock = false;
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = (if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler}).override {
                         overrides = self: super: {
                           algebraic-graphs = pkgs.haskell.lib.dontCheck
                             (self.callHackage "algebraic-graphs" "0.4" {});
                           dhall =
                             pkgs.haskell.lib.dontCheck
                             (self.callCabal2nix "dhall"   (dhallSrc+ "/dhall") {});
                           dhall-json =
                             pkgs.haskell.lib.dontCheck
                             (self.callCabal2nix "dhall"   (dhallSrc+ "/dhall-json") {});
                           repline = self.callHackage   "repline" "0.2.1.0"  {};
                         };
                       };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
