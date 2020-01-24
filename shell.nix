{ nixpkgs_rev ?
  "fba9144f92f532c5a525c468b474d1d81025d501"
  # Should be at at least as recent as 2019-10-07 master
, all_cabal_hashes_rev ?
  "33856621c362bae377b7b95d233c0112e891af72"
, compiler ? "default"
, doBenchmark ? false }:

let

allCabalHashesOverlay = self: super: {
 all-cabal-hashes = builtins.fetchurl
   { url =
       "https://github.com/commercialhaskell/all-cabal-hashes/archive/${all_cabal_hashes_rev}.tar.gz";
     name =
       "all-cabal-hashes-${all_cabal_hashes_rev}.tar.gz";
   };
  };

  # nixpkgs =
  #   import
  #   (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/${nixpkgs_rev}.tar.gz") {
  #   overlays = [ allCabalHashesOverlay ];
  #   };

  nixpkgs = import  <nixpkgs> { overlays = [allCabalHashesOverlay]; };

  inherit (nixpkgs) pkgs;

  dhallSrc = pkgs.fetchFromGitHub {
    owner     = "dhall-lang";
    repo      = "dhall-haskell";
    rev       = "7b414d9846ceb056fd7ee6a8147c03e3dd87ae60";
    sha256    = "1r26pqvk599qd0rhagr1pl2rzhg4rjd8h4338h575n8vbkmhphnl";
  };

  f = { mkDerivation, algebraic-graphs, base, bytestring, containers, dhall, dhall-json, stdenv
      , text, row-types, cabal-install, snap-core, snap-server, aeson
      }:
      mkDerivation {
        pname = "dhallia";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ cabal-install algebraic-graphs base
                                  bytestring containers dhall dhall-json
                                  row-types text snap-core snap-server aeson];
        doHaddock = false;
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = (if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler}).override {
                         overrides = self: super: {
                           aeson-yaml = self.callHackage "aeson-yaml" "1.0.2.0" {};
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
