{
description = "hbs2";

inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    # haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils";
    haskell-flake-utils.url = "github:ivanovs-4/haskell-flake-utils/master";
    hspup.url = "github:voidlizard/hspup";
    hspup.inputs.nixpkgs.follows = "nixpkgs";

    saltine = {
      url = "github:tel/saltine/3d3a54cf46f78b71b4b55653482fb6f4cee6b77d";
      flake = false;
    };

};

outputs = { self, nixpkgs, haskell-flake-utils, ... }@inputs:

 haskell-flake-utils.lib.simpleCabalProject2flake {
   inherit self nixpkgs;
   systems = [ "x86_64-linux" ];
   name = "hbs2";

   haskellFlakes = with inputs; [
   ];

   packageNames = [
     "hbs2"
     "hbs2-peer"
     "hbs2-core"
     "hbs2-storage-simple"
     "hbs2-tests"
   ];

   packageDirs = {
     "hbs2" = "./hbs2";
     "hbs2-tests" = "./hbs2-tests";
     "hbs2-core" = "./hbs2-core";
     "hbs2-storage-simple" = "./hbs2-storage-simple";
     "hbs2-peer" = "./hbs2-peer";
   };

   hpPreOverrides = {pkgs, ...}: final: prev: with pkgs; {
     saltine = prev.callCabal2nix "saltine" inputs.saltine { inherit (pkgs) libsodium; };
   };

   packagePostOverrides = { pkgs }: with pkgs; with haskell.lib; [
    disableExecutableProfiling
    disableLibraryProfiling
    dontBenchmark
    dontCoverage
    dontDistribute
    dontHaddock
    dontHyperlinkSource
    doStrip
    enableDeadCodeElimination
    justStaticExecutables

    dontCheck
   ];

   shellExtBuildInputs = {pkgs}: with pkgs;  [
     haskellPackages.haskell-language-server
     pkg-config
     inputs.hspup.packages.${pkgs.system}.default
   ];

 };


}
