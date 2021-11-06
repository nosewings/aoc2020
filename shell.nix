{
  pkgs ? import (
    fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz";
      sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
    }) {},
}:
with pkgs;
mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}
