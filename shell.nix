{
  pkgs ? import (
    fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.05.tar.gz";
      sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
    }) {},
  hpkgs ? import (
    fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/20.09.tar.gz";
      sha256 = "1wg61h4gndm3vcprdcg7rc4s1v3jkm5xd7lw8r2f67w502y94gcy";
    }) {}
}:
with pkgs;
mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskell-language-server
  ];
}
