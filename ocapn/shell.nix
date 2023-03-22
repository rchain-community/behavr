# This imports the nix package collection,
# so we can access the `pkgs` and `stdenv` variables
with import <nixpkgs> {};

# Make a new "derivation" that represents our shell
stdenv.mkDerivation {
  name = "idris-use-env";
  buildInputs = with pkgs; [
    (idrisPackages.with-packages (with idrisPackages;
        [ contrib ]))
  ];
}