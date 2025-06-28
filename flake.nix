{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages.default = pkgs.rustPlatform.buildRustPackage rec {
          pname = "void";
          version = "1.0.0";

          src = pkgs.fetchFromGitHub {
            owner = "amedoeyes";
            repo = "void-lang";
            rev = version;
            hash = "sha256-jA4wLTxsewcOgkJp12MKYDfm1fDjkeglNa88qAWwMNY=";
          };

          cargoLock = {
            lockFile = "${src}/Cargo.lock";
          };

          meta = {
            mainProgram = "void";
            description = "void-lang";
            homepage = "https://github.com/amedoeyes/void-lang";
            license = pkgs.lib.licenses.agpl3Only;
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            rustc
            cargo
          ];
        };
      }
    );
}
