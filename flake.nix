{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      nixpkgs,
      rust-overlay,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
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
            rust-bin.stable.latest.default
          ];
        };
      }
    );
}
