{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
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

          src = ./.;

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
