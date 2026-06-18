{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { nixpkgs, ... }:
    let
      inherit (nixpkgs) lib;
      forAllSystems = lib.genAttrs lib.systems.flakeExposed;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.rustPlatform.buildRustPackage rec {
            pname = "void";
            version = "1.0.0";

            src = ./.;

            cargoLock = {
              lockFile = "${src}/Cargo.lock";
            };

            nativeBuildInputs = with pkgs; [
              binutils
              nasm
            ];

            meta = {
              mainProgram = "void";
              description = "void-lang";
              homepage = "https://github.com/amedoeyes/void-lang";
              license = pkgs.lib.licenses.agpl3Only;
            };
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cargo
              clippy
              rust-analyzer
              rustc
              rustfmt
              binutils
              nasm
              gdb
            ];
          };
        }
      );
    };
}
