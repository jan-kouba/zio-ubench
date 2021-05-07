{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    nix4scala.url = "git+ssh://git@gitlab.com/netmagnet/nix4scala.git";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nix4scala, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ nix4scala.overlay ];
        };
      in {
        devShell = pkgs.mkScalaShell {
          buildInputs = [
            pkgs.sbt
            pkgs.jdk
            pkgs.ammonite
            pkgs.gitMinimal
            pkgs.gnupg
          ];
        };
      }
    );
}
