{
  description = "A basic Haskell development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        
        haskellPackages = pkgs.haskellPackages;
        
        source = builtins.path {
          name = "scorm-source";
          path = ./scorm;
          filter = path: type:
            let
              pathStr = toString path;
              name = baseNameOf pathStr;
            in
            name != ".git" &&
            name != ".direnv" &&
            name != "result" &&
            name != "dist-newstyle" &&
            name != ".envrc";
        };
        
        scorm = pkgs.haskell.lib.addBuildDepends
          (haskellPackages.callCabal2nix "scorm" source { })
          [ pkgs.zlib ];
        
        scormWithTests = pkgs.haskell.lib.doCheck scorm;
      in
      {
        packages.default = scorm;
        
        checks.default = scormWithTests;
        
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            zlib
          ];

          shellHook = ''
            echo "Haskell development environment"
            echo "GHC version: $(ghc --version)"
            echo "Cabal version: $(cabal --version)"
          '';
        };
      }
    );
}

