{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };

        ghcVersion = "ghc910";

        haskellEnv = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (ps: with ps; [
          stack
          megaparsec
          containers
          mtl
          unordered-containers
          directory
          process
          filepath
          async
          tasty
          tasty-hunit
          temporary
          silently
        ]);

        nativeBuildInputs = [
          haskellEnv

          pkgs.haskellPackages.cabal-install # Added this!
          pkgs.haskellPackages.hpack         # To convert package.yaml -> .cabal

          pkgs.nasm 
          pkgs.gcc
          pkgs.gnumake
          pkgs.binutils
        ];

        # haskellPkgs     = with pkgs; [ stack ghc ];
        # buildToolsPkgs  = with pkgs; [ nasm gcc gnumake ];
        debugPkgs       = with pkgs; [ valgrind gdb ];
        unitTestsPkgs   = with pkgs; [ python3 ];
        otherPkgs       = with pkgs; [ hlint ];
        
        allPkgs = nativeBuildInputs
          ++ debugPkgs 
          ++ unitTestsPkgs 
          ++ otherPkgs;

      in with pkgs; {
        # nix build
        packages.default = stdenv.mkDerivation {
          pname = "rune";
          version = "1.0.0";
          src = ./RuneLang;

          nativeBuildInputs = nativeBuildInputs ++ [ makeWrapper ];

          buildPhase = ''
              export HOME=$TMPDIR
              export CABAL_DIR=$TMPDIR/.cabal

              hpack

              mkdir -p $CABAL_DIR
              touch $CABAL_DIR/config

              cabal v2-build \
                --offline \
                --with-compiler=${haskellEnv}/bin/ghc \
                --package-db=clear \
                --package-db=global \
                --ghc-options="-O2" \
                all
          '';
          installPhase = ''
              mkdir -p $out/bin
              BINARY_PATH=$(find dist-newstyle -type f -executable -name "rune-exe" | head -n 1)

              if [ -n "$BINARY_PATH" ]; then
                echo "Found binary at $BINARY_PATH"
                cp "$BINARY_PATH" $out/bin/.rune-wrapped
                makeWrapper $out/bin/.rune-wrapped $out/bin/rune \
                    --prefix PATH : ${lib.makeBinPath [ nasm gcc binutils ]}
              else
                echo "Could not find 'rune-exe' in dist-newstyle"
                find dist-newstyle -type f
                exit 1
              fi
          '';
        };
        # nix dev shell
        devShells.default = mkShell {
         buildInputs = allPkgs;
         shellHook = ''
           export PKG_CONFIG_PATH=${pkgs.lib.makeLibraryPath [ ]}:$PKG_CONFIG_PATH
           export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ ]}:$LD_LIBRARY_PATH
         '';
       };
   });
}
