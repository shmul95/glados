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
          pkgs.haskellPackages.cabal-install
          pkgs.haskellPackages.hpack
          pkgs.nasm 
          pkgs.gcc
          pkgs.gnumake
          pkgs.binutils
        ];

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
                -j$NIX_BUILD_CORES \
                --with-compiler=${haskellEnv}/bin/ghc \
                --package-db=clear \
                --package-db=global \
                --ghc-options="-O2" \
                all
          '';
          installPhase = ''
              # some utf8 char can break and stop the compilation of the lib
              export HASKELLEG_UTF8=1
              export LANG=en_US.UTF-8
              export LC_ALL=en_US.UTF-8
              
              export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"

              mkdir -p $out/bin
              mkdir -p $out/lib

              BINARY_PATH=$(find dist-newstyle -type f -executable -name "rune-exe" | head -n 1)

              LIB_PATH=./lib/std
              LIB_SRC_PATH=$(find $LIB_PATH -name "*.ru" -type f)

              INC_PATH=./inc
              cp -r $INC_PATH $out/inc

              if [ -z "$BINARY_PATH" ]; then echo "Error: rune-exe not found"; exit 1; fi

              $BINARY_PATH build $LIB_SRC_PATH -shared -o $out/lib/libstd.so

               cp "$BINARY_PATH" $out/bin/.rune-raw
 
               makeWrapper $out/bin/.rune-raw $out/bin/rune \
                  --prefix PATH : ${lib.makeBinPath [ nasm gcc binutils ]} \
                  --set LD_LIBRARY_PATH "$out/lib" \
                  --set RUNE_LIB_DIR "$out/lib"
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
      }) // {
        templates.default = {
          path = ./template/standard; # Ensure this directory exists in your repo!
          description = "Standard Rune project template";
        };
      };
}
