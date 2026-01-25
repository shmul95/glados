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

          nativeBuildInputs = nativeBuildInputs;
          # buildInputs = [ ];

#           # the stack way
#           buildPhase = ''
#             export HOME=$TMPDIR
#             export STACK_ROOT=$TMPDIR/.stack
#             export STACK_IN_NIX_SHELL=1
#
#             # Create a minimal stack config that doesn't need to go online
#             cat > stack.yaml <<EOF
# resolver: ghc-9.10.2
# system-ghc: true
# install-ghc: false
# packages:
#   - '.'
# EOF
#             stack config set system-ghc --global true
#             make all STACK_NIX_FLAGS="--system-ghc --no-install-ghc --offline --nix"
#           '';
          # the cabal way
          buildPhase = ''
              export HOME=$TMPDIR
              export CABAL_DIR=$TMPDIR/.cabal

              hpack

              mkdir -p $CABAL_DIR
              touch $CABAL_DIR/config

              # cabal build --offline --ghc-options="-O2"
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
            find dist-newstyle -type f -name "rune-exe" -exec cp {} ./rune \;
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
