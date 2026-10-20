{
  description = "miso: build, install, and scaffold miso apps (MicroHs inside)";

  inputs = {
    # MicroHs (pkgs.microhs) and the MicroHs package database with miso
    # (pkgs.miso-mhs) come from miso's overlay, as in miso-sampler.
    miso.url = "github:dmjio/miso";
    microcabal = {
      url = "github:augustss/MicroCabal";
      flake = false;
    };
  };

  outputs = { self, miso, microcabal }:
    miso.inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import miso.inputs.nixpkgs {
          inherit system;
          overlays = [ miso.overlays.default ];
        };
        # Linux: a fully static binary (musl).  macOS links libSystem
        # dynamically, as every macOS program must.
        static = pkgs.stdenv.hostPlatform.isLinux;
        cc = if static then pkgs.pkgsStatic.stdenv.cc else pkgs.stdenv.cc;
        mhsVersion = pkgs.microhs.version;
        # the sources miso's overlay builds its packages from
        misoSrc = import "${miso}/nix/source.nix" pkgs;

        miso-cli = pkgs.stdenv.mkDerivation {
          pname = "miso";
          version = "0.1.0";
          src = ./.;
          nativeBuildInputs = [ pkgs.microhs cc pkgs.gnumake ];
          buildPhase = ''
            runHook preBuild
            export HOME=$TMPDIR
            make MHS=mhs MHSSRC=${pkgs.microhs.src} MCABALSRC=${microcabal} \
              CC=${cc.targetPrefix}cc LDFLAGS="${if static then "-static" else ""}"
            runHook postBuild
          '';
          # The layout mhs expects of an installation: bin/ (miso, and cpphs
          # for modules with CPP) next to the
          # package database mhs-VERSION/ (base, containers, mtl, miso, ...,
          # and the runtime sources programs are compiled with).
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            cp build/miso build/cpphs $out/bin/
            cp -r ${pkgs.miso-mhs}/lib/mcabal/mhs-${mhsVersion} $out/mhs-${mhsVersion}
            chmod -R u+w $out/mhs-${mhsVersion}
            # what comes with the tool (miso list tells these from what users install)
            (cd $out/mhs-${mhsVersion}/packages && ls *.pkg) > $out/mhs-${mhsVersion}/miso-bundled
            # the sources of its libraries, as miso-mhs built them, for
            # projects that replace one of them (scripts/ship-source.sh)
            db=$out/mhs-${mhsVersion}
            sh scripts/ship-source.sh $db transformers ${misoSrc.mhs-transformers}
            # mtl 2.3.1, patched as miso's nix/mhs/default.nix does
            cp -r ${misoSrc.mhs-mtl} mtl && chmod -R u+w mtl
            sed -i 's/^instance forall k (r :: k) (m :: (k -> Type)) \. MonadCont (ContT r m) where/instance MonadCont (ContT r m) where/' mtl/Control/Monad/Cont/Class.hs
            sh scripts/ship-source.sh $db mtl mtl
            sh scripts/ship-source.sh $db containers ${misoSrc.mhs-containers}/containers
            sh scripts/ship-source.sh $db miso ${misoSrc.miso} "-template-haskell -aeson -native -production -ssr -benchmark -text"
            runHook postInstall
          '';
        };

        # The release CI built, for nix: the tarball of the GitHub release in
        # nix/release.json (which the release workflow writes), unpacked.
        # The Linux binaries are static: nothing to build or patch.
        #   nix profile install github:haskell-miso/cli#release
        release = builtins.fromJSON (builtins.readFile ./nix/release.json);
        hasRelease = release.version != null && release.assets ? ${system};
        miso-release = pkgs.stdenvNoCC.mkDerivation {
          pname = "miso";
          version = pkgs.lib.removePrefix "v" release.version;
          src = pkgs.fetchurl { inherit (release.assets.${system}) url sha256; };
          dontConfigure = true;
          dontBuild = true;
          dontFixup = true;
          installPhase = ''
            mkdir -p $out
            cp -R . $out/
          '';
          meta = {
            description = "The command line tool for miso apps (the GitHub release)";
            homepage = "https://github.com/haskell-miso/cli";
            platforms = [ system ];
          };
        };

        # The release asset: miso-SYSTEM.tar.gz, and its sha256.
        tarball = pkgs.runCommand "miso-${system}-tarball" { } ''
          mkdir -p $out work
          cp -r ${miso-cli} work/miso-${system}
          chmod -R u+w work
          tar -C work -czf $out/miso-${system}.tar.gz miso-${system}
          (cd $out && sha256sum miso-${system}.tar.gz > miso-${system}.tar.gz.sha256)
        '';
      in
      {
        packages = {
          default = miso-cli;
          inherit tarball;
        } // pkgs.lib.optionalAttrs hasRelease { release = miso-release; };
        apps.default = {
          type = "app";
          program = "${miso-cli}/bin/miso";
        };
        # nix develop: make MHSSRC=$MHSSRC MCABALSRC=$MCABALSRC
        devShells.default = pkgs.mkShell {
          packages = [ pkgs.microhs pkgs.gnumake ];
          MHSSRC = "${pkgs.microhs.src}";
          MCABALSRC = "${microcabal}";
        };
      });
}
