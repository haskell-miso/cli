{
  description = "miso: build, install, and scaffold miso apps (MicroHs inside)";

  inputs = {
    # MicroHs (pkgs.microhs) and the MicroHs package database with miso
    # (pkgs.miso-mhs) come from miso's overlay, as in miso-sampler.
    miso.url = "github:dmjio/miso";
    microcabal = {
      url = "github:haskell-miso/MicroCabal";
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
          # The layout mhs expects of an installation: bin/ next to the
          # package database mhs-VERSION/ (base, containers, mtl, miso, ...,
          # and the runtime sources programs are compiled with).
          installPhase = ''
            runHook preInstall
            mkdir -p $out/bin
            cp build/miso $out/bin/miso
            cp -r ${pkgs.miso-mhs}/lib/mcabal/mhs-${mhsVersion} $out/mhs-${mhsVersion}
            runHook postInstall
          '';
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
        packages.default = miso-cli;
        packages.tarball = tarball;
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
