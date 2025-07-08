let
  pins = {
    # merge ancestor of https://github.com/NixOS/nixpkgs/pull/413046
    nixpkgs = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/3ea4dc04503b4370e06eca0e43c062c8b11883fa.tar.gz";
      sha256 = "sha256:04ygvidh3l3ls755b2wgl148567n9rnj168s87j03agly5wrf19c";
    };
  };

  defaultNixpkgs = import pins.nixpkgs {};

  cachedPackageSetVersion = "ghc" + builtins.replaceStrings ["."] [""] defaultNixpkgs.haskellPackages.ghc.version;

  mkNixpkgs = version: import pins.nixpkgs {
    config = {
      packageOverrides = nixpkgs: with nixpkgs.haskell.lib.compose; {
        haskell = nixpkgs.haskell // {
          packages = nixpkgs.haskell.packages // {
            "${version}" = nixpkgs.haskell.packages.${version}.override(old: {
              overrides = self: super: {
                # callCabal2nix is broken because of testu01 being missing so re-use splitmix drv
                splitmix = overrideSrc
                  { src = nixpkgs.lib.sourceFilesBySuffices ./. [ ".hs" ".cabal" ".c" ".md" "LICENSE" ]; }
                  super.splitmix;
              };
            });
          };
        };
      };
    };
  };

  build = version: getPkgs: ((getPkgs (mkNixpkgs version)).haskell.packages.${version});

  in {
      native = build cachedPackageSetVersion (ps: ps);

      freebsd = build cachedPackageSetVersion (ps: ps.pkgsCross.x86_64-freebsd);

      android = build "ghc910" (ps: ps.pkgsCross.aarch64-android-prebuilt.pkgsStatic);

      js = build "ghc912" (ps: ps.pkgsCross.ghcjs);
    }
