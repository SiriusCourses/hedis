with { fetch = import ./fetch.nix; };

[
  # system packages.
  (self: super:
     {
       glibcLocales-light = self.glibcLocales.override {
           allLocales = false;
           locales = [ "en_US.UTF-8/UTF-8" ];
       };
       # move to default.nix
       fetchzipSirius = opts: super.fetchzip (opts // {
         curlOpts = "${super.lib.optionalString (opts ? curlOpts) "${opts.curlOpts}"} --header PRIVATE-TOKEN:a6TQHBtZarD5ssyjsY5t";
       });
       cheops-package = self.fetchzipSirius {
         name = "archive.zip";
         url = "https://gitlab.sirius.online/api/v4/projects/12/repository/archive.zip?sha=3ea3c3f79b50e72df3590e56e23afbea5b722933&s=.zip";
         sha256 = "sha256:1cscw7m2ylp6mq32hi9z6rq04c2nd97jrbvmi3chh7i5ap9w90y6";
       };
     }
  )

  # docker images (TODO: move to a separate repo)
  (self: super:
      { sirius-base = self.callPackage ./pkgs/docker/base.nix {};
      }
  )

  (self: super:
    {
      haskellPackages = super.haskell.packages.ghc8103.override {
       overrides =
             let packagesFromDirectory =
                   { directory, ... }:
                   self1: super1:
                     let
                       haskellPaths = builtins.attrNames (builtins.readDir directory);

                       toKeyVal = file: {
                         name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;
                         value = self.haskell.lib.dontCheck (self1.callPackage (directory + "/${file}") { });
                       };
                     in builtins.listToAttrs (map toKeyVal haskellPaths);
             in packagesFromDirectory{ directory= ./pkgs; };
        };
  })

  # add source packages
  (self: super: {
     haskellPackages = super.haskellPackages.extend
       (super.haskell.lib.packageSourceOverrides
         { cheops-email = super.lib.cleanSource ../cheops-email;
           # tests are failing on MacOS
           hedis = fetch "hedis";
           prometheus-client       = (fetch "prometheus-haskell") + "/prometheus-client";
           prometheus-client-extra = (fetch "prometheus-haskell") + "/prometheus-client-extra";
           prometheus-metrics-ghc  = (fetch "prometheus-haskell") + "/prometheus-metrics-ghc";
           HaskellNet = (fetch "HaskellNet");
           HaskellNet-SSL = (fetch "HaskellNet-SSL");
           # Packages from cheops project. We should eventually put them to the separate repo or even opensource:
           cheops-db     = (super.cheops-package) + "/backend/cheops-db";
           cheops-lib    = (super.cheops-package) + "/backend/cheops-lib";
           cheops-logger = (fetch "cheops-logger") + "/cheops-logger";
           extended-clock = (super.cheops-package) + "/backend/extended-clock";
           naming-conventions = (super.cheops-package) + "/backend/naming-conventions";
           sirius-environment = (super.cheops-package) + "/backend/sirius-environment";
           thyme = (fetch "thyme");
           true-name = (fetch "true-name");
       });
  })

  (self: super: {
    haskellPackages = super.haskellPackages.extend
      (haskellSelf: haskellSuper: {
         # we can't add don't check in packageSourceOverrides for some reason
         hedis=super.haskell.lib.dontCheck(haskellSuper.hedis);
         prometheus-client-extra = super.haskell.lib.dontCheck(haskellSuper.prometheus-client-extra);
         thyme = super.haskell.lib.dontCheck (super.haskell.lib.disableLibraryProfiling ( haskellSuper.thyme )); 
         cheops-lib = super.haskell.lib.disableLibraryProfiling(haskellSuper.cheops-lib);
      });
  })


  (self: super: {
    haskellPackages = super.haskellPackages.extend
     (haskellSelf: haskellSuper:
      { cheops-email = self.callPackage ./pkgs/cheops-email { inherit haskellSuper; };
        cheops-email-exe = self.callPackage ./pkgs/cheops-email/cheops-email-exe.nix {
           cheops-email = self.haskellPackages.cheops-email;
        };
     });
  } // {
     docker-cheops-email = self.callPackage ./pkgs/docker/cheops-email.nix {
       cheops-email-exe = self.haskellPackages.cheops-email-exe;
     };
  })


]
