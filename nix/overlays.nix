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
     }
  )

  (self: super:
    {
      haskellPackages = super.haskellPackages.override {
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
  (self: super:
    { haskellPackages = super.haskellPackages.extend
        (super.haskell.lib.packageSourceOverrides
          { cheops-email = super.lib.cleanSource ../cheops-email;
            # tests are failing on MacOS
            prometheus-client         = (fetch "prometheus-haskell") + "/prometheus-client";
            prometheus-client-extra   = (fetch "prometheus-haskell") + "/prometheus-client-extra";
            prometheus-metrics-ghc    = (fetch "prometheus-haskell") + "/prometheus-metrics-ghc";
            # Packages from cheops project. We should eventually put them to the separate repo or even opensource:
            cheops-logger = (super.fetchzipSirius {
              name = "archive.zip";
              url = "https://gitlab.sirius.online/api/v4/projects/12/repository/archive.zip?sha=11e7004421a8b91569af14723a9f4c9fc353d776&s=.zip";
              sha256 = "sha256:0g8jzisgpxywfam3id5lwzzyjqgpkvi3dcxsyy3wlj8x51xmwgnd";
              }) + "/backend/cheops-logger";
            naming-conventions = (super.fetchzipSirius {
              name = "archive.zip";
              url = "https://gitlab.sirius.online/api/v4/projects/12/repository/archive.zip?sha=11e7004421a8b91569af14723a9f4c9fc353d776&s=.zip";
              sha256 = "sha256:0g8jzisgpxywfam3id5lwzzyjqgpkvi3dcxsyy3wlj8x51xmwgnd";
              }) + "/backend/naming-conventions";
            extended-clock = (super.fetchzipSirius {
              name = "archive.zip";
              url = "https://gitlab.sirius.online/api/v4/projects/12/repository/archive.zip?sha=11e7004421a8b91569af14723a9f4c9fc353d776&s=.zip";
              sha256 = "sha256:0g8jzisgpxywfam3id5lwzzyjqgpkvi3dcxsyy3wlj8x51xmwgnd";
              }) + "/backend/extended-clock";
            sirius-environment = (super.fetchzipSirius {
              name = "archive.zip";
              url = "https://gitlab.sirius.online/api/v4/projects/12/repository/archive.zip?sha=11e7004421a8b91569af14723a9f4c9fc353d776&s=.zip";
              sha256 = "sha256:0g8jzisgpxywfam3id5lwzzyjqgpkvi3dcxsyy3wlj8x51xmwgnd";
              }) + "/backend/sirius-environment";
            cheops-db = (super.fetchzipSirius {
              name = "archive.zip";
              url = "https://gitlab.sirius.online/api/v4/projects/12/repository/archive.zip?sha=11e7004421a8b91569af14723a9f4c9fc353d776&s=.zip";
              sha256 = "sha256:0g8jzisgpxywfam3id5lwzzyjqgpkvi3dcxsyy3wlj8x51xmwgnd";
              }) + "/backend/cheops-db";
         }
        );
    }
  )

]
