{
  description = "CFS Analysis Tools";

  outputs = { self, nixpkgs }: {

    packages.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux; {
      pythonPlotNine = python3.withPackages (p:
        with p; [
          plotnine
          matplotlib
          mizani
          pandas
          patsy
          scipy
          setuptools
          statsmodels
          scikit-misc
        ]
      );

      rTidyverse = rWrapper.override {
        packages = with rPackages; [
          tidyverse
#          gridExtra
#          optparse
        ];
      };

      default = buildEnv {
        name = "cfstools";
        paths = with self.packages.x86_64-linux; [
          rTidyverse
          imagemagick
        ];
      };
    };
  };
}
