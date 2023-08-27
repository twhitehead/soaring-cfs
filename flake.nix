{
  description = "Python with plotnine";

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

      rGGPlot = rWrapper.override {
        packages = with rPackages; [
          tidyverse
#          gridExtra
#          optparse
        ];
      };

      default = buildEnv {
        name = "GGPlot and PlotNine";
        paths = with self.packages.x86_64-linux; [
          pythonPlotNine
          rGGPlot
        ];
      };
    };
  };
}
