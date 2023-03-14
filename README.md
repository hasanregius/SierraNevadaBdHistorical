# Sierra Nevada Historical Study of Bd Invasion
This repository holds all the code used in preparing the "[Pathogen invasion history elucidates contemporary host pathogen dynamics](https://journals.plos.org/plosone/article?id=10.1371%2Fjournal.pone.0219981)" publication by Vredenburg et al. (2019; PLOS One)


## The repository consists of:
- Scripts on pulling, preparing, cleaning up, and extracting data
  - Bioclimactic and anthropogenic data from rasters provided by [WorldClim](https://www.worldclim.org/data/bioclim.html) and [Venter et al. (2016)](https://www.nature.com/articles/sdata201667); respectively
  - Historical weather data from Oregon State University's [PRISM](https://prism.oregonstate.edu/) program
  - Hydrography data from U.S. Geological Survey's [National Hydrography Dataset (NHD)](https://www.usgs.gov/national-hydrography)
- Scripts on checking assumptions required for modeling and for performing both the MCMC Bayesian model of pathogen arrival and the logistic regression included in the paper
