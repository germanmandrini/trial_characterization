# Trial characterization using APSIM (Envirotyping)

# Objective

Repository for characterizing crop trials. It takes a csv with lat and long and plantings dates of several trials. Downloads weather and soil data and runs APSIM. Returns meaningful variables aggregated by crop stages
# Use

The characterization variables can be used to characterize the environment of the trials, find clusters of trials with similar conditions, use the variables as regressors to explain the trial results, extrapolate results to areas where trials were not implemented.

# About the code

The codes are enumerated in the order they need to be executed. Only the first four codes need to be run, and these four codes will call the other ones.

# Output variables

| **Variable** | **Description** |
| --- | --- |
| id\_trial | Trial identifier, based on the input.csv |
| Site | User site identifier |
| Planting | Planting date |
| Crop | soybean or maize |
| state | US state |
| region | US region |
| X | latitude |
| Y | longiture |
| yield\_sim | Yield obtained from APSIM. If the simulated yield is very different than the trial yield, the simulation should be discarded |
| whc | water-holding capacity of the soil at the location of the exact coordinates (SSURGO) |
| sand | sand % (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| clay | clay % (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| om | Organic Matter % (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| ph | Ph (0-20cm) of the soil at the location of the exact coordinates (SSURGO) |
| rain\_# | Rain (mm) in the indicated period |
| radn\_# | Mean radiation (MJ/m2/day) in the indicated period |
| MaxT\_# | Max temperature (Celsius) in the indicated period |
| MinT\_# | Min temperature (Celsius) in the indicated period |
| swdef\_expan\_# | APSIM water stress index (0 means stress, 1 means no stress) |
| period\_start\_doy\_# | Doy of the year that the period started |

# Crop periods

The variables are divided into periods (example: rain\_7 means rain during period 7). The periods are the following:

| **Period** | **Description** |
| --- | --- |
| 0 | fallow\_initial |
| 1 | veg\_early |
| 2 | veg\_late |
| 3 | flowering |
| 4 | grainf\_early |
| 5 | grainf\_late |
| 6 | fallow\_end |

# Contact

Questions about the code and methodology: German Mandrini, Dpt of Crop Sciences, University of Illinois at Urbana-Champaign, [germanmandrini@gmail.com](mailto:germanmandrini@gmail.com)

Questions about collaborations: Nicolas F Martin, Dpt of Crop Sciences, University of Illinois at Urbana-Champaign, [nfmartin@illinois.edu](mailto:nfmartin@illinois.edu)
