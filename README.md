
You can use this DOI to cite this repository:

[![DOI](https://zenodo.org/badge/744563809.svg)](https://zenodo.org/doi/10.5281/zenodo.11041585)

Replication materials for study comparing resale value of different vehicle powertrains. Paper:

> Roberson, Laura A., Pantha, S., & Helveston, J.P. (2024) “Battery-Powered Bargains? Assessing Electric Vehicle Resale Value in the United States” _Environmental Research Letters_. DOI: [10.1088/1748-9326/ad3fce](https://doi.org/10.1088/1748-9326/ad3fce)

All code to replicate results are in the "code" folder.

Replication steps:

1. Install R and RStudio.
2. Open the pev-resale-2024.Rproj file to open RStudio.
3. Open 0-globals.R and set `PATH_DB` to the location of the listings database.
4. Run code scripts 1 through 4.

To perfectly replicate the same results as in our study, you'll need the full database, but this cannot be made public under the license agreement to obtain the data.

Instead, a sample of 1% of the database is included in `/data/db.parquet`, and `PATH_DB` in `0-globals.R` is set to this by default. The models will not produce the same exact results, but they should be relatively close. The only models that may differ strongly are the models with BEVs and PHEVs since the random sample provided may be too small compared to the full database. The purpose of including this sample is not to be able to perfectly replicate the study results (as this cannot be done without the original data, which again cannot be make publicly available) but rather to make transparent the code used to generate our results for review purposes.

The relevant variables in the full original database can be shared on an individual bases for review purposes only to reproduce the study results. Should this be needed, please contact the study corresponding author at jph (at) gwu (dot) edu.
