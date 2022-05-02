README.md

This is the data, code, and supplemental figures associated with the paper: Comparison of an in-situ imaging device and net-based method to study mesozooplankton communities in an oligotrophic system.

In this project, there are several directories which serve different purposes:

### Data

The data folder hosts .rds objects. These are r objects which can be loaded. Raw .tsv files of data are available on request (AB93\@email.sc.edu) or though export on Ecotaxa. Data are provided as .rds files because they already have some pre-filtered or cleaning attributes.

Files include:

-   moc_ae1912_net-vols.rds - net volumes for June Cruise

-   moc_ae1912 - The original mocness ecotaxa-tsv file after multi-manager app from June Cruise

-   moc_ae1917_net-vols.rds - net volumes for July Cruise

-   moc_ae1917 - The original mocness ecotaxa-tsv file after multi-manager app from June Cruise

-   moc_all-processed - Mocness files for the

<!-- -->

-   uvp_ae1912 - UVP data from the June Cruise

-   uvp_ae1917 - UVP data from the July Cruise

### R

The R folder holds all scripted used for analyses and plotting. Note that if you want to run some of this code yourself, I use the new native pipe `|>` in some of the code. This is the alternative to Magrittr's pipe `%\>%` but works effectively the same. You must have R 4.1 or greater installed for this to work.
