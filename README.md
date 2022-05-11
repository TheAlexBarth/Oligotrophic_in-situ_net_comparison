This is the data, code, and supplemental figures associated with the paper: Comparison of an in-situ imaging device and net-based method to study mesozooplankton communities in an oligotrophic system.

In this project, there are several directories which serve different purposes:

## R

The R folder holds all scripted used for analyses and plotting. Note that if you want to run some of this code yourself, I use the new native pipe `|>` in some of the code. This is the alternative to Magrittr's pipe `%\>%` but works effectively the same. You must have R 4.1 or greater installed for this to work. A lot of this code additionally requires my EcotaxaTools package. Which is under heavy development however, it is stable and any updates will be back-compatible. You can find the package [here](https://github.com/TheAlexBarth/EcotaxaTools). And a tutorial for it's use [here](https://thealexbarth.github.io/Ecotaxa_Tools_Tutorial/).

The R files have a naming convention which indicates what they do as the first word. Scripts also have an index which corresponds the other files they interact with. For example, mgmt_01 produces information which then is used in main_analysis_01. Some files are used across scripts but this provides a clear indication for the primary use of a script. For example, mgmt_03 products are used in main_analysis 3 and 4.

#### Import Scripts:

These files will not work. They import the data off the main computer which stored the original output. Raw UVP files can be found on Ecopart or by contacting Alex Barth (AB93\@email.sc.edu). Import scripts are not available for the MOCNESS since those were done in the command line then saved as .rds files (in data folder)

#### mgmt scripts:

These files take data from the data folder and format them for analyses. Products from mgmt scripts are stored in the data folder.

#### main_analysis scripts:

These produce figures and summary data for main analyses. All products from these scripts are stored in the Output folder. Note some of these do also produce supplement figures, specifically profiles (03, 06).

#### supp_analysis scripts:

Produces figures and summary data for supplemental figures. All products from these scripts are stored in output folder.

#### Tools:

These scripts are extra code for things that are too specific to be included in a package.

## Data

The data folder hosts .rds objects. These are r objects which can be loaded. Raw .tsv files of data are available on request (AB93\@email.sc.edu) or though export on Ecotaxa. Data are provided as .rds files because they already have some pre-filtered or cleaning attributes. Data are produced either from import scripts, mgmt scripts, or directly provided.

## Output

These are plots and summary data produced by analysis (either supp or main) scripts.

------------------------------------------------------------------------

Note that there are extra files in this directory depending on where it was accessed. The github repo will have all versions but I'm also using this readme for supplemental information zip file. So there are extra documents which I don't descript fully in this readme.
