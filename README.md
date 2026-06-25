## Macroevolutionary-Drivers-of-Passerine-Diversity
This repository contains all data, workflows, scripts, and source code required to reproduce the analyses and results presented in the manuscript Macroevolutionary Drivers of Passerine Community Assembly in a Neotropical Peninsular System, currently under revision at the [*Journal of Biogeography*](https://onlinelibrary.wiley.com/journal/13652699).

![](cyucatanicus.jpg) \
***Cyanocorax yucatanicus*** is an endemic species from the Yucatán Peninsula. Photo from [enciclovida, CONABIO](https://enciclovida.mx/especies/35985).

**Background** \
Neotropical systems harbor an exceptionally high diversity of birds; however, the macroevolutionary processes shaping avian communities across most regions remain poorly understood, and the relative contributions of different processes are often difficult to disentangle. The theory of island biogeography predicts that species richness in geographically isolated communities is determined primarily by the balance between immigration and extinction [(MacArthur and Wilson, 1967)](https://www.jstor.org/stable/j.ctt19cc1t2). However, the classic equilibrium theory has been challenged by evidence suggesting that speciation and the geological dynamics of island systems can also play major roles in generating biodiversity. Here, we used [DAISIE](https://rsetienne.github.io/DAISIE/) model of island biogeography to infer the timing and mechanisms of passerine community assembly in the Yucatán Peninsula, Mexico.

**/data:** \
Contains CSV files with the estimated speciation and immigration times to the Yucatán Peninsula for each species. \
***py_tbl_26_final***: Base dataset used to estimate the parameters of the null, Shift-Rate, and Relaxed-Rate models. \
***py_tbl_26_dryforest.csv***: Base dataset used to estimate parameters for Two-Type model for dry-forest and non-dry forest species. \
***py_tbl_26_tyrannidae***: Base dataset used to estimate parameters for Two-Type model for Tyrannidae and non-Tyrannidae species. \
***daisie_tbl.RDATA***: Contains the R objects required to run the analyses in the DAISIE package.

**/bin:** \
Contain files to assembling and annotated cpDNA from row data sequences. \
***1_getdata_from_genbankR.R***: Runs a workflow to download all sequences from GeneBank using the accesion numbers. Create a folder by species ordered by families. \
***2_intodaisie_ars_final.R***:  Uses the [DAISIEprep](https://www.sciencedirect.com/science/article/pii/S1055790325000417?via%3Dihub) R package to extract immigration and speciation times from calibrated phylogenies. The procedures used to obtain the calibrated phylogenies are described in detail in the original manuscript. \
***3_daisie_tbl.R***: csv files are converted to the DAISIE format. \
***4_model_selection.R***: R script used to estimate AIC and BIC values for model selection across the different scenarios evaluated for each model. \
***5_model_simulation.R***: R script used to perform parametric bootstrap analyses using datasets simulated with the parameter estimates from the selected model.  \
***6_cross_validation_simulations.R***: R script used to perform from the Shift-Rate model to the Two-Type model and vice versa. \
***7_cross_validation.R***: R script used to perform the cross validation analyses from the simulated communities in the previous step.

**References** \
MacArthur RH, Wilson EO (1967) The theory of island biogeography. Princeton University Press. \
Valente, L, Phillimore, AB, Etienne, RS (2015) Equilibrium and non-equilibrium dynamics simultaneously operate in the Galápagos islands. Ecology Letters 18: 844-852. https://doi.org/10.1111/ele.12461. \
Lambert, J.W., Roeble, L., Pannetier T., Etienne, S. R., Valente, L. (2025). Using phylogenetic data for island biogeography analyses: The DAISIEprep package. 206 (108324) https://doi.org/10.1016/j.ympev.2025.108324
