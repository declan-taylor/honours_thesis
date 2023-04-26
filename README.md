# honours_thesis
A respository for the code and data involved in Declan Taylor's 2023 ENSC honours thesis. 

##Directory overview

**/data** contains the raw data.

**/figures** contains .png images of the figures used in the manuscript and elsewhere for the project.

**/poster_materials** contains an assortment of scripts, images, and more used in the creating of the poster I presented in early April.

**/manuscript** contains the current version of my manuscript (*DeclanTaylor\_ENVR449\_ThesisManuscript.pdf* ), along with the first draft before revisions by my supervisor, Dr. Greg H. R. Henry.

###How I organized my code ( /scripts )
**/scripts/data_assembly/** contains all of the scripts used to transform myriad raw data into the master dataframes ("NEE", "ER", and "GEP") from which all of my analyses are done. Running *master_dataframe.R* assembles the dataframe from the raw data 

**/scripts/data_exploration** contains scripts that were used to explore the raw data but do not produce analyses or figures.

**/scripts/figure_scripts** contains the scripts used to create all figures present in the manuscript. 

**/scripts/analysis** contains a variety of scripts used in refreshing myself on stats and analyzing my data. All the models, as well as means calculations, are found in *masterStats.R*. The ANOVA statistics for environmental paramters are found in *envParamater\_stats.R*.
