# Subdirectory: Figures_Tables

## Folder description

### AIC_df_Q1 through Q4 txt files

- These text files contain AIC scores for best models, and those <2 AIC away from best models (i.e., basically equivalent) associated with questions 1-4.


### Data_transformations.xlsx

- This Excel file contains data transformations and distribution families for main models in analysis.


### Meteorology

- This folder contains plots conveying weather-related patterns of study area (i.e., precipitation and temperature) generated from Meteorology/Meteorology.rmd script.


### Q1_Q2_ANOVA

- Q1_Dist_ANOVA.png: Image showing ANOVA table from Question 1 where urbanization was quantified using distance from the urban center.

- Q1_Urbscore_ANOVA.png: Image showing ANOVA table from Question 1 where urbanization was quantified using urbanization score.

The same notation style is used for the latter two files (Q2_Dist_ANOVA and Q2_Urbscore_ANOVA).

- The Supplement folder contains models produced using population-level mean data (lm folder) and flower-level or plant-level data (lmer folder).

  - Within the lmer folder:
  
    - Alternative_Models contains ANOVA tables from Questions 1 and 2 wherein the models were statistically equivalent to the best models (i.e., <2 AIC away from best models). Alt_models_Height contains alternative models for the height analyses (which are not discussed in main text).
    
    - Best_models_Height contains best models for the height analyses (which are not discussed in main text).
    
  - Within the lm folder, files are named and organized in the same fashion as the lmer folder.

### Q3_ANOVA & Q4_ANOVA

- Contains ANOVA tables for Questions 3-4 as described in section above (Q1_Q2_ANOVA).


### Q1_Regressions_Plants

- Contains regression figures of main variables where urbanization was quantified using distance from the urban center (DISTGradient...) and urbanization score (URBSCOREGradient...) as PDFs and PNGs.

- Figures_with_data_points_shown folder shows same regressions, without data points displayed.


### Q2_Regressions_Plants

- Contains regression figures of main variables where urbanization was quantified using distance from the urban center (DISTGradient...) and urbanization score (URBSCOREGradient...) as PDFs and PNGs.

- legend_with_points folder contains files used to create legend, with points included in the legend.

- Q2_legend_points_revised files (png and pptx) are files used to create legend for distance from urban center models.

- Q2_legend_nopoints is image used as legend for urbanization score models.



### Q3_Regressions_Pollinators

- 3 tables summarizing pollinator diversity ("Q3_...Table.csv")

- Regression figure of main variables where urbanization was quantified using distance from the urban center (Pollinator_regressions_NOPOINTS pdf and png)

- Supplement folder contains regressions of main variables where urbanization was quantified using urbanization score ("Pollinator_regression_urbscore...")


### Q4_Regressions_Pollinators

- Contains regression figures of main variables where urbanization was quantified using distance from the urban center (Q4_Dist_urban_ANOVA.png) and urbanization score (Q4_Usc_urban_ANOVA.png).

- Contains table summarizing pollinator diversity for Question 4 (Q4_Pollinators_Table.csv) as well as both Questions 3-4 (Q3and4_Pollinators_Table.csv).

- Supplement folder contains

  - regressions of pollinator evenness ("Pollinator_regressions_Evenness_transects.pdf")
  
  - regressions of all variables where urbanization was quantified using urbanization score ("Pollinator_regressions_urbscore...") as pdf and png files.


### SampleSiteMaps

- 3 maps showing sampling sites:
1. Basemap = satellite ("SampleSiteMap_satellite_color.pdf")
2. Basemap = terrain, symbol colors highlight urbanization score ("SampleSiteMap_terrain_color_urbscore...") as pdf and png
3. Basemap = terrain, symbol colors highlight subtransects ("SampleSiteMap_terrain_color.pdf")