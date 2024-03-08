# No-evidence-of-impacts-from-a-mining-rush-in-a-protected-forest
This repository contains the input data and code used in the study entitled 'No evidence of impacts from a mining rush in a protected forest'. 2024. Authors: Katie Devenish, Simon Willcock, Kathryn M. Goodenough, Rio Heriniaina, O. Sarobidy Rakotonarivo, Julia P.G. Jones.  

This code and data constitute the methodology and produce the results presented in the above paper.

The analysis involves calculating the annual deforestation and degradation rate within all Level 9 drainage basins in the study area (the ex-province of Toamasina in Madagascar) in order to estimate the impact of a large artisanal mining rush for sapphires in the Bemainty drainage basin using the synthetic control method for impact evaluation.

Annual deforestation in hectares per drainage basin was calculated in ArcGIS and is already contained within the attributes of the basins layer (Mada_basins_Lev9_edited_final). 

Annual degradation data was obtained by adapting the raw Annual Disruptions data, a product of the Tropical Moist Forests dataset (Vancutsem et al, 2021), in Google Earth Engine. The output of this process (Annual_Disruptions_year_masked) was loaded into R and annual degradation calculated per drainage basin. Annual deforestation and degradation rates were calculated as a percentage of forest cover in each drainage basin at the start of each year. To obtain annual forest cover estimates for each sub-basin we reclassified the TMF Annual Change datasets and extracted forest area per sub-basin. Both of these processes were extremely computationally expensive. The code includes these steps, but also contains an option (Option 1) to skip these steps by loading csv files of annual forest cover (For_Area_ext2_21_04) and annual degradation per basin (Deg_Area_ext2_21_04). This data can then be joined back to the basins layer. This allows users to skip this computationally expensive stage and progress straight to the analysis.
The For90 layer (Vieilledent et al., 2018) and the Annual Disruptions data are only needed if following Option 2 in the code. 

Interview data (social_data_survey_full_anonymised_CORRECT) were collected by Rio Heriainina in Bemainty area in October - November 2019. 
