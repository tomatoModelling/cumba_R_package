<p align="center">
  <a href="https://github.com/tomatoModelling/cumba_R_package"><img src="https://github.com/tomatoModelling/cumba_R_package/blob/main/docs_/ImageCumba.png" alt="CUMBA'"></a>
</p>
<p align="center">
    <em>Carbon Use Model for yield and Brix Assessment.</em>
</a>
</p>

---

**GitHub**: <a href="https://github.com/tomatoModelling/cumba_R_package" target="_blank">github.com/tomatoModelling/cumba_R_package</a>


**Paper**: <a href="" target="_blank"></a>

---

# Cumbà Model 🍅:
CUMBA’ (Carbon Use Model for yield and Brix Assessment) is a simple process-based model developed to estimate the quantitative and qualitative aspects of tomato production as determined
by crop water requirement. 
It is a daily time step simulation model which computes the yield and brix degree of a tomato crop as a function of weather data and irrigation options.

---

## Table of Contents 

- [1. Input Data](#1-input-data)
- [2. Examples](#2-examples)
  - [2.1 Plotting](#21-plotting)
    - [2.1.1 Dynamic plots](#211-dynamic-plots)
    - [2.1.2 Scatter plots](#212-scatter-plots)
    - [2.1.3 Group comparison](#213-group-comparison)
    - [2.1.4 Plot saving](#214-plot-saving)
    - [2.1.5 Plot extracting](#215-plot-extracting)
  - [2.2 Statistics](#22-statistics)
    - [2.2.1 Dynamic plots](#221-simple-case)
    - [2.2.2 Several groups](#222-several-groups)
    - [2.2.3 Statistics plot](#223-statistics-plot)
  - [2.3 Data manipulation](#23-data-manipulation)
- [3. Tools](#3-tools)
  - [3.1 ggplotly](#31-ggplotly)
  - [3.2 patchwork](#32-patchwork)
- [4. Help](#4-help)
- [5. Citation](#5-Citation)

## 1. Input Data
Cumba Model needs of following input data: 
- `weather `: a dataframe with weather data which must have the following columns...
- `param `: a dataframe with model parameters values
- `estimateRad `: a boolean value to estimate solar radiation based on temperature using Hargreaves model. Default to 'true' (implying that the column Lat is present in weather df) if 'false' the 'weather' df must have 
  the Rad column
- `estimateET0 `:  a boolean value to estimate reference evapotranspiration based on temperature using Hargreaves model. Default to 'true'
- `deficitIrrigation `: a boolean value to estimate irrigation requirements. Default to 'false', implying that the irrigation_df is provided.
- `waterStressLevel `: a float corresponding to the threshold of water stress to trigger automatic irrigation. Default to .5, it is needed only if deficitIrrigation is 'true'.
- `minimumTurn `: an integer corresponding to the minimum number of days elapsed from the previous irrigation event. Default to 4, it is needed only if deficitIrrigation is 'true'.
- `irrigation_df `: a dataframe containing the irrigation scheduling for each experiment defined in the weather dataframe.








## 5. Citation
If you use this work, please consider citing the following paper:
