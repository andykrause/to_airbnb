# To Airbnb?
Code repository for the paper entitled 'To Airbnb: Factors impacting short-term leasing preference'

# Airbnb Revenues in Melbourne 

This project details research into the financial feasibility of short-term leasing (Airbnb) against more traditional long-term leasing strategies in Melbourne, Australia.  

### Replicability

This repository contains the code necessary to replication the analysis found in the paper **To Airbnb: Factors Influencing Short Term Preference**.  All data preparation, analysis, visualization and final paper production were done in the R statistical language.  

For a complete replication follow the general step below:

1. Clone the **to_airbnb** repository found at: www.github.com/andykrause/to_airbnb
2. Download the data from https://doi.org/10.7910/DVN/Q0VVTH into the /data folder where you downloaded the repository in #1. 
    *  Into a subfolder called */raw* download the following:
        *  ltr_data.csv
        *  str_daily_1.csv
        *  str_daily_2.csv
        *  str_prop_1.csv
        *  str_prop_2.csv
    *   Into a subfolder called */geographic* download and unzip the following:
        *  melbSuburbs.zip
        *  portPhillipBeach_saclip.zip
        *  sa1s.zip
3. Run the scripts in order
    * /scripts/1_str_preprocessing.rmd
    * /scripts/2_ltr_preprocessing.rmd
    * /scripts/3_data_prep.rmd
    * /scripts/4_data_analysis.rmd
    * /scripts/5_data_viz.rmd
4. Knit the final_paper.rmd file (most easily done use the RStudio (www.rstudio.com) IDE)
    * /papers/final_paper.rmd

If you'd like to only replicate the modeling and data visualizsation phases do the following:

1. Clone the **to_airbnb** repository found at: www.github.com/andykrause/to_airbnb
2. Download the prepared data from https://doi.org/10.7910/DVN/Q0VVTH into the /data folder where you downloaded the repository in #1. 
    *  Into a subfolder called */prepared* download the following:
        *  prepared_data.RData
    *   Into a subfolder called */analyzed* dowload the following:
        *  analyzed_data.Rdata
    *   Into a subfolder called */geographic* download and unzip the following:
        *  melbSuburbs.zip
        *  portPhillipBeach_saclip.zip
        *  sa1s.zip
3. Run the scripts in order
    * /scripts/4_data_analysis.rmd
    * /scripts/5_data_viz.rmd
4. Knit the final_paper.rmd file (most easily done use the RStudio (www.rstudio.com) IDE)
    * /papers/final_paper.rmd

*Please note that the scripts above use relative path locations that are most easily used by first opening the RStudio project file (to_airbnb.Rproj) in RStudio and then running any scripts from there. If you intend to run from base R or another IDE you will have to alter the relative paths in order to access data and custom functions.* 


