# DRPPM-Pre-Rank-GSEA

# Introduction

# Installation

## Via Download

1. Download the [Zip File](https://github.com/shawlab-moffitt/DRPPM-Pre-Rank-GSEA/archive/refs/heads/main.zip) from this GitHub repository: https://github.com/shawlab-moffitt/DRPPM-Pre-Rank-GSEA
2. Unzip the downloaded file into the folder of your choice.
4. Set your working directory in R to the local version of the repository
   * This can be done through the "More" settings in the bottom-right box in R Stuido
   * You may also use the `setwd()` function in R Console.

## Via Git Clone

1. Clone the [GitHub Repository](https://github.com/shawlab-moffitt/DRPPM-Pre-Rank-GSEA.git) into the destination of your choice.
   * Can be done in R Studio Terminal or a terminal of your choice
```bash
git clone https://github.com/shawlab-moffitt/DRPPM-Pre-Rank-GSEA.git
```
3. Set your working directory in R to the cloned repository
   * This can be done through the "More" settings in the bottom-right box in R Stuido
   * You may also use the `setwd()` function in R Console.

# Requirments

* `R` - https://cran.r-project.org/src/base/R-4/
* `R Studio` - https://www.rstudio.com/products/rstudio/download/

# R Dependencies

|  |  |  |  |  |
| --- | --- | --- | --- | --- |
| shiny_1.7.1 | shinythemes_1.2.0 | shinyjqui_0.4.1 | shinycssloaders_1.0.0 | enrichplot_1.12.3 |
| DT_0.23 | ggplot2_3.3.6 | readr_2.1.2 | dplyr_1.0.9 | clusterProfiler_4.0.5 |


# Required Files

* **Comprehensive Gene Set File:**
  * This is a provided file that contains various different gene sets to run the pre-ranked GSEA on
    * MSigDB, LINCS L1000, and Cell Marker.
    * The app may also take user uploaded gene set files

* **User Provided Pre Ranked Gene List:**
  * In the app, the user must upload a list of gene symbols, which can be formatted in different ways.
  * Tab-delimited txt or tsv file
    * Preferred Format:
      * A two column file of gene symbols with their corresponding hazard ratio is preferred. This can be obtained from the output of the [DRPPM-PATH-SURVIOER-Pipeline](https://github.com/shawlab-moffitt/DRPPM-PATH-SURVIOER-Pipeline), just subset the gene symbol and hazrd ratio column to their own file.
    * Other Formats
      * A single column of genes. The app will assume these are in ranked order
      * A table of more than two columns, the first of which being gene symbols. The app will assume these are in ranked order

# App Set-Up

* It is important to ensure that the comprehensive gene set file that is provided is in the propper location for the app to locate it when running.
* If the [Installation Section](https://github.com/shawlab-moffitt/DRPPM-Pre-Rank-GSEA#installation) is followed properly there should be no issue.

# App Features

# Quesions and Comments

Please email Alyssa Obermayer at alyssa.obermayer@moffitt.org if you have any further comments or questions.
