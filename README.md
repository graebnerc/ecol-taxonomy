# Code to replicate an ecological taxonomy for the EU

The code to recreate the results is actually only the script `R/clustering.R`.
The creation of a report is `quarto/CountryTaxonomy.qmd`.

All remaining scripts are only to re-create the data used for the clustering.
But by default, the data that is used as a vantage point 
(`data/tidy/full_taxonomy_data.csv`) is already created.

Nevertheless, to recreate this data, you can use:

* The code in the folder `sql` to recreate `patstat_green-patents.csv` (mainly `get_green_patents.sql`, the rest is for testing).
* The script `R/country_classification.R` is only a helper for the classifications used
* The script `R/get_data.R` downloads the relevant data to create `data/tidy/full_taxonomy_data.csv`
