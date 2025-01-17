# Conclusion on duplicated strata and georef sup to nom

This document is based on the precedent analysis of the repository. <https://github.com/bastienird/iotc_report>. Followed by discussions with IOTC some conclusion have been thrown.

**Author**: Bastien Grasset (IRD, MARBEC)\
**Co-Author**: Julien Barde (IRD, MARBEC)

**Affiliations**:\
- **IRD**: Institut de Recherche pour le Développement (IRD), av. Jean Monnet, CS 30171, 34203 Sète cédex, France\
- **MARBEC**: MARBEC, University of Montpellier, CNRS, Ifremer, IRD, Sète, France

## About data georeferenced exceeding nominal

Nominal data is the reference, for level 2 dataset a lowering of georeferenced data has to be made.

## About strata in georeferenced not appearing in nominal

As some are aggregation or resulting from a desaggregation in nominal and not in georeferenced, we will try to produce a desaggregated data for georeferenced data to match nominal.

## About strata with double units but spatial footprint not matching

The duplication of a strata is to take in account the geographic_identifier. Then the duplicated stratas will be removed and the not duplicated are kept and then converted. Following work to analysis this new treatment is to be done.

# Reproduce this work

This workflow can be reproduce running.

```{r}
rmarkdown::render_site()
```

For developper: To update the website https://bastienird.github.io/iotc_report move the result in docs/, with removing the data folder

## Minor recap

```{r}
rmarkdown::render("~/firms-gta/iotc_report/minor_recap.Rmd", envir = environment(), output_format = "bookdown::html_document2", output_dir = "docs")
```
