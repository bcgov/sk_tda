<!-- Add a project state badge
See https://github.com/BCDevExchange/Our-Project-Docs/blob/master/discussion/projectstates.md
If you have bcgovr installed and you use RStudio, click the 'Insert BCDevex Badge' Addin. -->

sk\_tda : Skeena Region FLNRORD Telemetry Data Analysis
=======================================================

### Usage

This repository contains a variety of scripts to analyse radio collared telemetry data. These include data preparation, home range estimations (MCP/Kernel Density), general data summaries and data visualisation/animaltion.

There are four core scripts that are required for the analysis, they need to be run in order:

Kernel Density Estimates
------------------------

-   01\_KDE\_Caribou.R - script to run KDEs
-   01\_KDE\_Caribou\_detail.R - script to test bandwidth (h parameter)
-   00\_KDE\_analysis\_workflow.Rmd - script to report on KDE methods
-   00\_KDE\_analysis\_workflow.html - output html document to describe methods

Minimum Convex Polygon
----------------------

-   01\_MCP\_Kernal.R

Mortality Summaaries
--------------------

- 03\_mort\_summary.R

Wolf RSF/HR analysis (in draft)
-------------------------------

-   04.0\_Wolf\_MCP\_Kernel.R - builds minimum convext polys and basic hrefkernel density estimates
-   04.1\_Wolf\_AvailablePts.R - draft script to extract random points to be used for RSF analysis
-   04.2\_Wolf\_sp\_layer.R - draft sscript to compile spatial layers for RSF
-   04.3\_Wolf\_Logistic\_regression.R - draft logistic regression for RSF
-   04.4\_Wolf\_animation.R - html animated graphics to show animal movement! (this is pretty cool!)

A more complete RSF scripting can also be found here <https://github.com/bcgov/clus/blob/master/R/caribou_habitat/04_caribou_habitat_model_telemetry_data_prep_doc.Rmd> compiled by Muhly, T.

### Project Status

This project is currently in development

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/sk_tda/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

[![Creative Commons License](https://i.creativecommons.org/l/by/4.0/88x31.png)](http://creativecommons.org/licenses/by/4.0/)

    Copyright 2019 Province of British Columbia

    This work is licensed under the Creative Commons Attribution 4.0 International License.
    To view a copy of this license, visit http://creativecommons.org/licenses/by/4.0/.

------------------------------------------------------------------------

*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.*
