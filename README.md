# READ-SSB-recdashboard
(Last updated October 29, 2026)

This repository is used to create the [RecDST data dashboard](https://connect.fisheries.noaa.gov/content/c257deee-a657-4c10-be8a-92827cb5bdfe/). 
The intention of the dashboard is to improve stakeholder understanding of the data used in the 
Recreational Fisheries Decision Support Tool (RecDST). The dashboard contains tables and visualizations of summary statistics and intermediate
model input data. The documentation section of the dashboard discusses data processing and methods. 

The dashboard does not contain every recreational fishing metric.  It focuses on the metrics most relevant
to understanding the RecDST. It currently contains historical data from MRIP for recreational trips and catch, data 
from stock assessments including numbers at age, and intermediate model inputs including catch per trip, 
catch at length, and directed trips. 

The dashboard is in development and does not currently have all metrics for all species (Western Gulf of Maine Cod, Gulf of Maine 
Haddock, and Mid-Atlantic Summer Flounder, Scup, and Black Sea Bass).

## Data
The historical trips and catch data are from the Marine Recreational Information Program 
([MRIP](https://www.fisheries.noaa.gov/insight/marine-recreational-information-program)). These data 
are processed by code in this repository in the stored_scripts folder. The code for processing the 
intermediate model input data and the numbers at age data are located in the respective 
[groundfishRDM](https://github.com/kimberly-bastille/groundfishRDM) and 
[flukeRDM](https://github.com/kimberly-bastille/flukeRDM) repositories. We use R Shiny 
to create and publish the dashboard app. 

## Contact Information
If you have questions or feedback, contact nefsc.recdst@noaa.gov. 

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic 
and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project 
code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against 
the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project 
will be governed by all applicable Federal law. Any reference to specific commercial products, processes, 
or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their 
endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and 
logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any 
commercial product or activity by DOC or the United States Government.
