<img src="./man/figures/demerstem_court_transpa.png" alt="DEMERSTEM project" align="right">

# GPSMonitoring

This package was initially developped during DEMERSTEM EU project. It aims to develop tools for an easier use of GPS data for fisheries monitoring. The purpose was to have knwowledge on where the caught of fish occurs for a better stock managment and finally a more sustainable fisheries. The purpose was definitely not for control and surveillance.

## DEMERSTEM project

The EU DEMERSTEM project (http://pescao-demerstem.org) aims to improve knowledge on stocks in ECOWAS area. A sub working package of this project aims to study the use of GPS logger for a better knowledge on artisanal fisheries.

## Getting Started

Once the GPSMonitoring package is installed a set of data is available in order to better explain function and functionalities. 

## Dependencies

The packages use :
* adehabitatLT, for trajectory recalibrate on a given frequency
* shiny and leaflet for polygon Area Of Interest creation and Silico observation
* caret, ranger and randomForest package for RF models use
* dplyr, sf, ggplot, ggpubr .. for data manupulation and visualization

## Installing

```
devtools::install_github('polehalieutique/GPSMonitoring')
```

## Executing program

The package is composed of a set of functions described in the 2 vignettes (French and english version).
Several dataset are also included in the package : 

* GPSdataset : About 270 000 position related to several fishing trips of small scale boat in Guinea
* Observed_FO : Fishing operation tagged during a selection of trips. 
* emprise, fond, grid some spatial data used to filter or to use as context files when producing ggplot

The two first dataset have been uploaded and described here : https://doi.org/10.15454/VXEZEZ

## Help


## Authors

Jerome Guitton, 

UMR DECOD (Dynamique et Durabilité des Ecosystèmes), Institut Agro, IFREMER, INRAE Rennes, France

France


[@jeromeguitton1](https://twitter.com/jeromeguitton1)

Mohamed Soumah, 

Centre National des Sciences Halieutique de Boussoura 
Conakry
Guinée

Marie-Pierre Etienne, 

Institut Agro


## Version History

* 0.1.0
    * Initial Release

## License

This project is licensed under the GNU GPL License - see the LICENSE.md file for details

## Acknowledgments

Faustinato Behivoke IHSM, Madagascar<BR/>
Modou Thiaw, CRODT/ISRA Sénégal<BR/>
Marc Leopold, IRD<BR/>
Nicolas Bez; IRD<BR/>

