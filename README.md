# pipaux
Fetch auxiliary information and organizing data (substitute of master file)

## Datasets Available

* cpi:           Consumer Price Index
* ppp:           Purchasing Power Parity (ICP) 
* gdp:           Gross Domestic Product
* maddison:      Madisson Project Data
* sna:           Special National Account cases
* pfw:           Price Framework
* pce:           Private consumption
* country_list:  List of countries and metadata at country level


## INSTRUCTIONS.
1. Folder sna (Special National Account cases) should be modified manually. 
2. country_list.csv must be updated manually, but name should NOT be changed! vintage control is done internally in pip_country_list("update") in the _vintage folder using .fst files. 
