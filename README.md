# Harmonizing dengue and climate data for Brazil

This repo tracks a reproducible workflow for downloading, processing, and harmonizing open-access data for dengue and climate in Brazil.


## Data sources and format

| type | source | original spatial resolution | original temporal resolution | harmonized spatial resolution | harmonized temporal resolution | link |
| ------ | ------ | ------ | ------ | ------ | ------ | ------ |
| dengue |  Datasus SINAN (Brazil MoH)  | municipality (admin2) |  monthly  | municipality (admin2) | monthly | https://datasus.saude.gov.br/transferencia-de-arquivos |
| temperature | ERA5-Land |  0.25º x 0.25º grid cells |  hourly  | municipality (admin2) | monthly | https://doi.org/10.24381/cds.e2161bac |
| precipitation | CHIRPS |  0.05º x 0.05º grid cells |  daily  | municipality (admin2) | monthly | https://www.chc.ucsb.edu/data/chirps |
| population |  Datasus SINAN (IBGE)  | municipality (admin2) |  yearly  | municipality (admin2) | monthly | https://datasus.saude.gov.br/transferencia-de-arquivos |

## Authors
Bruno M. Carvalho
brunomc.eco@gmail.com
