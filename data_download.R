#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")

#Download community data from OSF
get_file(node = "pk4bg",
         file = "BIO102_fluxes_raw.csv",
         remote_path = "C-Flux/BIO102")

get_file(node = "pk4bg",
         file = "Field_data_BIO102.csv",
         remote_path = "C-Flux/BIO102")
