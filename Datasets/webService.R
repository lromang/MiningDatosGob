#! /usr/bin/Rscript

#source("/home/luis/quickStats/MiningDatosGob/Datasets/ultimate.R")
source("/home/luis/Documents/Presidencia/MiningDatosGob/Datasets/ultimate.R")

filter_data <- dplyr::select(all_data, one_of("dep", "slug", "conj","rec","rec_des","rec_url"))
write.csv(filter_data, "./filter_data.csv", row.names = FALSE)
