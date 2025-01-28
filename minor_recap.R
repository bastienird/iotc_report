require(readr)
require(futile.logger)
require(reshape2)
library(data.table)
library(lubridate)
require(here)
require(dplyr)
require(ggplot2)
require(cowplot)
require(RColorBrewer)
require(flextable)
require(tidyr)
require(stringr)
require(DT)
base::options(knitr.duplicate.label = "allow")
library(dplyr)

source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation/download_zenodo_csv.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Functions_markdown.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation/download_zenodo_csv.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/time_coverage_analysis.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Groupping_differences.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/compare_strata_differences.R")

if (!file.exists(here::here("data/global_nominal_catch_firms_level0_harmonized.csv"))) {
  # Télécharger le fichier si nécessaire
  nominal_init <- download_zenodo_csv("10.5281/zenodo.11410529", "global_nominal_catch_firms_level0_harmonized.csv")
  readr::write_csv(nominal_init, here::here("data/global_nominal_catch_firms_level0_harmonized.csv"))
  
} else {
  nominal_init <- readr::read_csv(here::here("data/global_nominal_catch_firms_level0_harmonized.csv"))
}

if (!file.exists(here::here("data/global_catch_firms_level0_harmonized.csv"))) {
  # Télécharger le fichier si nécessaire
  level0 <- download_zenodo_csv("10.5281/zenodo.11460074", "global_catch_firms_level0_harmonized.csv")
  readr::write_csv(level0, here::here("data/global_catch_firms_level0_harmonized.csv"))
} else {
  level0 <- readr::read_csv(here::here("data/global_catch_firms_level0_harmonized.csv"))
}

data_trfmo <- level0 %>% dplyr::filter(source_authority == "ICCAT")
# Grouper et filtrer les données
data_trfmo_groupped <- data_trfmo %>%
  dplyr::group_by(time_start, fishing_fleet, species, gear_type) %>% 
  # on regarde pour chaque strate l'emrepeinte spatiale des tonnes et des nombres
  dplyr::summarise(
    geo_t = list(unique(geographic_identifier[measurement_unit == "t"])),
    geo_no = list(unique(geographic_identifier[measurement_unit == "no"])),
    .groups = "drop"
  ) %>%
  # Supprimer les lignes où geo_t ou geo_no est NULL ou vide car ce sont des lignes ou il n'y a pas de doublon no/t
  dplyr::rowwise() %>%
  dplyr::filter(
    !(is.null(geo_t) || is.null(geo_no) || length(geo_t) == 0 || length(geo_no) == 0)
  ) %>%
  # on regarde les empreintes spatiales identiques ou incluses l'une dans l'autre
  dplyr::mutate(
    identical_groups = setequal(geo_t, geo_no),
    geo_t_in_geo_no = all(geo_t %in% geo_no),
    geo_no_in_geo_t = all(geo_no %in% geo_t)
  )%>%
  # dplyr::filter(!identical_groups) %>%
  ungroup() %>% 
  dplyr::distinct()

# View(data_trfmo_groupped)


diff_percent <- (nrow(data_trfmo_groupped %>% dplyr::filter(!identical_groups)) * 100) / nrow(data_trfmo_groupped)
diff_t_in_no <- (nrow(data_trfmo_groupped %>% dplyr::filter(!identical_groups) %>% dplyr::filter(geo_t_in_geo_no)) * 100) / nrow(data_trfmo_groupped %>% dplyr::filter(!identical_groups))
diff_no_in_t <- (nrow(data_trfmo_groupped %>% dplyr::filter(!identical_groups) %>% dplyr::filter(geo_no_in_geo_t)) * 100) / nrow(data_trfmo_groupped %>% dplyr::filter(!identical_groups))
partial_diff <- (nrow(data_trfmo_groupped %>% dplyr::filter(!identical_groups) %>% dplyr::filter(!geo_no_in_geo_t & !geo_t_in_geo_no)) * 100) / nrow(data_trfmo_groupped %>% dplyr::filter(!identical_groups))

# 7% des strates en tonnes et en nombre ont une empreinte spatiale différente
# 5 % de ces données différentes ont l'empreinte spatiale des tonnes qui est incluses dans celle des nombres (tonnes plus petites que nombre en empreinte) 
# 93 % de ces données différentes ont l'empreinte spatiale des nombres qui est incluses dans celle des tonnes 
# le reste 0.41 % ont des empreintes spatiales juste différente ou aucune n'est incluse complètement dans l'autre.


# Si on regarde juste les données des nombres avec une empreinte spatiale non incluses dans celle des tonnes, ça fait 7% de 7% donc environ 0.005% des données des strates doublées qui ont des nombres sup au tonnes.

# Possibilité, différence d'aggrégation 
# On refait pareil en aggrégant la donnée 1 deg en 5 deg. 
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/sardara_functions/transform_cwp_code_from_1deg_to_5deg.R")
one_degree <- data_trfmo %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "5")
five_degree <- data_trfmo %>% dplyr::filter(substr(geographic_identifier, 1, 1) == "6")
one_degree_aggregated <- one_degree %>% rowwise() %>% 
  dplyr::mutate(geographic_identifier = transform_cwp_code_from_1deg_to_5deg(geographic_identifier))

# df_input_not_aggregated <- georef_dataset %>% dplyr::filter(is.null(geographic_identifier))
# fwrite(df_input_not_aggregated, "data/df_input_not_aggregated.csv")

data_trfmo_agg <- as.data.frame(base::rbind(one_degree_aggregated, five_degree))

data_trfmo_groupped_agg <- data_trfmo_agg %>%
  dplyr::group_by(time_start, fishing_fleet, species, gear_type) %>% 
  # on regarde pour chaque strate l'emrepeinte spatiale des tonnes et des nombres
  dplyr::summarise(
    geo_t = list(unique(geographic_identifier[measurement_unit == "t"])),
    geo_no = list(unique(geographic_identifier[measurement_unit == "no"])),
    .groups = "drop"
  ) %>%
  # Supprimer les lignes où geo_t ou geo_no est NULL ou vide car ce sont des lignes ou il n'y a pas de doublon no/t
  dplyr::rowwise() %>%
  dplyr::filter(
    !(is.null(geo_t) || is.null(geo_no) || length(geo_t) == 0 || length(geo_no) == 0)
  ) %>%
  # on regarde les empreintes spatiales identiques ou incluses l'une dans l'autre
  dplyr::mutate(
    identical_groups = setequal(geo_t, geo_no),
    geo_t_in_geo_no = all(geo_t %in% geo_no),
    geo_no_in_geo_t = all(geo_no %in% geo_t)
  )%>%
  # dplyr::filter(!identical_groups) %>%
  ungroup() %>% 
  dplyr::distinct()

# View(data_trfmo_groupped)

diff_percent_agg <- (nrow(data_trfmo_groupped_agg %>% dplyr::filter(!identical_groups)) * 100) / nrow(data_trfmo_groupped_agg)
diff_t_in_no_agg <- (nrow(data_trfmo_groupped_agg %>% dplyr::filter(!identical_groups) %>% dplyr::filter(geo_t_in_geo_no)) * 100) / nrow(data_trfmo_groupped_agg %>% dplyr::filter(!identical_groups))
diff_no_in_t_agg <- (nrow(data_trfmo_groupped_agg %>% dplyr::filter(!identical_groups) %>% dplyr::filter(geo_no_in_geo_t)) * 100) / nrow(data_trfmo_groupped_agg %>% dplyr::filter(!identical_groups))
partial_diff_agg <- (nrow(data_trfmo_groupped_agg %>% dplyr::filter(!identical_groups) %>% dplyr::filter(!geo_no_in_geo_t & !geo_t_in_geo_no)) * 100) / nrow(data_trfmo_groupped_agg %>% dplyr::filter(!identical_groups))

(nrow(data_trfmo_groupped_agg%>%dplyr::filter(!identical_groups) ) * 100) /nrow(data_trfmo_groupped_agg)

# 6% des strates en tonnes et en nombre ont une empreinte spatiale différente


(nrow(data_trfmo_groupped_agg%>%dplyr::filter(!identical_groups)%>%dplyr::filter(geo_t_in_geo_no) ) * 100) /nrow(data_trfmo_groupped_agg%>%dplyr::filter(!identical_groups))
# 5.6 % de ces données différentes ont l'empreinte spatiale des tonnes qui est incluses dans celle des nombres (tonnes plus petites que nombre en empreinte) 

(nrow(data_trfmo_groupped_agg%>%dplyr::filter(!identical_groups)%>%dplyr::filter(geo_no_in_geo_t) ) * 100) /nrow(data_trfmo_groupped_agg%>%dplyr::filter(!identical_groups))
# 94% % de ces données différentes ont l'empreinte spatiale des nombres qui est incluses dans celle des tonnes 


(nrow(data_trfmo_groupped_agg%>%dplyr::filter(!identical_groups)%>%dplyr::filter(!geo_no_in_geo_t & !geo_t_in_geo_no) ) * 100) /nrow(data_trfmo_groupped_agg%>%dplyr::filter(!identical_groups))
# le rest 0.26 % ont des empreintes spatiales juste différente ou aucune n'est incluse complètement dans l'autre.


# Si on regarde juste les données des nombres avec une empreinte spatiale non incluses dans celle des tonnes, ça fait 6% de 6% donc environ 0.004% des données des strates doublées qui ont une empreinte spatiale des nombres sup à celle des tonnes.




# 
# 
# 
# ######### compare georef_nominal 
# 
# nominal <- nominal_init%>% dplyr::filter(source_authority == "data_trfmo")
# 
# compare_nominal_georef_corrected <- function(nominal, georef_mapped, list_strata = list(c("species", "year", "source_authority", "gear_type", "fishing_fleet"))) {
#   # Convertir les data.frames en data.tables
#   setDT(nominal)
#   setDT(georef_mapped)
#   
#   # Créer la colonne "year" à partir de time_start
#   georef_mapped[, year := as.character(year(ymd(time_start)))]
#   nominal[, year := as.character(year(ymd(time_start)))]
#   
#   # Conserver uniquement les données en tonnes
#   georef_mapped_tons <- georef_mapped[measurement_unit == "t"]
#   
#   # Initialise une liste pour stocker les résultats (un résultat pour chaque liste de dimensions à conserver pour faire la comparaison)
#   results <- list()
#   
#   for (strata in list_strata) {
#     # Nom pour la catégorie actuelle de strata
#     name <- paste0(toString(strata))
#     
#     # Agréger les données pour le nominal et georef sur les colonnes spécifiées dans 'strata' (ex groupper les données par années, espèces, engins, pavillon)
#     nominal_grouped <- nominal[, .(measurement_value_nominal = sum(measurement_value, na.rm = TRUE)), by = strata]
#     georef_mapped_grouped <- georef_mapped[, .(measurement_value_georef = sum(measurement_value, na.rm = TRUE)), by = strata]
#     georef_mapped_tons_grouped <- georef_mapped_tons[, .(measurement_value_georef_tons = sum(measurement_value, na.rm = TRUE)), by = strata]
#     
#     # # Retirer les valeurs des colonnes pour comparer uniquement les strates (si on veut garder que elles)
#     nominal_grouped_without_value <- nominal_grouped[, .SD, .SDcols = strata]
#     georef_grouped_without_value <- georef_mapped_grouped[, .SD, .SDcols = strata]
#     georef_tons_grouped_without_value <- georef_mapped_tons_grouped[, .SD, .SDcols = strata]
#     
#     
#     # # Assurer que les colonnes sont dans le même ordre pour la comparaison
#     setcolorder(georef_grouped_without_value, names(nominal_grouped_without_value))
#     setcolorder(georef_tons_grouped_without_value, names(nominal_grouped_without_value))
#     
#     # Trouver les strates présentes dans georef_mapped mais absentes de nominal
#     georef_no_nominal <- fsetdiff(georef_grouped_without_value, nominal_grouped_without_value, all = FALSE)
#     georef_no_nominal_with_value <- merge(georef_mapped_tons_grouped, georef_no_nominal, by = strata, all = FALSE)
#     sum_georef_no_nominal_tons <- sum(georef_no_nominal_with_value$measurement_value_georef_tons ,na.rm = TRUE)
#     
#     
#     # Comparer uniquement les données en tonnes
#     georef_tons_no_nominal <- fsetdiff(georef_tons_grouped_without_value, nominal_grouped_without_value, all = FALSE)
#     
#     # Comparer les valeurs des strates communes entre nominal et georef_mapped pour les données en tonnes
#     georef_sup_nominal <- merge(nominal_grouped, georef_mapped_tons_grouped, by = strata, all = FALSE)
#     
#     # Vérifier si les colonnes existent après le merge
#     if ("measurement_value_georef_tons" %in% names(georef_sup_nominal) && 
#         "measurement_value_nominal" %in% names(georef_sup_nominal)) {
#       georef_sup_nominal[, Difference := measurement_value_georef_tons - measurement_value_nominal]
#       georef_sup_nominal <- georef_sup_nominal[round(Difference, 3) > 1] # Supérieur strictement à 1, on s'affranchit des petits kouaks
#     } else {
#       georef_sup_nominal <- data.table()  # Retourne une table vide s'il n'y a pas de données
#     }
#     
#     if ("fishing_fleet" %in% colnames(georef_sup_nominal)){
#       tons_nei_georef <- georef_no_nominal_with_value[
#         fishing_fleet == "NEI" ,
#         sum(measurement_value_georef_tons)] + georef_sup_nominal[
#           fishing_fleet == "NEI" ,
#           sum(measurement_value_georef_tons) 
#         ]} else {
#           tons_nei_georef <- 0
#         }
#     
#     tons_aggregated_georef <- georef_no_nominal_with_value[
#       species %in% c("TUN", "TUS" ,"BIL"),
#       sum(measurement_value_georef_tons)
#     ] + georef_sup_nominal[
#       species %in% c("TUN", "TUS" ,"BIL"),
#       sum(measurement_value_georef_tons)
#     ]
#     
#     if ("fishing_fleet" %in% colnames(nominal_grouped)){
#       tons_nei_nominal <- nominal_grouped[
#         fishing_fleet == "NEI",
#         sum(measurement_value_nominal)
#       ]} else {tons_nei_nominal <- 0}
#     
#     
#     sum_georef_sup_nom <- sum(georef_sup_nominal$Difference, na.rm = TRUE)
#     
#     suffisant <- ifelse(sum_georef_no_nominal_tons + sum_georef_sup_nom -(tons_aggregated_georef + tons_nei_georef) > 0, FALSE, TRUE)
#     # Stocker les résultats
#     results[[name]] <- list(
#       georef_no_nominal = georef_no_nominal,           # Strates dans georef mais absentes dans nominal
#       georef_no_nominal_with_value = georef_no_nominal_with_value %>% dplyr::rename(measurement_value = measurement_value_georef_tons),           # Strates dans georef mais absentes dans nominal avec la valeur totale
#       georef_tons_no_nominal = georef_tons_no_nominal, # Strates en tonnes absentes dans nominal
#       georef_sup_nominal = georef_sup_nominal,          # Strates où georef est supérieur à nominal
#       tons_nei_nominal = tons_nei_nominal,          # Strates nei qui pourraient expliquer les différences
#       tons_nei_georef = tons_nei_georef,          # Strates nei qui pourraient expliquer les différences
#       sum_georef_no_nominal = sum_georef_no_nominal_tons, 
#       suffisant = suffisant, 
#       tons_aggregated_georef = tons_aggregated_georef,
#       sum_georef_sup_nom = sum_georef_sup_nom
#     )
#   }
#   
#   return(results)
# }
# 
# compare_nominal_georef <- compare_nominal_georef_corrected(nominal, data_trfmo,list(c("species", "year", "gear_type", "fishing_fleet"), c("species", "year", "fishing_fleet")))
# compare_nominal_georef$`species, year, gear_type, fishing_fleet`$georef_sup_nominal %>% dplyr::group_by(fishing_fleet) %>% dplyr::summarise(t = n()) %>% dplyr::arrange(desc(t))
# # C'est surtout pour LKA et TWN puis AUS mais on retrouve aussi EUFRA.
# 
# 
# 
# compare_nominal_georef$`species, year, fishing_fleet`$georef_sup_nominal %>% dplyr::group_by(fishing_fleet) %>% dplyr::summarise(t = n()) %>% dplyr::arrange(desc(t))
# # Si on enlève les gear_type il y en a moins et il y ap plus trop de EUFRA (youpi)
# 
# compare_nominal_georef$`species, year, gear_type, fishing_fleet`$suffisant
# 
# View(compare_nominal_georef$`species, year, fishing_fleet`$georef_sup_nominal)
