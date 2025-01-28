
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE
)
base::options(knitr.duplicate.label = "allow")


source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation/download_zenodo_csv.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Functions_markdown.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/generation/download_zenodo_csv.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/time_coverage_analysis.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/Groupping_differences.R")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/Analysis_markdown/functions/compare_strata_differences.R")

formals(qflextable2)$pgwidth = 0.5

compare_nominal_georef_corrected <- function(nominal, georef_mapped, list_strata = list(c("species", "year", "source_authority", "gear_type", "fishing_fleet"))) {
  # Convertir les data.frames en data.tables
  setDT(nominal)
  setDT(georef_mapped)
  
  # Créer la colonne "year" à partir de time_start
  georef_mapped[, year := as.character(year(ymd(time_start)))]
  nominal[, year := as.character(year(ymd(time_start)))]
  
  # Conserver uniquement les données en tonnes
  georef_mapped_tons <- georef_mapped[measurement_unit == "t"]
  
  # Initialise une liste pour stocker les résultats (un résultat pour chaque liste de dimensions à conserver pour faire la comparaison)
  results <- list()
  
  for (strata in list_strata) {
    # Nom pour la catégorie actuelle de strata
    name <- paste0(toString(strata))
    
    # Agréger les données pour le nominal et georef sur les colonnes spécifiées dans 'strata' (ex groupper les données par années, espèces, engins, pavillon)
    nominal_grouped <- nominal[, .(measurement_value_nominal = sum(measurement_value, na.rm = TRUE)), by = strata]
    georef_mapped_grouped <- georef_mapped[, .(measurement_value_georef = sum(measurement_value, na.rm = TRUE)), by = strata]
    georef_mapped_tons_grouped <- georef_mapped_tons[, .(measurement_value_georef_tons = sum(measurement_value, na.rm = TRUE)), by = strata]
    
    # # Retirer les valeurs des colonnes pour comparer uniquement les strates (si on veut garder que elles)
    nominal_grouped_without_value <- nominal_grouped[, .SD, .SDcols = strata]
    georef_grouped_without_value <- georef_mapped_grouped[, .SD, .SDcols = strata]
    georef_tons_grouped_without_value <- georef_mapped_tons_grouped[, .SD, .SDcols = strata]

    
    # # Assurer que les colonnes sont dans le même ordre pour la comparaison
    setcolorder(georef_grouped_without_value, names(nominal_grouped_without_value))
    setcolorder(georef_tons_grouped_without_value, names(nominal_grouped_without_value))
    
    # Trouver les strates présentes dans georef_mapped mais absentes de nominal
    georef_no_nominal <- fsetdiff(georef_grouped_without_value, nominal_grouped_without_value, all = FALSE)
    georef_no_nominal_with_value <- merge(georef_mapped_tons_grouped, georef_no_nominal, by = strata, all = FALSE)
    sum_georef_no_nominal_tons <- sum(georef_no_nominal_with_value$measurement_value_georef_tons ,na.rm = TRUE)
    
    
    # Comparer uniquement les données en tonnes
    georef_tons_no_nominal <- fsetdiff(georef_tons_grouped_without_value, nominal_grouped_without_value, all = FALSE)
    
    # Comparer les valeurs des strates communes entre nominal et georef_mapped pour les données en tonnes
    georef_sup_nominal <- merge(nominal_grouped, georef_mapped_tons_grouped, by = strata, all = FALSE)

        # Vérifier si les colonnes existent après le merge
    if ("measurement_value_georef_tons" %in% names(georef_sup_nominal) && 
        "measurement_value_nominal" %in% names(georef_sup_nominal)) {
      georef_sup_nominal[, Difference := measurement_value_georef_tons - measurement_value_nominal]
      georef_sup_nominal <- georef_sup_nominal[round(Difference, 3) > 1] # Supérieur strictement à 1, on s'affranchit des petits kouaks
    } else {
      georef_sup_nominal <- data.table()  # Retourne une table vide s'il n'y a pas de données
    }
    
 if ("fishing_fleet" %in% colnames(georef_sup_nominal)){
    tons_nei_georef <- georef_no_nominal_with_value[
  fishing_fleet == "NEI" ,
  sum(measurement_value_georef_tons)] + georef_sup_nominal[
  fishing_fleet == "NEI" ,
  sum(measurement_value_georef_tons) 
    ]} else {
  tons_nei_georef <- 0
    }
    
    tons_aggregated_georef <- georef_no_nominal_with_value[
 species %in% c("TUN", "TUS" ,"BIL"),
  sum(measurement_value_georef_tons)
] + georef_sup_nominal[
 species %in% c("TUN", "TUS" ,"BIL"),
  sum(measurement_value_georef_tons)
]
    
    if ("fishing_fleet" %in% colnames(nominal_grouped)){
tons_nei_nominal <- nominal_grouped[
  fishing_fleet == "NEI",
  sum(measurement_value_nominal)
]} else {tons_nei_nominal <- 0}


sum_georef_sup_nom <- sum(georef_sup_nominal$Difference, na.rm = TRUE)

  suffisant <- ifelse(sum_georef_no_nominal_tons + sum_georef_sup_nom -(tons_aggregated_georef + tons_nei_georef) > 0, FALSE, TRUE)
    # Stocker les résultats
    results[[name]] <- list(
      georef_no_nominal = georef_no_nominal,           # Strates dans georef mais absentes dans nominal
      georef_no_nominal_with_value = georef_no_nominal_with_value %>% dplyr::rename(measurement_value = measurement_value_georef_tons),           # Strates dans georef mais absentes dans nominal avec la valeur totale
      georef_tons_no_nominal = georef_tons_no_nominal, # Strates en tonnes absentes dans nominal
      georef_sup_nominal = georef_sup_nominal,          # Strates où georef est supérieur à nominal
      tons_nei_nominal = tons_nei_nominal,          # Strates nei qui pourraient expliquer les différences
      tons_nei_georef = tons_nei_georef,          # Strates nei qui pourraient expliquer les différences
      sum_georef_no_nominal = sum_georef_no_nominal_tons, 
      suffisant = suffisant, 
      tons_aggregated_georef = tons_aggregated_georef,
      sum_georef_sup_nom = sum_georef_sup_nom
    )
  }
  
  return(results)
}

datatable2 <- function(df, options = list(scrollX = TRUE), ...) {
  df[] <- lapply(df, function(x) if(is.numeric(x)) round(x, 3) else x)
  # df <- df %>% head(100)
  # Combiner les options par défaut avec celles passées en argument
  options <- modifyList(list(scrollX = TRUE), options)
  
  # Appeler la fonction datatable originale
  DT::datatable(df, options = options, ...)
}



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

nominal <- nominal_init%>% dplyr::filter(source_authority == "ICCAT") %>% dplyr::filter(species %in% c("ALB", "BET", "YFT", "SKJ", "SWO", "TUN", "TUS", "BIL")) #%>% dplyr::select(-source_authority)
lvl0 <-level0 %>% dplyr::filter(source_authority == "ICCAT")%>% dplyr::filter(species %in% c("ALB", "BET", "YFT", "SKJ", "SWO", "TUN", "TUS", "BIL")) #%>% dplyr::select(-source_authority)


list_stratalist <- list(c("species", "year"), c("species", "year", "gear_type"),  c("species", "year", "gear_type", "fishing_fleet"), c("species", "year", "fishing_fleet"))

lvl0_strata <- compare_nominal_georef_corrected(nominal , lvl0, list_strata = list_stratalist)


qflextable2(head(lvl0_strata$`species, year`$georef_sup_nominal) %>% dplyr::select(species, year, measurement_value = measurement_value_georef_tons), captionn = "Aggrégation georef espèces/année (exemple illustratif)")

qflextable2(head(lvl0_strata$`species, year`$georef_sup_nominal) %>% dplyr::select(species, year, measurement_value = measurement_value_nominal), captionn = "Aggrégation nominale espèces/année (exemple illustratif)")



qflextable2(head(lvl0_strata$`species, year`$georef_sup_nominal), captionn = "Strates supérieures en georef que en nominales sur comparaison espèces/année")


lvl0_strata$`species, year`$sum_georef_no_nominal # 268 976.6
lvl0_strata$`species, year`$tons_aggregated_georef # 268 976.6
species_no_nom <- pie_chart_2_default("species", lvl0_strata$`species, year`$georef_no_nominal_with_value %>% dplyr::mutate(measurement_unit = "Tons"), titre_1 = "",title_yes_no =FALSE)

species_no_nom

qflextable2(lvl0_strata$`species, year`$georef_sup_nominal, captionn = "Strates supérieures en Georef que en nominales sur comparaison espèces/année")


lvl0_strata_all <- compare_nominal_georef_corrected(nominal , lvl0, list_strata = list(c("species", "gear_type", "fishing_mode", "year", "fishing_fleet")))

nombre_strates_sup_ou_no <- nrow(lvl0_strata_all$`species, gear_type, fishing_mode, year, fishing_fleet`$georef_no_nominal) + nrow(lvl0_strata_all$`species, gear_type, fishing_mode, year, fishing_fleet`$georef_sup_nominal)

tonnes <- sum(lvl0_strata_all$`species, gear_type, fishing_mode, year, fishing_fleet`$georef_no_nominal_with_value$measurement_value) + sum(lvl0_strata_all$`species, gear_type, fishing_mode, year, fishing_fleet`$georef_sup_nominal$Difference)

especes <- unique(lvl0_strata_all$`species, gear_type, fishing_mode, year, fishing_fleet`$georef_no_nominal$species)

lvl0_t <- lvl0 %>% dplyr::filter(measurement_unit == "t")
sum_lvl0 <- sum(lvl0_t$measurement_value)


georef_sup_nom_st_ff <- as.data.frame(lvl0_strata$`species, year, fishing_fleet`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_sup_nom_st_gear <- as.data.frame(lvl0_strata$`species, year, gear_type`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_no_nom_st_ff <- as.data.frame(lvl0_strata$`species, year, fishing_fleet`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t")  %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_no_nom_st_gear <- as.data.frame(lvl0_strata$`species, year, gear_type`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_sup_nom <- as.data.frame( lvl0_strata$`species, year`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_no_nom <- as.data.frame(lvl0_strata$`species, year`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t") %>%  dplyr::mutate(time_start = paste0(year , "-01-01"))


binding_st_ff <- rbind(georef_sup_nom_st_ff %>% dplyr::mutate(measurement_unit = "Georef_sup_nom") %>% dplyr::select(species, year, time_start,measurement_value, measurement_unit, fishing_fleet), georef_no_nom_st_ff%>% dplyr::mutate(measurement_unit = "Georef_no_nom"))

binding_st_gear <- rbind(georef_sup_nom_st_gear %>% dplyr::mutate(measurement_unit = "Georef_sup_nom") %>%  dplyr::select(species, year,time_start, measurement_value,measurement_unit, gear_type), georef_no_nom_st_gear%>% dplyr::mutate(measurement_unit = "Georef_no_nom"))

summary_of_differences_st_ff_vs_gear <- compute_summary_of_differences(binding_st_ff, binding_st_gear, "Dimensions year/species/fishing_fleet", "Dimensions year/species/gear_type") %>% dplyr::rename("Type of data" = measurement_unit)


qflextable2(summary_of_differences_st_ff_vs_gear, caption = "Differences de données georeferencées supérieures aux nominales et géoreferencées sans nominales pour différentes strates")


time_coverage_analysis_list_st_ff_vs_gear <- time_coverage_analysis(list(fonction_groupement("time_start", georef_sup_nom_st_ff, georef_sup_nom_st_gear )), "time_start", titre_1 = "georef_sup_nom_st_ff",titre_2 = "georef_sup_nom_st_gear")


time_coverage_analysis_list_st_ff_vs_gear$plots[[1]]


time_coverage_analysis_list_st_ff_vs_gear_no <- time_coverage_analysis(list(fonction_groupement("time_start", georef_no_nom_st_ff, georef_no_nom_st_gear %>% dplyr::mutate(time_start = paste0(year , "-01-01")) )), "time_start", titre_1 = "georef_no_nom_st_ff",titre_2 = "georef_no_nom_st_gear")


time_coverage_analysis_list_st_ff_vs_gear_no$plots[[1]]


datatable2(lvl0_strata$`species, year, fishing_fleet`$georef_no_nominal_with_value)


georef_no_nom_st_ff <- as.data.frame(lvl0_strata$`species, year, fishing_fleet`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(year = as.character(year))

species_no_nom_st_ff <- pie_chart_2_default("species", georef_no_nom_st_ff, titre_1 = "",title_yes_no =FALSE)

fishing_fleet_no_nom_st_ff <- pie_chart_2_default("fishing_fleet", georef_no_nom_st_ff, titre_1 = "",title_yes_no =FALSE)

sum_georef_no_nominal_st_ff <- lvl0_strata$`species, year, fishing_fleet`$sum_georef_no_nominal

sum_nei_nominal_gr <- lvl0_strata$`species, year, fishing_fleet`$tons_nei_nominal


render_subfigures(list(fishing_fleet_no_nom_st_ff, species_no_nom_st_ff), list("Codes espèces", "Codes pavillons"), general_title = "Proportion relative des captures géoreferencées sans strates correspondantes en nominales pour une comparaison sur des strates de dimensions espèces/années/pavillons")


datatable2((lvl0_strata$`species, year, fishing_fleet`$georef_sup_nominal))


time_coverage_analysis_list_st_ff <- time_coverage_analysis(list(fonction_groupement("time_start", georef_sup_nom_st_ff, georef_sup_nom )), "time_start", titre_1 = "Georef_sup_nominal_st_ff",titre_2 = "Georef_sup_nominal_year_species")
sum_georef_sup_nominal_st_ff <- lvl0_strata$`species, year, fishing_fleet`$sum_georef_sup_nom

sum_georef_sup_nominal <- lvl0_strata$`species, year`$sum_georef_sup_nom

# georef_sup_nom_andgeoref_no_nom_st_ff <- lvl0_strata$`species, year, fishing_fleet`$sum_georef_no_nominal + lvl0_strata$`species, year, fishing_fleet`$sum_georef_sup_nom # 587 284.4  + 4 209 992 = 4 797 276
# lvl0_strata$`species, year, fishing_fleet`$tons_aggregated_georef # = 268 977
# lvl0_strata$`species, year, fishing_fleet`$tons_nei_georef # 0 


binding_st_species_year <- rbind(georef_sup_nom %>% dplyr::mutate(measurement_unit = "Georef_sup_nom") %>%  dplyr::select(species, year,time_start, measurement_value,measurement_unit), georef_no_nom%>% dplyr::mutate(measurement_unit = "Georef_no_nom"))

summary_of_differences_st_ff_vs_rien <- compute_summary_of_differences(binding_st_ff, binding_st_species_year, "Dimensions year/species/fishing_fleet", "Dimensions year/species") %>% dplyr::rename("Type of data" = measurement_unit)


qflextable2(summary_of_differences_st_ff_vs_rien, caption = "Differences de données georeferencées supérieures aux nominales et géoreferencées sans nominales entre strate espèces/années/fishing_fleet et espèces années")


sum_no_sup_st_ff_gr <- lvl0_strata$`species, year, gear_type, fishing_fleet`$sum_georef_no_nominal +lvl0_strata$`species, year, gear_type, fishing_fleet`$sum_georef_sup_nom # 5 098 630

agg_georef_st_ff_gr <- lvl0_strata$`species, year, gear_type, fishing_fleet`$tons_aggregated_georef # = 268 977
nei_nom_st_ff_gr <- lvl0_strata$`species, year, gear_type, fishing_fleet`$tons_nei_georef # 203 111.5 donc l'aggregation et les nei ne permettent pas d'expliquer les sup to nom et les no nom (même en les sommant)


datatable2(lvl0_strata$`species, year, gear_type, fishing_fleet`$georef_sup_nominal)


datatable2(lvl0_strata$`species, year, gear_type, fishing_fleet`$georef_no_nominal_with_value)


georef_sup_nom_st_gear_ff <- as.data.frame(lvl0_strata$`species, year, gear_type, fishing_fleet`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_no_nom_st_gear_ff <- as.data.frame(lvl0_strata$`species, year, gear_type, fishing_fleet`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

species_no_nom_st_gear_ff <- pie_chart_2_default("species", georef_no_nom_st_gear_ff, second = georef_sup_nom_st_gear_ff, titre_1 = "Georef_no_nom (uniquement les tonnes)", titre_2 = "Georef_sup_nom")

gear_type_no_nom_st_gear_ff <- pie_chart_2_default("gear_type", georef_no_nom_st_gear_ff, second = georef_sup_nom_st_gear_ff, titre_1 = "Georef_no_nom (uniquement les tonnes)", titre_2 = "Georef_sup_nom")
fishing_fleet_no_nom_st_gear_ff <- pie_chart_2_default("fishing_fleet", georef_no_nom_st_gear_ff, second = georef_sup_nom_st_gear_ff, titre_1 = "Georef_no_nom (uniquement les tonnes)", titre_2 = "Georef_sup_nom")

time_coverage_analysis_list_st_gear_ff <- time_coverage_analysis(list(fonction_groupement("time_start", georef_no_nom_st_gear_ff, georef_sup_nom_st_gear_ff )), "time_start", titre_1 = "georef_no_nom (uniquement les tonnes)",titre_2 = "georef_sup_nom")


render_subfigures(plots_list = list(species_no_nom_st_gear_ff, gear_type_no_nom_st_gear_ff, fishing_fleet_no_nom_st_gear_ff) , titles_list =list(NULL, NULL, NULL), general_title = "Characteristics of data sup to nom and data no nom (Stratas species/year/gear/fishing_fleet)")


time_coverage_analysis_list_st_gear_ff$plots[[1]]


CA_RAISED_FILTERED_NO_FLEET <- read_csv("data/CA_RAISED_FILTERED_NO_FLEET_REDUCED.csv", guess_max = 0, 
                                        col_types = cols(AVG_WEIGHT = col_double()))%>%
  mutate(geographic_identifier = FISHING_GROUND_CODE,fishing_mode = CATCH_SCHOOL_TYPE_CODE,
         unit = "no", unit_target = "t", species = SPECIES_CODE, gear_type = GEAR_CODE,source_authority = "ICCAT",
         conversion_factor = AVG_WEIGHT/1000, 
         time_start = lubridate::as_date(paste0(YEAR,"-",MONTH_START, "-01 "))) %>%
  mutate(time_end =lubridate::ceiling_date(time_start, "month") - 1 ) %>%
  select(gear_type	,source_authority,	species	,geographic_identifier,	time_start,	time_end,	unit	,unit_target,	conversion_factor, fishing_mode) %>% mutate(measurement_value = conversion_factor) %>% 
  select(-conversion_factor) %>% dplyr::rename(measurement_unit = unit) %>% dplyr::select(-unit_target)


options("OutDec" = ".")
source("https://raw.githubusercontent.com/firms-gta/geoflow-tunaatlas/master/tunaatlas_scripts/pre-harmonization/map_codelists_no_DB.R")

mapping_codelist <-map_codelists_no_DB("catch", mapping_dataset = "https://raw.githubusercontent.com/fdiwg/fdi-mappings/main/global/firms/gta/codelist_mapping_rfmos_to_global.csv", 
                                       dataset_to_map = CA_RAISED_FILTERED_NO_FLEET, 
                                       mapping_keep_src_code = FALSE, summary_mapping = TRUE, source_authority_to_map = c("ICCAT")) 


IOTC_conv_fact_mapped <- mapping_codelist$dataset_mapped 
    
IOTC_conv_fact_mapped <- IOTC_conv_fact_mapped %>% 
          dplyr::mutate(gear_type = as.character(gear_type), time_start = as.character(time_start)) %>% 
          dplyr::mutate(year = as.character(lubridate::year(time_start))) %>% dplyr::rename(conversion_factors = measurement_value)  %>% dplyr::mutate(time_end = as.character(time_end))%>%  dplyr::mutate(geographic_identifier = as.character(geographic_identifier)) %>% dplyr::select(-measurement_unit) %>% dplyr::mutate(conversion_factors = round(conversion_factors, 6)) %>% dplyr::distinct()

IOTC_conv_fact_mapped_not_distinct <- IOTC_conv_fact_mapped %>% dplyr::group_by(across(-conversion_factors)) %>%
    dplyr::mutate(not_distinct = n_distinct(conversion_factors)) %>% dplyr::arrange(desc(not_distinct)) %>% dplyr::filter(not_distinct != 1)

IOTC_conv_fact_mapped <- IOTC_conv_fact_mapped %>% dplyr::group_by(across(-conversion_factors))%>% dplyr::summarise(conversion_factors = min(conversion_factors))


lvl0_conv <- lvl0 %>% 
          dplyr::mutate(time_start = as.character(time_start))%>% 
                                                             dplyr::mutate(time_end = as.character(time_end))%>% 
                                                             dplyr::mutate(geographic_identifier = as.character(geographic_identifier))

lvl0_tons <- lvl0_conv %>% dplyr::filter(measurement_unit == "t") 

# On convertit que les données en nombre. Donc on prends ces données et on cherche pour chaque strate 'time_start, engin, species, geographic_identifier' s'il y a un facteur de conversion. S'il y en a un, on multiplie la valeur en nombre par le facteur de conversion et on obtient une valeur en tonnes.

lvl0_number <- lvl0_conv  %>% dplyr::filter(measurement_unit == "no") %>% dplyr::inner_join(IOTC_conv_fact_mapped, by = c("source_authority", "time_start", "time_end", "geographic_identifier","gear_type", "species", "fishing_mode")) %>% 
          dplyr::mutate(measurement_value = measurement_value * conversion_factors) %>% 
          dplyr::mutate(measurement_unit = "t") %>% 
          dplyr::select(colnames(lvl0)) 

lvl0_upgraded <- rbind(lvl0_number, lvl0_tons)
        
list_stratalist <- list(c("species", "year") , c("species", "year", "gear_type"),  c("species", "year", "gear_type", "fishing_fleet"))

lvl0_strata_upgraded <- compare_nominal_georef_corrected(nominal , lvl0_upgraded, list_strata = list_stratalist)

        lvl0_conv_n_only <-lvl0 %>% 
          dplyr::mutate(time_start = as.character(time_start))%>% 
                                                             dplyr::mutate(time_end = as.character(time_end))%>% 
                                                             dplyr::mutate(geographic_identifier = as.character(geographic_identifier)) %>% 
          group_by(time_start,time_end, species, fishing_fleet, gear_type, source_authority, fishing_mode, geographic_identifier) %>%
            dplyr::mutate(numberunit = n_distinct(measurement_unit)) # on regarde les strates ayant deux unités différentes, on ne convertit que elles
        
        lvl0_onlynumber <- lvl0_conv_n_only %>% dplyr::filter(measurement_unit == "no" & numberunit ==1)%>% 
          dplyr::select(colnames(lvl0_conv_n_only))
        
        lvl0_onlytons_and_number_tons <- lvl0_conv_n_only %>% dplyr::filter(!(measurement_unit == "no" & numberunit ==1))%>% 
          dplyr::select(colnames(lvl0_conv_n_only)) %>% dplyr::filter(measurement_unit != "no")
        
        only_number_raised <- lvl0_onlynumber%>% dplyr::inner_join(IOTC_conv_fact_mapped, by = c("source_authority", "time_start", "time_end", "geographic_identifier","gear_type", "species", "fishing_mode")) %>% 
          dplyr::mutate(measurement_value = measurement_value * conversion_factors) %>% 
          dplyr::mutate(measurement_unit = "t") %>% 
          dplyr::select(colnames(lvl0_conv_n_only))
        
        lvl0_onlytons_and_number_tons_without_number <- lvl0_onlytons_and_number_tons %>%  dplyr::filter(measurement_unit != "no")
        
        lvl0_upgraded_st_only_no <- rbind(only_number_raised, lvl0_onlytons_and_number_tons_without_number)
        
        lvl0_upgraded_st_only_no <- lvl0_upgraded_st_only_no %>%
          dplyr::ungroup() %>%
          dplyr::group_by(across(-measurement_value)) %>% 
          dplyr::summarise(measurement_value = sum(measurement_value, na.rm = TRUE), .groups = 'drop')
        
        # list_stratalist <- list(c("species", "year")) #, c("species", "year", "gear_type"),  c("species", "year", "gear_type", "fishing_fleet")

lvl0_strata_upgraded_only_number <- compare_nominal_georef_corrected(nominal , lvl0_upgraded_st_only_no, list_strata = list_stratalist)


georef_sup_nom_conv <- as.data.frame(lvl0_strata_upgraded$`species, year`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_no_nom_conv_number_only <- as.data.frame(lvl0_strata_upgraded_only_number$`species, year`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(time_start = as.character(year))

georef_sup_nom_conv_number_only <- as.data.frame(lvl0_strata_upgraded_only_number$`species, year`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

species_no_nom_conv_number_only <- pie_chart_2_default("species", georef_sup_nom_conv, second = georef_sup_nom_conv_number_only, titre_1 = "georef_sup_nom_conv", titre_2 = "georef_sup_nom_conv_number_only")


species_no_nom_conv <- pie_chart_2_default("species", georef_sup_nom, second = georef_sup_nom_conv, titre_1 = "Georef_sup_nom", titre_2 = "Georef_sup_nom_conv")

time_coverage_analysis_list_conv_init <- time_coverage_analysis(list(fonction_groupement("time_start", georef_sup_nom, georef_sup_nom_conv )), "time_start", titre_1 = "Georef_sup_nom",titre_2 = "Georef_sup_nom_conv")


render_subfigures(plots_list = list(species_no_nom_conv, time_coverage_analysis_list_conv_init$plots[[1]]) , titles_list =list(NULL, NULL), general_title = "Characteristics of data sup to nom after and before conversion for strata species/year")

summary_of_differences <- compute_summary_of_differences(georef_sup_nom, georef_sup_nom_conv, "georef_sup_nom", "georef_sup_nom_conv")   

  groupping_differences_list <- groupping_differences(georef_sup_nom, georef_sup_nom_conv, c("time_start", "Difference"), "geographic_identifier", NULL)
  
  Groupped_all <- groupping_differences_list$Groupped_all

groupement_diff_espece <- fonction_groupement(these_col = "species",georef_sup_nom, georef_sup_nom_conv)
compare_strata_differences_list <- compare_strata_differences(georef_sup_nom, georef_sup_nom_conv, Groupped_all, "georef_sup_nom", "georef_sup_nom_conv", parameter_columns_to_keep = c("Precision", "measurement_unit", "Values dataset 1",
                                                                               "Values dataset 2", "Loss / Gain",
                                                                               "Difference (in %)", "Dimension",
                                                                               "Difference in value"))

# lvl0 %>% dplyr::group_by(measurement_unit) %>% dplyr::summarise(sum = sum(measurement_value)) # tonnes: 21 582 058

lvl0_strata_species <- compare_nominal_georef_corrected(nominal , lvl0_upgraded %>% dplyr::filter(measurement_unit == "t"), list_strata = c("species"))
qflextable2(lvl0_strata_species$species$georef_sup_nominal, captionn = "Total after conversion for each species and comparison to nominal (on corresponding years)")


georef_sup_nom_converted <- as.data.frame(lvl0_strata_upgraded$`species, year`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_no_nom_converted <- as.data.frame(lvl0_strata_upgraded$`species, year`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t")  %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

binding_converted <- rbind(georef_sup_nom_converted %>% dplyr::mutate(measurement_unit = "Georef_sup_nom") %>% dplyr::select(species, year, time_start,measurement_value, measurement_unit), georef_no_nom_converted%>% dplyr::mutate(measurement_unit = "Georef_no_nom"))

georef_sup_nom_converted_only_number <- as.data.frame(lvl0_strata_upgraded_only_number$`species, year`$georef_sup_nominal) %>% dplyr::mutate(measurement_unit = "t") %>% dplyr::mutate(measurement_value = Difference) %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

georef_no_nom_converted_only_number <- as.data.frame(lvl0_strata_upgraded_only_number$`species, year`$georef_no_nominal_with_value) %>% dplyr::mutate(measurement_unit = "t")  %>% dplyr::mutate(time_start = paste0(year , "-01-01"))

binding_converted_only_number <- rbind(georef_sup_nom_converted_only_number %>% dplyr::mutate(measurement_unit = "Georef_sup_nom") %>% dplyr::select(species, year, time_start,measurement_value, measurement_unit), georef_no_nom_converted_only_number%>% dplyr::mutate(measurement_unit = "Georef_no_nom"))

summary_of_differences_converted_vs_only_number <- compute_summary_of_differences(binding_converted, binding_converted_only_number, "Méthode A", "Méthode B") %>% dplyr::rename("Type of data" = measurement_unit)
qflextable2(summary_of_differences_converted_vs_only_number, caption = "Difference between two methods of raising")

species_no_nom_conv_number_only


time_coverage_analysis_list_conv <- time_coverage_analysis(list(fonction_groupement("time_start", georef_sup_nom_conv, georef_sup_nom_conv_number_only )), "time_start", titre_1 = "georef_sup_nom_conv",titre_2 = "georef_sup_nom_conv_number_only")


time_coverage_analysis_list_conv_only_number_init <- time_coverage_analysis(list(fonction_groupement("time_start", georef_sup_nom_conv_number_only, georef_sup_nom )), "time_start", titre_1 = "georef_sup_nom_conv_number_only",titre_2 = "georef_sup_nom")
# Create the plots (assuming your plots are ggplot objects)
plotA <- time_coverage_analysis_list_conv$plots[[1]]
plotB <- time_coverage_analysis_list_conv_only_number_init$plots[[1]]

# Arrange the plots side by side with their respective captions
grid.arrange(
  plotA, textGrob("Comparaison georef_sup_nom traitement A et traitement B (fig.A)", gp = gpar(fontsize = 12)),
  plotB, textGrob("Comparaison des valeurs georef_sup_nom entre le traitement B et les données initiales (fig.B)", gp = gpar(fontsize = 12)),
  ncol = 2, widths = c(1, 1)
)



calculate_raising_factors <- function(nominal_df, lvl0_conv_df, strata_cols) {
  
  lvl0_conv_only_double_unit <-lvl0_conv_df %>% 
        dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::group_by(across(all_of(strata_cols))) %>%
            dplyr::mutate(numberunit = n_distinct(measurement_unit)) %>% dplyr::rowwise()%>% dplyr::filter(numberunit != 1 | (numberunit == 2 &  measurement_unit == "no"))
  
  # Regrouper les données du dataframe nominal selon les strates et calculer la somme des valeurs
  nominal_groupped <- nominal_df %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::group_by(across(all_of(strata_cols))) %>%
    dplyr::summarise(measurement_value = sum(measurement_value), .groups = 'drop')
  
  # Regrouper les données du dataframe lvl0_conv selon les strates et les unités de mesure, puis pivoter les données
  lvl0_conv_groupped <- lvl0_conv_only_double_unit %>%
    dplyr::mutate(year = lubridate::year(time_start)) %>%
    dplyr::group_by(across(all_of(strata_cols)), measurement_unit) %>%
    dplyr::summarise(measurement_value = sum(measurement_value), .groups = 'drop') %>%
    tidyr::pivot_wider(names_from = measurement_unit, values_from = measurement_value)

    # Effectuer la jointure entre les deux jeux de données
  join_result <- dplyr::full_join(lvl0_conv_groupped, nominal_groupped, by = strata_cols) %>%
    dplyr::mutate(      t = dplyr::coalesce(t, 0), 
raising_factors = (measurement_value - t) / no) 
  
  return(join_result)
}

raisingfactorfromnumber_to_nominal_t_and_no <- calculate_raising_factors(nominal, lvl0,c("species", "year", "fishing_fleet", "gear_type"))%>% dplyr::mutate("Inf_1_kilo" = ifelse(raising_factors<0.001, TRUE, FALSE))  
raisingfactorfromnumber_to_nominal_t_and_no_groupped <- raisingfactorfromnumber_to_nominal_t_and_no%>% dplyr::filter(!is.na(Inf_1_kilo)) %>% dplyr::group_by(Inf_1_kilo) %>% dplyr::summarise(Nombre_de_strates = n()) %>% dplyr::mutate(Pourcentage = (Nombre_de_strates*100)/ sum(Nombre_de_strates))

raisingfactorfromnumber_to_nominal_t_and_no_10_kilos <- calculate_raising_factors(nominal, lvl0,c("species", "year", "fishing_fleet", "gear_type"))%>% dplyr::mutate("Inf_1_kilo" = ifelse(raising_factors<0.01, TRUE, FALSE))  
raisingfactorfromnumber_to_nominal_t_and_no_10_kilos_groupped <- raisingfactorfromnumber_to_nominal_t_and_no_10_kilos%>% dplyr::filter(!is.na(Inf_1_kilo)) %>% dplyr::group_by(Inf_1_kilo) %>% dplyr::summarise(Nombre_de_strates = n()) %>% dplyr::mutate(Pourcentage = (Nombre_de_strates*100)/ sum(Nombre_de_strates))
qflextable2(raisingfactorfromnumber_to_nominal_t_and_no_groupped, caption = paste0("Number of strata inferior/superior to 1 kilo for analysis ", toString(c("species", "year", "fishing_fleet", "gear_type"))))

raisingfactorfromnumber_to_nominal_t_and_no_ff <- calculate_raising_factors(nominal, lvl0,c("species", "year", "fishing_fleet"))%>% dplyr::mutate("Inf_1_kilo" = ifelse(raising_factors<0.001, TRUE, FALSE))  
raisingfactorfromnumber_to_nominal_t_and_no_ff_groupped <- raisingfactorfromnumber_to_nominal_t_and_no_ff%>% dplyr::filter(!is.na(Inf_1_kilo)) %>% dplyr::group_by(Inf_1_kilo) %>% dplyr::summarise(Nombre_de_strates = n()) %>% dplyr::mutate(Pourcentage = (Nombre_de_strates*100)/ sum(Nombre_de_strates))
qflextable2(raisingfactorfromnumber_to_nominal_t_and_no_ff_groupped, caption = paste0("Number of strata inferior/superior to 1 kilo for analysis", toString(c("species", "year", "fishing_fleet"))))


raisingfactorfromnumber_to_nominal_t_and_no_sp_year <- calculate_raising_factors(nominal, lvl0,c("species", "year"))%>% dplyr::mutate("Inf_1_kilo" = ifelse(raising_factors<0.001, TRUE, FALSE))  
raisingfactorfromnumber_to_nominal_t_and_no_sp_year_groupped <- raisingfactorfromnumber_to_nominal_t_and_no_sp_year%>% dplyr::filter(!is.na(Inf_1_kilo)) %>% dplyr::group_by(Inf_1_kilo) %>% dplyr::summarise(Nombre_de_strates = n()) %>% dplyr::mutate(Pourcentage = (Nombre_de_strates*100)/ sum(Nombre_de_strates))


qflextable(raisingfactorfromnumber_to_nominal_t_and_no_sp_year_groupped%>% dplyr::filter((Inf_1_kilo)))


qflextable2(raisingfactorfromnumber_to_nominal_t_and_no_10_kilos_groupped, caption = paste0("Number of strata inferior/superior to 10 kilos for analysis", toString(c("species", "year", "fishing_fleet", "gear_type"))))

compare_nominal_georef_corrected


df <- georef_sup_nom_st_gear_ff %>% dplyr::mutate_if(is.numeric, round)
colnames(df) <- c("Species", "Year", "Fleet", "Nominal_Value", 
                  "Georef_Tons", "Diff", "Unit", "Value", "Start_Time")

datatable2(df)


df <- georef_sup_nom_st_ff %>% dplyr::mutate_if(is.numeric, round)
colnames(df) <- c("Species", "Year", "Fleet", "Nominal_Value", 
                  "Georef_Tons", "Diff", "Unit", "Value", "Start_Time")

datatable2(df)


datatable2(lvl0_strata$`species, year, gear_type`$georef_no_nominal_with_value)


species_no_nom_st_gear <- pie_chart_2_default("species", georef_no_nom_st_gear, titre_1 = "",title_yes_no =FALSE)

gear_type_no_nom_st_gear <- pie_chart_2_default("gear_type", georef_no_nom_st_gear, titre_1 = "",title_yes_no =FALSE)

sum_georef_no_nominal_st_gear <- lvl0_strata$`species, year, gear_type`$sum_georef_no_nominal

sum_nei_nominal_gr <- lvl0_strata$`species, year, gear_type`$tons_nei_nominal


render_subfigures(list(gear_type_no_nom_st_gear, species_no_nom_st_gear), list("Codes espèces", "Codes engins"), general_title = "Proportion relative des captures géoreferencées sans strates correspondantes en nominales pour une comparaison sur des strates de dimensions espèces/années/engins")


datatable2((lvl0_strata$`species, year, gear_type`$georef_sup_nominal))


sum_georef_sup_nominal_st_gear <- lvl0_strata$`species, year, gear_type, fishing_fleet`$sum_georef_sup_nom
georef_sup_nom_andgeoref_no_nom_st_gear <- lvl0_strata$`species, year, gear_type`$sum_georef_no_nominal + lvl0_strata$`species, year, gear_type`$sum_georef_sup_nom # 587 284.4  + 4 209 992 = 4 797 276
# lvl0_strata$`species, year, gear_type`$tons_aggregated_georef # = 268 977
# lvl0_strata$`species, year, gear_type`$tons_nei_georef # 0 


georef_no_nom_sup_nom_st_gear_ff <- rbind(georef_sup_nom_st_gear_ff %>% dplyr::group_by(species, gear_type, measurement_unit, time_start) %>% dplyr::summarise(measurement_value = sum(measurement_value)),georef_no_nom_st_gear_ff %>% dplyr::select(species, time_start , gear_type, measurement_unit,  measurement_value))

georef_no_nom_sup_nom_st_gear <-rbind(georef_sup_nom_st_gear%>% dplyr::select(species, time_start, gear_type, measurement_unit, measurement_value), georef_no_nom_st_gear%>% dplyr::mutate(time_start = paste0(year , "-01-01")) %>% dplyr::select(species, time_start , gear_type, measurement_unit, measurement_value))


time_coverage_analysis_list_st_gear <- time_coverage_analysis(list(fonction_groupement("time_start", georef_no_nom_sup_nom_st_gear_ff , georef_no_nom_sup_nom_st_gear )), "time_start", titre_1 = "georef_sup_nom_st_gear_ff",titre_2 = "georef_sup_nom_st_gear") # code très compliqué parce qu'on peut pas juste comparer les strates georef sup nom, il faut absolument ajouter les georef_no_nominal quand on compare deux strates différentes parce qu'en fait les nominales ne vont pas correspondre si on regarde que les sup.


time_coverage_analysis_list_st_gear$plots[[1]]

