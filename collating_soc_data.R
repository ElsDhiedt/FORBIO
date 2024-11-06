library(tidyverse)
library(readxl)


file_path <- "C:/Users/elsdhi/OneDrive - UKCEH/Documents/UGent/TreeDivNet/Bodemanalysen.xlsx"
sheet_name <- "Raw data"

#basal area data
file_paths_BA <- c("C:/Users/elsdhi/OneDrive - UKCEH/Documents/UGent/TreeDivNet/size_2019.xlsx")

#load data
data_raw <- read_excel(file_path, sheet = sheet_name)
BA_raw <- read_excel(file_paths_BA, sheet = "Data")

data <- data_raw %>%
  mutate(plot_number = gsub("Ze|Ge|He", "", Plot_ID),
         treatment = case_when(grepl("prov", composition_ID) & Site == "Zedelgem" ~ paste0(composition_ID, " oak"),
                               grepl("prov", composition_ID) & Site == "Gedinne" ~ paste0(composition_ID, " beech"),
                               plot_number == "0" ~ "natural regeneration"))

species_comp <- data %>%
  mutate(year_ID = paste0(Year, Plot_ID)) %>%
  pivot_longer(cols = c("beech", "oak", "birch", "lime", "pine", "larch", "maple", "Douglas"), names_to = "species", values_to = "presence") %>%
  mutate(latin = case_when(species == "beech" ~ "Fagus silvatica",
                           species == "birch" ~ "Betula pendula",
                           (species == "oak" & Site == "Zedelgem") ~ "Quercus robur",
                           (species == "oak" & Site %in% c("Gedinne", "Hechtel-Eksel")) ~ "Quercus petraea",
                           (species == "larch" & Site == "Gedinne") ~ "Larix x eurolepis",
                           (species == "larch" & Site == "Hechtel-Eksel") ~ "Larix kaempferi",
                           species == "maple" ~ "Acer pseudoplatanus",
                           species == "Douglas" ~ "Pseudotsuga menziesii",
                           species == "lime" ~ "Tilia cordata",
                           species == "pine" ~ "Pinus silvestris")) %>%
  filter(presence == 1) %>%
  group_by(year_ID) %>%
  nest() %>%
  mutate(species_composition_words = map_chr(data, ~paste0(.x$latin, collapse = " & ")))


data <- data %>%
  mutate(year_id = paste0(Year, Plot_ID)) %>%
  group_by(year_id) %>%
  mutate(species_composition_initial = species_comp$species_composition_words[match(year_id, species_comp$year_ID)],
         species_composition_observed = species_composition_initial,
         Shannon_initial = -sum(c(`#_beech`/`#_Tree_Total`*log(`#_beech`/`#_Tree_Total`),
                                 `#_oak`/`#_Tree_Total`*log(`#_oak`/`#_Tree_Total`),
                                 `#_birch`/`#_Tree_Total`*log(`#_birch`/`#_Tree_Total`),
                                 `#_lime`/`#_Tree_Total`*log(`#_lime`/`#_Tree_Total`),
                                 `#_pine`/`#_Tree_Total`*log(`#_pine`/`#_Tree_Total`),
                                 `#_larch`/`#_Tree_Total`*log(`#_larch`/`#_Tree_Total`),
                                 `#_maple`/`#_Tree_Total`*log(`#_maple`/`#_Tree_Total`),
                                 `#_Douglas`/`#_Tree_Total`*log(`#_Douglas`/`#_Tree_Total`)),
                               na.rm=T),
         evenness_initial = Shannon_initial / log10(sp_nr) )
  
#Basal area 2018

data_observed <- data %>% 
  merge(BA_raw[, c("Plot_ID", "Acer.G.relative", "Betula.G.relative", "Fagus.G.relative", "Larix.G.relative", "Pinus.G.relative", "Pseudotsuga.G.relative", "Quercus.G.relative", "Tilia.G.relative")], by = "Plot_ID") %>%
  group_by(year_id) %>%
  mutate(Shannon_observed = -sum(c(Acer.G.relative*log(Acer.G.relative),
                                 Betula.G.relative*log(Betula.G.relative),
                                 Fagus.G.relative*log(Fagus.G.relative),
                                 Larix.G.relative*log(Larix.G.relative),
                                 Pinus.G.relative*log(Pinus.G.relative),
                                 Pseudotsuga.G.relative*log(Pseudotsuga.G.relative),
                                 Quercus.G.relative*log(Quercus.G.relative),
                                 Tilia.G.relative*log(Tilia.G.relative)),
                                 na.rm=T),
         species_richness_observed = sp_nr,
         evenness_observed = Shannon_observed / log10(species_richness_observed)) %>%
  #update observed for old years, as the observed = initial
  mutate(Shannon_observed = case_when(Year < 2018 ~ Shannon_initial,
                                      .default = Shannon_observed),
         evenness_observed = case_when(Year < 2018 ~ evenness_initial,
                                       .default = evenness_observed),
         country = "Belgium",
         sampling_depth = 10)

data_init <- data_observed %>%
  filter(Year < 2018)

data_2018 <- data_observed %>%
  filter(Year >= 2018) %>%
  mutate(C_init = data_init$C[match(Plot_ID, data_init$Plot_ID)])

#select cols of interest

data_final <- data_2018 %>%
  mutate(pH_method = "water",
         sampling_date = NA,
         stand_age = NA,
         moisture = NA,
         C.N = C/N,
         bulk_density = NA,
         C_stock = NA,
         nitrate = NA,
         ammonium = NA,
         nutrient_methods = NA,
         biomass = NA,
         litter_quantity = NA,
         litter_CN = NA,
         groundcover_biomass = NA,
         comments = NA) %>%
  ungroup() %>%
  dplyr::select(Site, country, plot_number, Block_ID, treatment, sp_nr,
                species_composition_initial, Shannon_initial, evenness_initial,
                C_init, Year, sampling_date, stand_age, species_richness_observed,
                species_composition_observed, evenness_observed, sampling_depth,
                C, bulk_density, C_stock, pH_H2O, pH_method, moisture, N, C.N, nitrate, ammonium, Olsen_P, K,
                nutrient_methods, biomass, litter_quantity, litter_CN, groundcover_biomass,
                comments)

  
  
