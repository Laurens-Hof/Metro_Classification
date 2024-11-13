#import & boilerplate------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(sf)
library(cbsodataR)
library(tmap)

#import municipal classifications
gemeentenamen = cbs_get_data("84929NED") %>%
  mutate(Code = str_trim(Code_1),
         Naam = str_trim(Naam_2)) %>%
  select(Code, Naam)
#import commuter data
woonwerk_import = cbs_get_data("85481NED")
#import municipal boundaries
gemeentegrenzen = st_read("https://service.pdok.nl/cbs/gebiedsindelingen/2024/wfs/v1_0?request=GetFeature&service=WFS&version=2.0.0&typeName=gemeente_gegeneraliseerd&outputFormat=json")

#data prepping-------------------------------------------------------------------

woonwerk = woonwerk_import %>%
  #rename and rescale where appropriate, then keep only necessary data
  mutate(woonregio = str_trim(WoonregioS),
         werkregio = str_trim(WerkregioS),
         banen = BanenVanWerknemers_1*1000) %>%
  filter(Perioden == "2022MM12") %>%
  select(woonregio, werkregio, banen) %>%
  #keep only municipalities
  filter((str_sub(woonregio, 1, 2) == "GM")) %>%
  filter((str_sub(werkregio, 1, 2) == "GM")|werkregio == "NL00") %>%
  #make a commute matrix
  pivot_wider(names_from = werkregio, values_from = banen) %>%
  rename(beroepsbevolking = NL00) %>%
  #calculate the fraction of labor force commuting to a municipality
  mutate(across(3:358, ~.x/beroepsbevolking)) %>%
  pivot_longer(3:358, names_to = "werkregio", values_to = "fraction") %>%
  #filter out uncategorizables
  filter(woonregio != "GM0000") %>%
  #filter out zero commutes (Vlieland & Schiermonnikoog)
  filter(fraction > 0) %>%
  #replace municipality codes by names. First woonregio
  left_join(gemeentenamen, join_by(woonregio == Code)) %>%
  mutate(woongemeente = Naam,
         woonregio = NULL,
         Naam = NULL) %>%
  left_join(gemeentenamen, join_by(werkregio == Code)) %>%
  mutate(werkgemeente = Naam,
         werkregio = NULL,
         Naam = NULL) %>%
  relocate(woongemeente, werkgemeente)


#Classification----------------------------------------------

#input: the number of metropolitan areas to be defined
topn = 8
#input: the percentage of labor force of mun. a that needs to be employed in mun. b
#for mun a to be considered part of metro area of mun b
threshold = 0.05

#initialize a table we'll be working on.
#also initialize a metro area flag, which will start out as labeling everything as independent
woonwerk_operate = woonwerk %>%
  mutate(metro_area = woongemeente)

#initialize checklist
used_names = vector(mode = "list", length = topn)

for(i in 1:topn){
  #calculate the biggest job center among independent municipalities that do not
  #already head a metro area
  work_rank_op = woonwerk_operate %>%
    mutate(banen = beroepsbevolking * fraction) %>%
    group_by(werkgemeente) %>%
    filter(!is.na(werkgemeente) &
            metro_area == woongemeente &
             !(werkgemeente %in% used_names)) %>%
    summarize(banen_totaal = sum(banen)) %>%
    arrange(desc(banen_totaal))
  
  
  #find the unassigned municipalities whose strongest commute flow is greater than the threshold
  #and destined for the current largest muni
  strongest_o = woonwerk_operate %>%
    filter(woongemeente != werkgemeente
           & metro_area == woongemeente) %>%
    group_by(woongemeente) %>%
    slice(which.max(fraction)) %>%
    filter(werkgemeente == work_rank_op$werkgemeente[1] & fraction >= threshold)
  
    woonwerk_operate = woonwerk_operate %>%
      mutate(metro_area = ifelse(woongemeente %in% strongest_o$woongemeente,
                                 work_rank_op$werkgemeente[1],
                                 metro_area))
    
    used_names[i] = work_rank_op$werkgemeente[1]
    print(paste(used_names))
  } 
 
#trim away unclassified munis
woonwerk_operate = woonwerk_operate %>%
  mutate(metro_area = case_when(woongemeente %in% used_names ~ woongemeente,
                                woongemeente == metro_area ~ "None",
                                .default = metro_area)) %>%
  filter( metro_area != "None")

#consolidate the list into a row per muni
classified_list = woonwerk_operate %>%
  group_by(woongemeente) %>%
  summarize(metro_area = first(metro_area))

#join the list onto the map
classified_map = gemeentegrenzen %>%
  left_join(classified_list, by = join_by(statnaam == woongemeente))

tm_shape(classified_map) +
  tm_polygons("metro_area")