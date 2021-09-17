library(tidyverse)
library(readxl)

unzip("inst/data-raw/Base_de_datos_INFOgob (2).zip", exdir = "inst/data-raw")

congresal_pattern <- "(CONGRESAL)|(CONSTITUYENTE)|(DIPUTADOS)"

files <- list.files("inst/data-raw", recursive = TRUE, full.names = TRUE) %>% .[str_detect(., congresal_pattern)]
filenames <- list.files("inst/data-raw", recursive = TRUE) %>%
  tools::file_path_sans_ext() %>%
  .[str_detect(., congresal_pattern)] %>%
  str_remove(".*/")

listado_informacion <- tibble(files = files,
                              filenames = filenames) %>%
  separate(filenames, into = c("eleccion", "tipo_cargo"), extra = "merge") %>%
  separate(tipo_cargo, into = c("tipo_dato", "tipo_eleccion")) %>%
  arrange(tipo_dato, eleccion)

padrones <- listado_informacion %>%
  filter(tipo_dato == "Padron") %>%
  mutate(data = map(files, read_excel) %>% map(clean_names)) %>%
  select(eleccion, data) %>%
  unnest(data)

autoridades <- listado_informacion %>%
  filter(tipo_dato == "Autoridades") %>%
  mutate(data = map(files, read_excel) %>% map(clean_names)) %>%
  select(eleccion, data) %>%
  unnest(data)

candidatos <- listado_informacion %>%
  filter(tipo_dato == "Candidatos") %>%
  mutate(data = map(files, read_excel) %>% map(clean_names)) %>%
  select(eleccion, data) %>%
  unnest(data)

# TODO: ISP congresal
