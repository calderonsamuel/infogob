library(tidyverse)
library(readxl)
library(janitor)

unzip("inst/data-raw/Base_de_datos_INFOgob.zip", exdir = "inst/data-raw")

# congresal_pattern <- "(CONGRESAL)|(CONSTITUYENTE)|(DIPUTADOS)"

files <- list.files("inst/data-raw", recursive = TRUE, full.names = TRUE) %>% .[str_detect(., "REGIONAL")]
filenames <- list.files("inst/data-raw", recursive = TRUE) %>%
  tools::file_path_sans_ext() %>%
  .[str_detect(., "REGIONAL")] %>%
  str_remove(".*/")

listado_informacion <- tibble(files = files,
                              filenames = filenames) %>%
  separate(filenames, into = c("eleccion", "tipo_cargo"), extra = "merge") %>%
  mutate(ballotage = str_starts(tipo_cargo, "2V"),
         tipo_cargo = str_remove(tipo_cargo, "^2V_")) %>%
  separate(tipo_cargo, into = c("tipo_dato", "tipo_eleccion")) %>%
  arrange(tipo_dato, eleccion)

# ----
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
