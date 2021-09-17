library(tidyverse)
library(readxl)
library(janitor)

files <- list.files("inst/data-raw", pattern = "PRESIDENCIAL", recursive = TRUE, full.names = TRUE)
filenames <- list.files("inst/data-raw", pattern = "PRESIDENCIAL", recursive = TRUE) %>% tools::file_path_sans_ext() %>% str_remove(".*/")

listado_informacion <- tibble(files = files,
       filenames = filenames) %>%
  separate(filenames, into = c("eleccion", "tipo_cargo"), extra = "merge") %>%
  mutate(ballotage = str_starts(tipo_cargo, "2V"),
         tipo_cargo = str_remove(tipo_cargo, "^2V_")) %>%
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
  select(eleccion, ballotage, data) %>%
  unnest(data)

candidatos <- listado_informacion %>%
  filter(tipo_dato == "Candidatos") %>%
  mutate(data = map(files, read_excel) %>% map(clean_names)) %>%
  select(eleccion, ballotage, data) %>%
  unnest(data)

# TODO: ISP presidencial

# listado_informacion %>%
#   filter(tipo_dato == "ISP") %>%
#   mutate(data = map(files, read_excel, col_types = "text") %>% map(clean_names) %>% map(names)) %>%
#   select(eleccion, ballotage, data) %>%
#   unnest(data) %>%
#   count(data)
