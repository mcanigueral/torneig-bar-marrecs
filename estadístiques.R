library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(purrr)
library(googlesheets4)
library(highcharter)

config <- config::get(file = "config.yml")

# Google authentication ---------------------------------------------------

options(
  gargle_oauth_cache = config$auth$cache,
  gargle_oauth_email = config$auth$email
)
# googlesheets4::gs4_auth()


# Read Google sheet -------------------------------------------------------

sheet_url <- config$sheet_url

last_results <- map(
  set_names(c('dards', 'futboli', 'butifarra')),
  ~ read_sheet(sheet_url, sheet = .x)
)


# Guanyadors --------------------------------------------------------------

last_results %>% 
  map(
    ~ .x %>% 
      group_by(jugador) %>% 
      summarise(punts = sum(punts)) %>% 
      arrange(desc(punts)) %>% 
      head(3)
  )


# Més partides jugades ----------------------------------------------------

last_results$dards %>% group_by(jugador) %>% summarise(dards = n()) %>% 
  left_join(
    last_results$futboli %>% group_by(jugador) %>% summarise(futboli = n()),
    by = 'jugador'
  ) %>% 
  left_join(
    last_results$butifarra %>% group_by(jugador) %>% summarise(butifarra = n()),
    by = 'jugador'
  ) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    n_partides = dards + futboli + butifarra
  ) %>% 
  arrange(desc(n_partides))


# Més partides als tres jocs ----------------------------------------------

last_results$dards %>% group_by(jugador) %>% summarise(dards = n()) %>% 
  left_join(
    last_results$futboli %>% group_by(jugador) %>% summarise(futboli = n()),
    by = 'jugador'
  ) %>% 
  left_join(
    last_results$butifarra %>% group_by(jugador) %>% summarise(butifarra = n()),
    by = 'jugador'
  ) %>% 
  tidyr::drop_na() %>% 
  mutate(
    mitjana_ponderada = dards*0.33+futboli*0.33+butifarra*0.33
  ) %>% 
  arrange(desc(mitjana_ponderada))



# Més partides perdudes ---------------------------------------------------

last_results$dards %>% filter(punts == 1) %>% group_by(jugador) %>% summarise(dards = n()) %>% 
  left_join(
    last_results$futboli %>% filter(punts == 1) %>% group_by(jugador) %>% summarise(futboli = n()) %>% 
      filter(futboli < 14),
    by = 'jugador'
  ) %>% 
  left_join(
    last_results$butifarra %>% filter(punts == 1) %>% group_by(jugador) %>% summarise(butifarra = n()),
    by = 'jugador'
  ) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    partides_perdues = dards + futboli + butifarra
  ) %>% 
  arrange(desc(partides_perdues)) %>% 
  head(8)


# Més partides guanyades ---------------------------------------------------

last_results$dards %>% filter(punts == 3) %>% group_by(jugador) %>% summarise(dards = n()) %>% 
  left_join(
    last_results$futboli %>% filter(punts == 3) %>% group_by(jugador) %>% summarise(futboli = n()),
    by = 'jugador'
  ) %>% 
  left_join(
    last_results$butifarra %>% filter(punts == 3) %>% group_by(jugador) %>% summarise(butifarra = n()),
    by = 'jugador'
  ) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    partides_guanyades = dards + futboli + butifarra
  ) %>% 
  arrange(desc(partides_guanyades))


# Més dies diferents participant ------------------------------------------


last_results$dards %>% group_by(jugador) %>% summarise(dards = n_distinct(date(datetime))) %>% 
  left_join(
    last_results$futboli %>% group_by(jugador) %>% summarise(futboli = n_distinct(date(datetime))),
    by = 'jugador'
  ) %>% 
  left_join(
    last_results$butifarra %>% group_by(jugador) %>% summarise(butifarra = n_distinct(date(datetime))),
    by = 'jugador'
  ) %>% 
  replace(is.na(.), 0) %>% 
  mutate(
    mitjana_ponderada = dards*0.33+futboli*0.33+butifarra*0.33
  ) %>% 
  arrange(desc(mitjana_ponderada))

last_results$dards %>% mutate(dia = date(datetime)) %>% select(jugador, dia) %>% 
  bind_rows(
    last_results$futboli %>% mutate(dia = date(datetime)) %>% select(jugador, dia)
  ) %>% 
  bind_rows(
    last_results$butifarra %>% mutate(dia = date(datetime)) %>% select(jugador, dia)
  ) %>% 
  distinct() %>% 
  group_by(jugador) %>% 
  summarise(n_dies = n_distinct(dia)) %>% 
  arrange(desc(n_dies))

