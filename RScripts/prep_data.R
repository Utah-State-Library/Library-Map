##### Prep Data #####

#### UT Outlets ####
outlets <- readRDS("data/pls_outlet_national.rds") %>%
  filter(STABR == "UT", hide_lib == 0, FISCAL_YEAR == max(FISCAL_YEAR)) %>%
  mutate(
    CITY = case_when(
      CITY == "South Salt Lake City" ~ "South Salt Lake",
      CITY == "Mt. Pleasant" ~ "Mount Pleasant",
      .default = CITY
    ),
    LAT = case_when(
      CURRENT_LIBNAME_OUTLET == "Teen Center" ~ 38.57367327593949,
      .default = LAT
    ),
    LONG = case_when(
      CURRENT_LIBNAME_OUTLET == "Teen Center" ~ -109.54459779999999,
      .default = LONG
    )
  )

saveRDS(outlets, "data/processed/outlet_ut_app.RDS")

#### UT PLS ####
pls <- readRDS("data/pls_national.rds") %>%
  filter(
    STABR == "UT",
    hide_lib == 0,
    FISCAL_YEAR == current_year
  ) %>%
  mutate(CNTY = str_to_title(CNTY), CITY = str_to_title(CITY)) %>%
  select(
    CURRENT_LIBNAME,
    POPU_LSA,
    REGBOR,
    VISITS,
    TOTSTAFF,
    TOT_LIB_STAFF,
    LOCGVT,
    STGVT,
    FEDGVT,
    OTHINCM,
    TOTINCM
  )

saveRDS(pls, "data/processed/pls_ut_app.RDS")

#### National PLS ####

pls_national <- readRDS("data/pls_national.rds") %>%
  filter(
    hide_lib == 0,
  ) %>%
  mutate(CNTY = str_to_title(CNTY), CITY = str_to_title(CITY)) %>%
  select(
    FISCAL_YEAR,
    STABR,
    CURRENT_LIBNAME,
    CURRENT_LIBNAME_DISAMB,
    POPU_LSA,
    REGBOR,
    VISITS,
    REFERENC,
    LIBRARIA,
    MASTER,
    OTHPAID,
    TOTSTAFF,
    #TOT_LIB_STAFF, #UT specific & not in IMLS data
    LOCGVT,
    STGVT,
    FEDGVT,
    OTHINCM,
    TOTINCM,
    BKVOL,
    AUDIO_PH,
    VIDEO_PH,
    # OTHMATS,
    TOTPHYS,
    # EBOOK_CIR,
    # ESERIAL_CIR,
    # EAUDIO_CIR,
    # EVIDEO_CIR,
    KIDCIRCL,
    PHYSCIR,
    OTHPHCIR,
    TOTCIR,
    ELMATCIR,
    LOANTO,
    LOANFM,
    TOTPRO,
    TOTATTEN,
    # KIDPRO,
    # KIDATTEN,
    YAPRO,
    YAATTEN,
    ADULTPRO,
    ADULTATTEN,
    GENPRO,
    GENATTEN,
    GPTERMS,
    PITUSR,
    WIFISESS
  )

pls_national %<>%
  mutate(
    drop = ifelse(
      STABR == "UT" &
        str_detect(CURRENT_LIBNAME_DISAMB, "Bookmobile|Garden City"),
      1,
      0
    )
  ) %>%
  filter(drop != 1) %>%
  select(-drop)

pls_national[pls_national == -1] <- NA
pls_national[pls_national == -3] <- NA
pls_national[pls_national == -9] <- NA

st_crosswalk <- data.frame(
  state = c(
    state.name,
    "Washington DC",
    "Guam",
    "N. Mariana Islands",
    "Virgin Islands",
    "American Samoa"
  ),
  abb = c(state.abb, "DC", "GU", "MP", "VI", "AS")
)

pls_national %<>% left_join(st_crosswalk, by = c("STABR" = "abb"))
pls_utah <- pls_national %>% filter(state == "Utah")

pls_national %<>% select(-CURRENT_LIBNAME)

saveRDS(pls_national, "data/processed/pls_national_app.RDS")


pls_utah %<>%
  pivot_longer(cols = national_vars, names_to = "var", values_to = "value")

pls_utah %<>%
  rowwise() %>%
  mutate(percap = value / POPU_LSA) %>%
  ungroup() %>%
  group_by(var) %>%
  mutate(
    percap_multiplier = case_when(
      median(percap, na.rm = T) <= 1 & median(percap, na.rm = T) > .1 ~ 100,
      median(percap, na.rm = T) <= .1 & median(percap, na.rm = T) > .01 ~ 1000,
      median(percap, na.rm = T) <= .01 ~ 10000,
      # median(percap, na.rm = T) <= .001 ~ 100000,
      .default = 1
    ),
    percap_text = case_when(
      percap_multiplier == 100 ~ "Per 100 People",
      percap_multiplier == 1000 ~ "Per 1,000 People",
      percap_multiplier == 10000 ~ "Per 10,000 People",
      # percap_multiplier == 100000 ~ "Per 100,000 People",
      .default = "Per Capita"
    )
  ) %>%
  ungroup()

pls_utah %<>%
  rowwise() %>%
  mutate(percap = round((value * percap_multiplier) / POPU_LSA, 2)) %>%
  ungroup() %>%
  group_by(FISCAL_YEAR, var) %>%
  mutate(
    rank = if (all(is.na(percap))) {
      NA_real_
    } else {
      rank(-percap, na.last = "keep", ties.method = "average")
    },
    n = ifelse(!is.na(rank), sum(!is.na(percap)), NA)
  ) %>%
  ungroup()

saveRDS(pls_utah, "data/processed/pls_utah_app.RDS")


#### PLS National - Peer Libraries Data ####

# Subset to most recent 6 years for peer stability & data size
pls_national_peers <- pls_national %>%
  filter(FISCAL_YEAR %in% c(current_year:(current_year - 5))) %>%
  pivot_longer(cols = national_vars, names_to = "var", values_to = "value")

pls_national_peers %<>%
  rowwise() %>%
  mutate(percap = value / POPU_LSA) %>%
  ungroup() %>%
  mutate(
    percap_multiplier = case_when(
      var %in% per100cols ~ 100,
      var %in% per1000cols ~ 1000,
      #var %in% per10000cols ~ 10000,
      # median(percap, na.rm = T) <= 1 & median(percap, na.rm = T) > .1 ~ 100,
      # median(percap, na.rm = T) <= .1 & median(percap, na.rm = T) > .01 ~ 1000,
      # median(percap, na.rm = T) <= .01 ~ 10000,
      # median(percap, na.rm = T) <= .001 ~ 100000,
      .default = 1
    ),
    percap_text = case_when(
      percap_multiplier == 100 ~ "Per 100 People",
      percap_multiplier == 1000 ~ "Per 1,000 People",
      percap_multiplier == 10000 ~ "Per 10,000 People",
      # percap_multiplier == 100000 ~ "Per 100,000 People",
      .default = "Per Capita"
    )
  ) %>%
  ungroup()

pls_national_peers %<>%
  rowwise() %>%
  mutate(percap = round((percap * percap_multiplier), 2)) %>%
  ungroup()

saveRDS(pls_national_peers, "data/processed/pls_national_peers_app.RDS")


#### PLS National - Statewide Summarised Data ####

pls_national_state <- pls_national %>%
  group_by(state, FISCAL_YEAR) %>%
  mutate(
    n_systems = n_distinct(CURRENT_LIBNAME_DISAMB)
  ) %>%
  select(-CURRENT_LIBNAME_DISAMB) %>%
  mutate(across(
    c(POPU_LSA, all_of(national_vars)),
    ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))
  )) %>%
  distinct()

pls_national_state %<>%
  pivot_longer(cols = national_vars, names_to = "var", values_to = "value")

pls_national_state %<>%
  rowwise() %>%
  mutate(percap = value / POPU_LSA) %>%
  ungroup() %>%
  group_by(var) %>%
  mutate(
    percap_multiplier = case_when(
      median(percap, na.rm = T) <= 1 & median(percap, na.rm = T) > .1 ~ 100,
      median(percap, na.rm = T) <= .1 & median(percap, na.rm = T) > .01 ~ 1000,
      median(percap, na.rm = T) <= .01 &
        median(percap, na.rm = T) > .001 ~ 10000,
      median(percap, na.rm = T) <= .001 ~ 100000,
      .default = 1
    ),
    percap_text = case_when(
      percap_multiplier == 100 ~ "Per 100 People",
      percap_multiplier == 1000 ~ "Per 1,000 People",
      percap_multiplier == 10000 ~ "Per 10,000 People",
      percap_multiplier == 100000 ~ "Per 100,000 People",
      .default = "Per Capita"
    )
  ) %>%
  ungroup()

pls_national_state %<>%
  rowwise() %>%
  mutate(percap = round((value * percap_multiplier) / POPU_LSA, 2)) %>%
  ungroup() %>%
  group_by(FISCAL_YEAR, var) %>%
  mutate(
    rank = if (all(is.na(percap))) {
      NA_real_
    } else {
      rank(-percap, na.last = "keep", ties.method = "average")
    },
    n = ifelse(!is.na(rank), sum(!is.na(percap)), NA)
  ) %>%
  ungroup()

saveRDS(pls_national_state, "data/processed/pls_national_state_app.RDS")


# state_shapes <- st_read("data/states_terrs_natwservice/s_16ap26.shp")
# state_shapes <- state_shapes %>% st_transform('+proj=longlat +datum=WGS84')

# state_shapes_simple <- ms_simplify(state_shapes, keep = .05, keep_shapes = TRUE)
states <- sf::read_sf(
  "https://rstudio.github.io/leaflet/json/us-states.geojson"
)

pls_national_state_map <- states %>%
  left_join(pls_national_state, by = c("name" = "state")) %>%
  filter(!is.na(POPU_LSA)) %>%
  rename("state" = "name")

pls_national_state_map %<>%
  rename(
    "YEAR" = "FISCAL_YEAR",
    "pc_multip" = "percap_multiplier",
    "pc_text" = "percap_text"
  )

write_sf(
  pls_national_state_map,
  "data/processed/pls_national_state_map_app.shp"
)


### Similarity Data

df <- pls_national %>% filter(FISCAL_YEAR == 2023)

df$name <- paste0(df$CURRENT_LIBNAME_DISAMB, 1:nrow(df))

df %<>%
  mutate(
    POPU_LSA_scl = scale(POPU_LSA),
    TOTSTAFF_scl = scale(TOTSTAFF),
    TOTINCM_scl = scale(TOTINCM),
    REGBOR_scl = scale(REGBOR),
    VISITS_scl = scale(VISITS)
  )

# Make a distance matrix, and name the dimensions
distances <- as.matrix(dist(
  df[, c(
    "POPU_LSA_scl",
    "TOTSTAFF_scl",
    "TOTINCM_scl",
    "REGBOR_scl",
    "VISITS_scl"
  )],
  method = 'manhattan' #'euclidean'
))
dimnames(distances) <- list(df$name, df$name)

# This function assumes "dists" is a NAMED vector of distances for one observation
# It also may return more neighbors than requested if there are ties
closest_neighbors <- function(dists, num_closest) {
  elig <- !is.na(dists) & (dists > 0) # Make sure we exclude the diagonal whether it's set to NA or left as 0
  cutoff <- head(sort(dists[elig]), num_closest)[num_closest]
  keep <- elig & dists <= cutoff
  return(names(dists)[keep])
}

# The distance matrix rows/columns are already named,
# so the vector passed to closest_neighbors() by apply() is named

df$peers <- apply(distances, 1, closest_neighbors, num_closest = 10)

# make a save df so we can still use all of the df columns for testing below
df_save <- df %>% select(FISCAL_YEAR, CURRENT_LIBNAME_DISAMB, peers)

saveRDS(df_save, "data/processed/pls_national_simlibs.RDS")

### Testing
head(df[, c('name', 'peers')], 5)

x <- df %>%
  select(
    CURRENT_LIBNAME_DISAMB,
    POPU_LSA,
    TOTSTAFF,
    TOTINCM,
    REGBOR,
    VISITS,
    peers
  )

lib <- "South Routt Library District"

y <- x %>% filter(CURRENT_LIBNAME_DISAMB == lib)
y$peers <- gsub("[0-9]", "", y$peers)
closest <- eval(parse(text = y$peers))

#closest <- paste0(unlist(y$peers), collapse = "|")
#closest <- gsub("[0-9]", "", closest)

z <- x %>%
  filter(
    CURRENT_LIBNAME_DISAMB == lib | CURRENT_LIBNAME_DISAMB %in% closest #str_detect(CURRENT_LIBNAME_DISAMB, closest)
  )
