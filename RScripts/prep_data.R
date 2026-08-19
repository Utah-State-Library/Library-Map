##### Prep Data #####

source("RScripts/lists.R", local = TRUE)$value

#### UT Outlets ####
outlets <- readRDS("data/pls_outlet_national_2025.rds") %>%
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

current_year <- max(as.numeric(outlets$FISCAL_YEAR))

#### UT PLS ####
pls_ut <- readRDS("data/pls_national_2025.rds") %>% #pls_national_updated.rds
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
    TOTINCM,
    VLNT,
    VLNT_HRS,
    KIDPHYSCIR,
    TOTCIR,
    TOTPRO,
    TOTATTEN
  )

saveRDS(pls_ut, "data/processed/pls_ut_app.RDS")

#### National PLS ####

pls_national <- readRDS("data/pls_national_2025.rds") %>% #pls_national_updated.rds
  filter(
    hide_lib == 0,
  ) %>%
  mutate(CNTY = str_to_title(CNTY), CITY = str_to_title(CITY)) %>%
  select(
    FISCAL_YEAR,
    STABR,
    CURRENT_LIBNAME,
    CURRENT_LIBNAME_DISAMB,
    FSCSKEY,
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
    # LOCEXP, #UT specific
    # STEXP,
    # FEDEXP,
    # OTHEXP,
    # TOTEXP,
    PRMATEXP,
    ELMATEXP,
    OTHMATEX,
    TOTEXPCO,
    TOTOPEXP,
    BKVOL,
    AUDIO_PH,
    VIDEO_PH,
    OTHPHYS,
    TOTPHYS,
    # EBOOK_CIR,
    # ESERIAL_CIR,
    # EAUDIO_CIR,
    # EVIDEO_CIR,
    KIDPHYSCIR,
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
    K0_5PRO,
    K0_5ATTEN,
    K6_11PRO,
    K6_11ATTEN,
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

#pls_national %<>% select(-CURRENT_LIBNAME)

saveRDS(pls_national, "data/processed/pls_national_app.RDS")

national_vars <- setdiff(
  names(pls_national),
  c(
    "FISCAL_YEAR",
    "CURRENT_LIBNAME",
    "CURRENT_LIBNAME_DISAMB",
    "FSCSKEY",
    "STABR",
    "state",
    "POPU_LSA"
  )
)


#### PLS Utah ####

pls_utah %<>%
  mutate(
    FTE_col = ifelse(TOTSTAFF == 0, NA, TOTSTAFF),
    POP_col = POPU_LSA,
    FTE = TOTSTAFF
  ) %>%
  pivot_longer(cols = national_vars, names_to = "var", values_to = "value") %>%
  pivot_longer(
    cols = c(POP_col, FTE_col),
    names_to = "per_name",
    values_to = "per_value"
  )

pls_utah %<>%
  group_by(var) %>%
  mutate(
    per_multiplier = case_when(
      var %in% per100cols & per_name == "POP_col" ~ 100,
      var %in% per1000cols & per_name == "POP_col" ~ 1000,
      .default = 1
    ),
    per_text = case_when(
      per_name == "FTE_col" ~ "Per FTE",
      per_multiplier == 100 ~ "Per 100 People",
      per_multiplier == 1000 ~ "Per 1,000 People",
      .default = "Per Capita"
    )
  ) %>%
  ungroup()

pls_utah %<>%
  rowwise() %>%
  mutate(per_calc = round((value * per_multiplier) / per_value, 2)) %>%
  ungroup() %>%
  group_by(FISCAL_YEAR, var, per_name) %>%
  mutate(
    rank = if (all(is.na(per_calc))) {
      NA_real_
    } else {
      rank(-per_calc, na.last = "keep", ties.method = "average")
    },
    n = ifelse(!is.na(rank), sum(!is.na(per_calc)), NA)
  ) %>%
  ungroup()

saveRDS(pls_utah, "data/processed/pls_utah_appv2.RDS")

#### Library Map ####
# uses outlets & pls dfs from above

map_all <- outlets %>%
  left_join(pls_ut, by = c("CURRENT_LIBNAME_AE" = "CURRENT_LIBNAME")) %>%
  group_by(CURRENT_LIBNAME_AE) %>%
  mutate(
    n_locs = sum(C_OUT_TY == "CE") + sum(C_OUT_TY == "BR"),
    OUTLET_NAME = gsub(
      paste0(CURRENT_LIBNAME_AE, " "),
      "",
      CURRENT_LIBNAME_OUTLET
    ),
    OUTLET_NAME = gsub(
      "Salt Lake City Public Library |Washington County Library |Weber County Library",
      "",
      OUTLET_NAME
    ),
    OUTLET_NAME = trimws(OUTLET_NAME)
  ) %>%
  ungroup()

map_all %<>%
  mutate(
    LAT = as.numeric(LAT),
    LONG = as.numeric(LONG),
    library_data_header = case_when(
      n_locs == 1 ~ paste0(
        "
      <table style='width: 100%'>
        <div style='font-size: 14px;'><b>",
        CURRENT_LIBNAME_AE,
        "</b><br>",
        FISCAL_YEAR,
        " Public Library Survey",
        "<br></div><br>"
      ),
      n_locs > 1 ~ paste0(
        "
      <table style='width: 100%'>
        <div style='font-size: 14px;'><b>",
        CURRENT_LIBNAME_AE,
        "</b><br>",
        FISCAL_YEAR,
        " Public Library Survey",
        "<br>",
        "</div> <div style='font-size: 12px;'><em>",
        "This table shows data for the entire library system, and is not branch specific.</em>",
        "<br></div>"
      )
    ),
    library_data_table = paste0(
      "<tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Number of Library Locations: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      n_locs,
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #ffffff;\">",
      "Population of Legal Service Area: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      format(POPU_LSA, big.mark = ","),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Cardholders: ",
      "</td>
              <td style = \"text-align: right; background-color: #f2f2f2;\">",
      format(REGBOR, big.mark = ","),
      "</td>
            </tr> <tr>
              <td style = \"text-align:left; background-color: #ffffff;\">",
      "Visits: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      format(VISITS, big.mark = ","),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Number of Library Staff: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      format(TOT_LIB_STAFF, big.mark = ""),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #ffffff;\">",
      "Total FTE of Library Staff: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      format(TOTSTAFF, big.mark = ","),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Total Revenue: ",
      "</td>
              <td style = \"text-align: right; background-color: #f2f2f2;\">",
      dollar(TOTINCM),
      "</td>
            </tr> <tr>
              <td style = \"text-align:left; background-color: #ffffff;\">",
      "Local Government Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      dollar(LOCGVT),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "State Government Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      dollar(STGVT),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #ffffff;\">",
      "Federal Government Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #ffffff;\">",
      dollar(FEDGVT),
      "</td>
        </tr> <tr>
          <td style = \"text-align:left; background-color: #f2f2f2;\">",
      "Other Revenue: ",
      "</td>
          <td style = \"text-align: right; background-color: #f2f2f2;\">",
      dollar(OTHINCM),
      "</td>
        </tr> </table>"
    ),
    library_header = case_when(
      CURRENT_LIBNAME_OUTLET != CURRENT_LIBNAME_AE ~
        paste0(
          "<table style='width: 100%'>
           <div style='font-size: 16px;'><b>",
          CURRENT_LIBNAME_AE,
          "</b> </div> <hr>
           <div style='font-size: 14px;'><b>",
          OUTLET_NAME,
          "</b> </div>"
        ),
      CURRENT_LIBNAME_OUTLET == CURRENT_LIBNAME_AE ~
        paste0(
          "<table>
           <div style='font-size: 16px;'><b>",
          CURRENT_LIBNAME_OUTLET,
          "</b>",
          "</div>"
        )
    ),
    library_label = paste0(
      library_header,
      "<div style='font-size: 12px;'>",
      str_to_title(ADDRESS),
      ", ",
      str_to_title(CITY),
      ", ",
      ZIP,
      "<hr><div style='font-size: 12px;'>",
      "Click to see system-wide information",
      "</div> </table>"
    ),
    library_popup = paste0(
      library_data_header,
      library_data_table
    )
  )

saveRDS(map_all, "data/processed/library_map_app.RDS")


#### PLS National - Peer Libraries Data ####

# Subset to most recent 6 years for peer stability & data size
pls_national_peers <- pls_national %>%
  mutate(
    FTE_col = ifelse(TOTSTAFF == 0, NA, TOTSTAFF),
    POP_col = POPU_LSA,
    FTE = TOTSTAFF
  ) %>%
  filter(FISCAL_YEAR %in% c(current_year:(current_year - 5))) %>%
  pivot_longer(cols = national_vars, names_to = "var", values_to = "value") %>%
  pivot_longer(
    cols = c(POP_col, FTE_col),
    names_to = "per_name",
    values_to = "per_value"
  )

pls_national_peers %<>%
  mutate(
    per_multiplier = case_when(
      var %in% per100cols & per_name == "POP_col" ~ 100,
      var %in% per1000cols & per_name == "POP_col" ~ 1000,
      .default = 1
    ),
    per_text = case_when(
      per_name == "FTE_col" ~ "Per FTE",
      per_multiplier == 100 ~ "Per 100 People",
      per_multiplier == 1000 ~ "Per 1,000 People",
      .default = "Per Capita"
    )
  ) %>%
  ungroup()


pls_national_peers %<>%
  rowwise() %>%
  mutate(per_calc = round((value * per_multiplier) / per_value, 2)) %>%
  ungroup()


pls_national_peers %<>%
  group_by(FISCAL_YEAR, state, var, per_name) %>%
  mutate(
    rank_state = if (all(is.na(per_calc))) {
      NA_real_
    } else {
      rank(-per_calc, na.last = "keep", ties.method = "average")
    },
    n_state = ifelse(!is.na(rank_state), sum(!is.na(per_calc)), NA)
  ) %>%
  ungroup() %>%
  group_by(FISCAL_YEAR, var, per_name) %>%
  mutate(
    rank_national = if (all(is.na(per_calc))) {
      NA_real_
    } else {
      rank(-per_calc, na.last = "keep", ties.method = "average")
    },
    n_national = ifelse(!is.na(rank_national), sum(!is.na(per_calc)), NA)
  ) %>%
  ungroup()

saveRDS(pls_national_peers, "data/processed/pls_national_peers_appv2.RDS")


#### PLS National - Statewide Summarised Data ####

pls_national_state <- pls_national %>%
  group_by(state, FISCAL_YEAR) %>%
  mutate(
    n_systems = n_distinct(CURRENT_LIBNAME_DISAMB)
  ) %>%
  select(-c(FSCSKEY, CURRENT_LIBNAME_DISAMB, CURRENT_LIBNAME)) %>%
  mutate(across(
    c(POPU_LSA, all_of(national_vars)),
    ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))
  )) %>%
  distinct()

pls_national_state %<>%
  mutate(
    FTE_col = ifelse(TOTSTAFF == 0, NA, TOTSTAFF),
    POP_col = POPU_LSA,
    FTE = FTE_col
  ) %>%
  pivot_longer(cols = national_vars, names_to = "var", values_to = "value") %>%
  pivot_longer(
    cols = c("POP_col", "FTE_col"),
    names_to = "per_name",
    values_to = "per_value"
  )


pls_national_state %<>%
  mutate(
    per_multiplier = case_when(
      var %in% per100cols & per_name == "POP_col" ~ 100,
      var %in% per1000cols & per_name == "POP_col" ~ 1000,
      .default = 1
    ),
    per_text = case_when(
      per_name == "FTE_col" ~ "Per FTE",
      per_multiplier == 100 ~ "Per 100 People",
      per_multiplier == 1000 ~ "Per 1,000 People",
      .default = "Per Capita"
    )
  ) %>%
  ungroup()


pls_national_state %<>%
  rowwise() %>%
  mutate(per_calc = round((value * per_multiplier) / per_value, 2)) %>%
  ungroup()

pls_national_state %<>%
  group_by(FISCAL_YEAR, var, per_name) %>%
  mutate(
    rank = if (all(is.na(per_calc))) {
      NA_real_
    } else {
      rank(-per_calc, na.last = "keep", ties.method = "average")
    },
    n = ifelse(!is.na(rank), sum(!is.na(per_calc)), NA)
  ) %>%
  ungroup()

saveRDS(pls_national_state, "data/processed/pls_national_state_appv2.RDS")

#### National State Map ####

states <- sf::read_sf(
  "https://rstudio.github.io/leaflet/json/us-states.geojson"
)

pls_national_state_map <- states %>%
  left_join(pls_national_state, by = c("name" = "state")) %>%
  #filter(!is.na(POPU_LSA)) %>%
  rename("state" = "name")

pls_national_state_map %<>% # need to shorten colnames to 14 chars or < to save as sf
  rename(
    "YEAR" = "FISCAL_YEAR",
    "pc_multip" = "per_multiplier",
    "pc_text" = "per_text"
  ) %>%
  select(-c(STABR, n_systems)) %>%
  filter(YEAR >= 2016)

write_sf(
  pls_national_state_map,
  "data/processed/pls_national_state_map_appv2.shp"
)


#### National Similarity Data  ####

# This function assumes "dists" is a NAMED vector of distances for one observation
# It also may return more neighbors than requested if there are ties
closest_neighbors <- function(dists, num_closest) {
  elig <- !is.na(dists) & (dists > 0) # Make sure we exclude the diagonal whether it's set to NA or left as 0
  cutoff <- head(sort(dists[elig]), num_closest)[num_closest]
  keep <- elig & dists <= cutoff
  return(names(dists)[keep])
}

calculate_similarity <- function(df_p, year = imls_year) {
  df <- df_p %>% filter(FISCAL_YEAR == year)
  df$name <- paste0(df$CURRENT_LIBNAME_DISAMB) #, 1:nrow(df)
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
    method = 'euclidean' #'manhattan'
  ))

  dimnames(distances) <- list(df$name, df$name)

  df$peers <- lapply(
    seq_len(nrow(distances)),
    function(i) closest_neighbors(distances[i, ], num_closest = 10)
  )

  # make a save df so we can still use all of the df columns for testing below
  df %>% select(FISCAL_YEAR, state, CURRENT_LIBNAME_DISAMB, peers)
}

df_national <- calculate_similarity(pls_national, year = imls_year)

states <- unique(pls_national$state[pls_national$FISCAL_YEAR == imls_year]) %>%
  sort()
df_state <- NULL
for (i in states) {
  df <- pls_national %>% filter(state == i)
  df_calc <- calculate_similarity(df, imls_year)
  df_state %<>% rbind(df_calc)
  rm(df)
}

utah_sim <- pls_national %>%
  filter(state == "Utah", FISCAL_YEAR == max(FISCAL_YEAR))
df_utah_sim <- calculate_similarity(
  utah_sim,
  year = unique(utah_sim$FISCAL_YEAR)
)

df_state %<>% filter(state != "Utah") %>% rbind(df_utah_sim)

df_state %<>%
  rename("state_peers" = "peers") %>%
  select(CURRENT_LIBNAME_DISAMB, state_peers)
df_peers_all <- left_join(df_national, df_state, by = "CURRENT_LIBNAME_DISAMB")
saveRDS(df_peers_all, "data/processed/pls_national_simlibs.RDS")

# ### Testing
# head(df[, c('name', 'peers')], 5)

# x <- df %>%
#   select(
#     CURRENT_LIBNAME_DISAMB,
#     POPU_LSA,
#     TOTSTAFF,
#     TOTINCM,
#     REGBOR,
#     VISITS,
#     peers
#   )

# lib <- "South Routt Library District"

# y <- x %>% filter(CURRENT_LIBNAME_DISAMB == lib)
# y$peers <- gsub("[0-9]", "", y$peers)
# closest <- eval(parse(text = y$peers))

# #closest <- paste0(unlist(y$peers), collapse = "|")
# #closest <- gsub("[0-9]", "", closest)

# z <- x %>%
#   filter(
#     CURRENT_LIBNAME_DISAMB == lib | CURRENT_LIBNAME_DISAMB %in% closest #str_detect(CURRENT_LIBNAME_DISAMB, closest)
#   )
