
paste_vector <- function(x, collapse = ",") {
  paste0(trimws(unique(x)), collapse = collapse)
}

unlist_var <- function(data, vars) {
  data |>  tidyr::unnest({{ vars }}, keep_empty = TRUE)
}

readRenviron(".Renviron")
TravelBucketList <- airtabler::airtable(base = "appXq2Y21vsALZOxB", 
                                        tables = c("Countries (DO NOT MODIFY)",
                                                   "TI Movement (DO NOT MODIFY)",
                                                   "A. Requests",
                                                   "B. Appeals",
                                                   "C. Contracts"
                                        ))

TravelBucketList


# 
 
# 

# 
# # movement <- TravelBucketList$`TI Movement (DO NOT MODIFY)`$select_all()
# # movement <- movement |>
# #   dplyr::rename(id_movement = id,
# #                 id_countries = Country,
# #                 id_appeals = `B. Appeals`,
# #                 id_request = `COVID-19 Vaccine FOIA`)
# 
# 
# 
# 
