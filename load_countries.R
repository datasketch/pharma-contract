source("data_load.R")
countries <- TravelBucketList$`Countries (DO NOT MODIFY)`$select_all()
countries <- countries |> dplyr::rename(id_countries = id,
                                        id_appeals = `B. Appeals`,
                                        id_request = `A. Requests`,
                                        id_contracts = `C. Contracts`,
                                        id_movement = `TI Movement`)



dic_countries <- dplyr::tibble(id = names(countries),
                               label = names(countries),
                               hdType = purrr::map(countries, ~class(.x)) |> unlist())
var_list <- dic_countries |>
  dplyr::filter(hdType %in% "list")
var_list <- var_list$id
ls <- purrr::map(var_list, function(i) {
  unlist_var(countries[,c("id_countries", i)], i) |>
    tidyr::drop_na({{ i }})
}) |>  purrr::reduce(dplyr::left_join, by = "id_countries")

countries <- countries |>
  dplyr::select(id_countries, Country) |>
  dplyr::left_join(ls)
countries
