source("load_countries.R")
# data contracts
contracts <- TravelBucketList$`C. Contracts`$select_all()
contracts <- contracts |> dplyr::rename(id_contracts = id,
                                    id_countries = `Country`)
contracts <- contracts |> tidyr::unnest(id_countries)
contracts <- contracts |> tidyr::unnest(Vaccine, keep_empty = T)
countries_contracts <- countries |>
  dplyr::select(id_countries, id_contracts, Country) |>
  dplyr::distinct(.keep_all = TRUE)
data_contracts <- contracts |>
  dplyr::left_join(countries_contracts)
data_contracts
# dic_contracts <- homodatum::create_dic(data_contracts)
# dic_contracts$id <- dic_contracts$label
# save(data_contracts, file = "data/data_contracts.RData")
# save(dic_contracts, file = "data/dic_contracts.RData")
