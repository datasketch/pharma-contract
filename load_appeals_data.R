# Data appeals
source("load_countries.R")
appeals <- TravelBucketList$`B. Appeals`$select_all()
appeals <- appeals |> dplyr::rename(id_appeals = id,
                                    id_request = `Request ID`,
                                    id_countries = `Country`,
                                    id_movement = `TI Movement`
)

ls_app <- purrr::map(names(appeals)[-1], function(i) {
  unlist_var(appeals[,c("id_appeals", i)], i) #|>
  #tidyr::drop_na({{ i }})
}) |>  purrr::reduce(dplyr::left_join, by = "id_appeals")

ls_app <- ls_app |> dplyr::select(-thumbnails)

ls_app <- ls_app |>
  dplyr::group_by(id_appeals) |> 
  dplyr::summarise(dplyr::across(dplyr::everything(),
                                 dplyr::funs(paste_vector)))
names(ls_app) <- gsub("_paste_vector", "", names(ls_app))



countries_appeals <- countries |>
  dplyr::select(id_countries, id_appeals, Country) |>
  dplyr::distinct(.keep_all = TRUE)
data_appeals <- ls_app |>
  dplyr::left_join(countries_appeals)
data_appeals
# dic_appeals <- homodatum::create_dic(data_appeals)
# dic_appeals$id <- dic_appeals$label
# save(data_appeals, file = "data/data_appeals.RData")
# save(dic_appeals, file = "data/dic_appeals.RData")
