source("load_countries.R")

request <- TravelBucketList$`A. Requests`$select_all()
request <- request |> dplyr::rename(id_request = id,
                                    id_appeals = `B. Appeals`,
                                    id_countries = `Country`,
                                    id_movement = `Chapter or National Contact`
                                    )

dic_request <- dplyr::tibble(id = names(request),
                         label = names(request),
                         hdType = purrr::map(request, ~class(.x)) |> unlist())

var_list <- dic_request |>
  dplyr::filter(hdType %in% "list")
var_list <- var_list$id
ls_req <- purrr::map(var_list, function(i) {
  unlist_var(request[,c("id_request", i)], i) #|>
    #tidyr::drop_na({{ i }})
}) |>  purrr::reduce(dplyr::left_join, by = "id_request")
other_var <- dic_request |>
  dplyr::filter(hdType != "list")
request <- request[,other_var$id] |>
  dplyr::left_join(ls_req)

request <- request |> dplyr::select(-thumbnails)

request <- request |>
  dplyr::group_by(id_request, `Authority summoned`) |> 
  dplyr::summarise(dplyr::across(dplyr::everything(),
                                 list(paste_vector)))
names(request) <- gsub("_1", "", names(request))
# data request
countries_request <- countries |>
  dplyr::select(id_countries, id_request, `Request Country` = Country) |>
  dplyr::distinct(.keep_all = TRUE)
data_request <- request |>
  dplyr::left_join(countries_request)
data_request <- data_request |>
  dplyr::rename(Date = `Submission date (DD/MM/YYYY)`,
                Authority = `Authority summoned`,
                `Authority type` = `Type of authority`,
                `Request type (formal / informal)` = `Type request`)
data_request
#dic_request <- homodatum::create_dic(data_request)
#dic_request$id <- dic_request$label
#save(data_request, file = "data/data_request.RData")
#save(dic_request, file = "data/dic_request.RData")
