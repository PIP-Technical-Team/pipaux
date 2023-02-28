## code to prepare `aux_data_model` dataset goes here
library(dm)
library(pipaux)

gls <- pipfun::pip_create_globals()
maindir <-  gls$PIP_DATA_DIR

branch <- "DEV"
aux_dirs <-
  fs::path(maindir, "_aux/", branch) |>
  fs::dir_ls(type = "dir") |>
  fs::path_file()

aux_data <- lapply(aux_dirs, \(.) {
  load_aux(., maindir, branch )
})

df <- purrr::keep(aux_data, is.data.frame)
dl <- purrr::keep(aux_data, ~{!is.data.frame(.x)})

df_l <- list()
for (i in seq_along(dl)) {

  name_x <- names(dl[i])
  x <- dl[[i]] # each list

  for (j in seq_along(x)) {
    df_l[[paste(name_x, names(x[j]), sep = "_")]] <- x[[j]]
  }

}

dm_data <- append(df, df_l) |>
  dm()


usethis::use_data(aux_data_model,
                  overwrite = TRUE,
                  internal = TRUE)
