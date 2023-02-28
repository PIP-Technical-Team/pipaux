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
names(aux_data) <- aux_dirs

df <- purrr::keep(aux_data, is.data.frame)
dl <- purrr::keep(aux_data, ~{!is.data.frame(.x)})


expand_list <- function(x, name_x) {

  if (is.data.frame(x)) {
    return(x)
  } else {
    names_x <- paste(name_x, names(x), sep = "_")
    l <- purrr::map2(.x = x,
                     .y = names_x,
                     .f = expand_list)
    names(l) <- names_x
    return(l)
  }
}

debugonce(expand_list)
dd <- expand_list(aux_data[[11]], names(aux_data[11]))




for (i in seq_along(dl)) {

  x <- dl[[i]] # each list
  name_x <- names(dl[i])
  if (is.data.frame(x)) {
    df_l[[paste(name_x, names(x[j]), sep = "_")]] <- x
    next
  }


  for (j in seq_along(x)) {
    df_l[[paste(name_x, names(x[j]), sep = "_")]] <- x[[j]]
  }

}

dm_data <- append(df, df_l) |>
  dm()


usethis::use_data(aux_data_model,
                  overwrite = TRUE,
                  internal = TRUE)
