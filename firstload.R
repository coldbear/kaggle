packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

dato <- fromJSON("/Users/coldbear/Desktop/sigma_rent_kaggle/train.json")
In [2]:
  # unlist every variable except `photos` and `features` and convert to tibble
vars <- setdiff(names(dato), c("photos", "features"))
dato <- map_at(dato, vars, unlist) %>% tibble::as_tibble(.)
head(dato, n = 1)
is.na(dato)

dato$created2=as.Date(dato$created)
