# tidyr functions -------------------------------------------------------------------------------------------------------
list_col_type <- function(x) {
  is_data_frame <- vapply(x, is.data.frame, logical(1))
  is_atomic <- vapply(x, is.atomic, logical(1))

  if (all(is_data_frame)) {
    "dataframe"
  } else if (all(is_atomic)) {
    "atomic"
  } else {
    "mixed"
  }
}


enframe <- function(x, col_name, .id = NULL) {
  out <- data_frame(dplyr::combine(x))
  names(out) <- col_name

  if (!is.null(.id)) {
    out[[.id]] <- id_col(x)
  }
  out
}

id_col <- function(x) {
  stopifnot(is.list(x))

  ids <- if (is.null(names(x))) seq_along(x) else names(x)
  lengths <- vapply(x, length, integer(1))

  ids[rep(seq_along(ids), lengths)]
}


unnest_c <- function(data, ..., .drop = NA, .id = NULL, .sep = NULL) {
  dots <- lazyeval::lazy_dots(...)
  if (length(dots) == 0) {
    list_cols <- names(data)[vapply(data, is.list, logical(1))]
    list_col_names <- lapply(list_cols, as.name)
    dots <- lazyeval::as.lazy_dots(list_col_names, env = parent.frame())
  }
  dots
}

`%||%` <- function(x, y) if (length(x) == 0) y else x

compact <- function(x) x[vapply(x, length, integer(1)) > 0]


# test --------------------------------------------------------------------

library(dplyr)
library(tidyr)
data <- data_frame(
  a = list(c("a", "b"), "c"),
  b = list(1:2, 3),
  c = c(11, 22)
)
data
unnest(data)

df <- data_frame(
  x = 1:2,
  y = list(a = 1, b = 3:4)
)
df
unnest(df, .id = "name")

ldf <- list(data_frame(x = 1))
data <- data_frame(x = ldf, y = ldf)

data
unnest(data)
unnest(data, .sep = "_")

l <- lapply(1:5, function(x) runif(5))
names(l) <- LETTERS[1:5]

data <- dplyr::data_frame(x = 1:5, y = l, z = setNames(l, LETTERS[6:10]))
data <- dplyr::data_frame(x = 1:5, y = l, z = l)
unnest(data, .id = "names")

ldf <- lapply(l, function(x) data.frame(a = x))
data <- dplyr::data_frame(x = 1:5, y = ldf, z = ldf)
unnest(data, .id = "names", .sep = "_")

data <- dplyr::data_frame(x = 1:5, y = ldf, z = setNames(ldf, LETTERS[6:10]))
unnest(data, .id = "names", .sep = "_")

.drop = NA
.id = "names"
.sep = "_"

unnest_cols <- unnest_c(data)
