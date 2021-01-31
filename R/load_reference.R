#' Load growth reference
#'
#' This function provides access to the built-in growth references.
#' @param refcode String, code of a reference. Only the first element is loaded.
#' @param element String, either `"table"`, `"index"`, `"study"` or
#' `"all"`.  The default is `"table"`.
#' @param con A database connection. The default `con = NULL` reads the
#' data from the internal data of the `jamesreferences` package.
#' Currently not implemented.
#' @return The return value depends on the `element` parameter.
#'
#' `element` | Return value
#' --------- | ---------------------------------------
#' `"table"` | Object of class `zoo`, no `study` attribute
#' `"index"` | Numeric vector with index values
#' `"study"` | Named character vector with study data
#' `"all"`   | All stored information
#'
#' @examples
#' ref <- load_reference("gc_2019_dsc_male_")
#' @export
load_reference <- function(refcode = NULL,
                           element = c("table", "index", "study", "all"),
                           con = NULL) {
  if (!is.null(con)) {
    warning("Database access not implemented.")
    return(NULL)
  }
  if (is.null(refcode)) {
    return(NULL)
  }
  element <- match.arg(element)
  ref <- get0(refcode[[1L]])
  switch(element,
    table = {
      attr(ref, "study") <- NULL
      ref
    },
    index = zoo::index(ref),
    study = attr(ref, "study"),
    all = ref
  )
}
