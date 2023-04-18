#' @title Compare Product Versions
#' @author Gareth Burns
#' @description Compare a product with a different version
#' @param product A character value. The CDISC product
#' @param version A character value. The version of the CDISC version being used
#' @param previous A character value. A previous version to compare
#'   the \code{version} to. If not supplied will compare with preceeding version.
#' @return A character value. A sentence with a joke
#' @seealso \url{https://www.cdisc.org/cdisc-library/api-documentation}
#' @import httr
#' @import jsonlite
#' @export

CompareProductVersions <- function(product, version, previous = NULL) {

  if (is.na(Sys.getenv("CDISC_API_KEY", unset = NA))) {
    stop("Please set 'CDISC_API_KEY' environmental variable")
  }

  requestURL <- ifelse(is.null(previous), {
    sprintf("https://library.cdisclibrary.org/api/mdr/diff/%1$",
            version)
  }, {
    sprintf("https://library.cdisclibrary.org/api/mdr/diff/%1$/$2%s",
            previous)
  })

  response <-
    GET(requestURL,
        accept("application/json"),
        add_headers("api-key" = Sys.getenv("CDISC_API_KEY")))

  if (status_code(response) != 200) {
    stop(sprintf(
      "An error occured with a status code: %1$s",
      status_code(response)
    ))
  } else {
    return(content(response))
  }
}
