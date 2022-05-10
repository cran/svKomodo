.active.data.frame <- list(object = "", fun = function() {
  if (exists(.active.data.frame$object, envir = .GlobalEnv)) {
    obj <- get(.active.data.frame$object, envir = .GlobalEnv)
    res <- paste(c(.active.data.frame$object, names(obj)), "\t",
      c(class(obj), sapply(obj, class)), "\n", sep = "")
    return(.active.data.frame$cache <<- res)
  } else {
    return(.active.data.frame$cache <<- NULL)
  }
}, cache = "")


#' Refresh Komodo interface (active object lists and R object browser)
#'
#' These function manage to refresh the list of active objects in Komodo and
#' also the data displayed in the Komodo R object browser. They should not be
#' called directly by the end-user.
#'
#' @param force do we force refresh, even if data have not changed?
#' @param ... any argument (ignored, but useful for `addTaskCallback()`).
#'
#' @return Both functions return `TRUE` on success.
#' @seealso [koCmd()]
#' @keywords misc
#' @concept interprocess communication Komodo
#' @export
koRefresh <- function(force = FALSE) {
  safekoCmd <- function(cmd, data = NULL, ...) {
    res <- try(koCmd(cmd, data, ...), silent = TRUE)
    return(!inherits(res, "try-error"))
  }

  # Refresh active items and the R Objects explorer
  # If force == TRUE, do not compare with the cache
  # Active items are represented by .active.xxx objects in .GlobalEnv
  aObjs <- ls(pattern = "^\\.active\\.", envir = .GlobalEnv, all.names = TRUE)
  for (item in aObjs) {
    obj <- get(item, envir = .GlobalEnv, inherits = FALSE)
    if (mode(obj) == "list") {
      objclass <- sub("^\\.active\\.", "", item)
      # JavaScript does not accept dots in function names. So, we have
      # to remove them from class name (eg., data.frame => dataframe)
      objclass <- gsub("\\.", "", objclass)
      cache <- obj$cache
      res <- obj$fun()
      if (is.null(res))  # Active object not found, remove obj
        rm(list = item, envir = .GlobalEnv)
      if (isTRUE(force) || !identical(res, cache))  # Refresh in Komodo
        if (!safekoCmd(paste('sv.r.obj_refresh_', objclass,
          '("<<<data>>>");', sep = ""), data = res))
          return(FALSE)
    }
  }

  # Make sure to clear active data frame and active lm object in case none
  # are defined in the current session
  if (!".active.data.frame" %in% aObjs)
    if (!safekoCmd('sv.r.obj_refresh_dataframe("<<<data>>>");'))
      return(FALSE)
  if (!".active.lm" %in% aObjs)
    if (!safekoCmd('sv.r.obj_refresh_lm("<<<data>>>");'))
      return(FALSE)

  # Refresh object browser (only data from .GlobalEnv)
  lst <- objList(envir = .GlobalEnv, all.info = FALSE, compare = TRUE)

  if (isTRUE(attr(lst, "changed"))) {
    msg <- paste(capture.output(print(lst, sep = ";;", header = TRUE)),
      collapse = "\n")
    if (!safekoCmd('sv.r.objects.refreshGlobalEnv("<<<data>>>");', data = msg))
      return(FALSE)
  }
  return(TRUE)
}

#' @rdname koRefresh
#' @export
koAutoRefresh <- function(...) {
  try(koRefresh(force = FALSE), silent = TRUE)
  return(TRUE)  # We need to return TRUE for callback reschedule
}

# FIXME: When Komodo server is not available:
#> koAutoRefresh()
#Error in call[[1L]] : object of type 'symbol' is not subsettable
# PhG: I hope changes with safekoCmd() will do the job in this case!
