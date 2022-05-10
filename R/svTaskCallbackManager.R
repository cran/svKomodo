#' Create task callbacks that are evaluated both from R and socket/http server
#'
#' `svTaskCallbackManager()` is a copy of `taskCallbackManager()` in R base
#' package, as of version 4.0.5 of R. Two important differences: (1) the top
#' task created is named `SV-taskCallbackManager` instead of
#' `R-taskCallbackManager`, and its tasks are executed after each top-level task
#' in R console, or after execution of non-hidden R code from the socket or http
#' server (take care: only once per set of code, no matter the number of
#' top-level task in the R code send by the client in the second case). All
#' taskCallbacks defined by `addTaskCallback()` or `taskCallbackManager$add()`
#' from R base package are not executed when code is invoked from the R socket
#' or http server!
#'
#' @param handlers this can be a list of callbacks in which each element is a
#' list with an element named `f` which is a callback function, and an optional
#' element named `data` which is the 5-th argument to be supplied to the
#' callback when it is invoked. Typically this argument is not specified, and
#' one uses add to register callbacks after the manager is created.
#' @param registered a logical value indicating whether the `evaluate`
#' function has already been registered with the internal task callback
#' mechanism. This is usually \code{FALSE} and the first time a callback is
#' added via the add function, the evaluate function is automatically
#' registered. One can control when the function is registered by specifying
#' `TRUE` for this argument and calling `addTaskCallback()` manually.
#' @param verbose a logical value, which if `TRUE`, causes information to be
#' printed to the console about certain activities this dispatch manager
#' performs. This is useful for debugging callbacks and the handler itself.
#'
#' @return See `?taskCallbackManager` for both the returned object and how to use it.
#' @author Slightly modified from the original R core team's function by Ph.
#' Grosjean <phgrosjean@sciviews.org>
#' @seealso [taskCallbackManager()]
#' @keywords IO
#' @concept task callback
#' @export
#' @examples
#' # create a task callback manager
#' cbman <- svTaskCallbackManager()
#' # Add a function to activate after each code evaluation in R
#' cbman$add(function(expr, value, ok, visible) {
#'   cat("Hi from the callback manager!\n")
#'   return(TRUE)
#'   }, name = "exampleHandler")
#' # Just issue a command and see the callback function activated
#' 1 + 1
#' # List defined callbacks
#' cbman$callbacks()
#' # Remove the callback we just defined
#' cbman$remove("exampleHandler")
#' 1 + 1
#' # Remove the task callback manager (base R function)
#' removeTaskCallback("SV-taskCallbackManager")
svTaskCallbackManager <- function(handlers = list(), registered = FALSE,
verbose = FALSE) {
  suspended <- FALSE
  .verbose <- verbose

  add <- function(f, data = NULL, name = NULL, register = TRUE) {
    if (is.null(name))
      name <- as.character(length(handlers) + 1L)
    handlers[[name]] <<- list(f = f)
    if (!missing(data))
      handlers[[name]][["data"]] <<- data
    if (!registered && register) {
      register()
    }
    name
  }

  remove <- function(which) {
    if (length(which) != 1L)
      stop("'which' must be of length 1")
    if (is.character(which)) {
      tmp <- match(which, names(handlers))
      if (is.na(tmp))
        stop(gettextf("no such element '%s'", which), domain = NA)
      which <- tmp
    } else if (is.numeric(which)) {
      which <- as.integer(which)
      if (which <= 0 || which > length(handlers))
        stop("invalid 'which' argument")
    } else {
      stop("'which' must be character or numeric")
    }
    handlers <<- handlers[-which]
    return(TRUE)
  }

  evaluate <- function(expr, value, ok, visible) {
    if (suspended)
      return(TRUE)
    discard <- character()
    for (i in names(handlers)) {
      h <- handlers[[i]]
      if (length(h) > 1L) {
        val <- h[["f"]](expr, value, ok, visible, h[["data"]])
      } else {
        val <- h[["f"]](expr, value, ok, visible)
      }
      if (!val) {
        discard <- c(discard, i)
      }
    }
    if (length(discard)) {
      if (.verbose)
        cat(gettextf("Removing %s", paste(discard, collapse = ", ")), "\n")
      idx <- is.na(match(names(handlers), discard))
      if (length(idx)) {
        handlers <<- handlers[idx]
      } else {
        handlers <<- list()
      }
    }
    return(TRUE)
  }

  suspend <- function(status = TRUE) {
    suspended <<- status
  }

  register <- function(name = "SV-taskCallbackManager", verbose = .verbose) {
    if (verbose)
      cat(gettext("Registering 'evaluate' as low-level callback\n"))
    id <- addTaskCallback(evaluate, name = name)
    registered <<- TRUE
    id
  }

  list(add = add, evaluate = evaluate, remove = remove, register = register,
    suspend = suspend, callbacks = function() handlers)
}
