#' Connect to the SciViews-K (Komodo Edit/IDE) socket server and run JavaScript code in Komodo
#'
#' If Komodo Edit/IDE with the SciViews-K extension is running on your machine,
#' you can connect to its socket server and run javascript code in it with this
#' function.
#'
#' @param cmd the JavaScript you want to execute in Komodo Edit, in a character
#  string vector.
#' @param data if a named list, replace `<<<name>>>` in cmd for each name of the
#' list by its component first. If a character string, replace `<<<data>>>` in
#' cmd. If `NULL` (by default), do nothing to cmd before submitting it. See the
#' last examples for using data.
#' @param async not used yet!
#' @param host the host where Komodo is located. Currently, only `localhost` is
#' accepted. Can be changed by setting `options(ko.host = ....)`.
#' @param port the socket port where the SciViews-K server is listening, by
#' default, it is port 7052. Can be changed by setting
#' `options(ko.port = ....)`.
#' @param kotype the type of Komodo server in use. Currently
#' (SciViews-K >= 0.9-25), it can be either `"socket"` or `"file"`.
#' @param timeout number of seconds to wait for a response.
#' @param type which type of Komodo command do we send? If `type = "js"` (by
#' default), `cmd` is considered to be JavaScript code to be evaluated in
#' Komodo. If `type = "rjsonp"`, `cmd` is parsed as RJson object with padding
#' (included in a JavaScript function call) and will be evaluated as such in
#' Komodo. if `type = "output"`, the string in `cmd` is considered to be some R
#' output and will be displayed in the Komodo local R console (no evaluation).
#' @param pad a string naming the JavaScript function to run in Komodo, with the
#' constructed RJson object as argument. If `NULL` (by default), the RJson
#' object is evaluated directly without padding.
#' @param ... further arguments to pass to `toRjson()`.
#'
#' @return Returns the results of the evaluation of the javascript code in
#' Komodo Edit/IDE if `async = FALSE`. Note that `async = TRUE` is not
#' supported yet.
#'
#' If there is an error, or `cmd` is an invalid JavaScript code, a character
#' string containing javascript error message is returned (this is changed from
#' version 0.9-47, previously a 'try-error' was returned).
#'
#' @details
#' Komodo Edit (https://www.activestate.com/products/komodo-ide/) is an Open
#' Source (MPL, GPL & LGPL) editor based on the excellent Mozilla platform and
#' the powerful Scintilla text editor widget. It runs on many Linux
#' distributions, on Windows and on MacOS. Komodo IDE was a commercial
#' equivalent, but with many tools for developers, especially targeting
#' languages like Perl, Tcl, Python, Ruby, etc. This product is now freely
#' distributed as well and Komodo Edit was deprecated. However, the project does
#' not appear to be actively maintained any more and it may not work on more
#' recent MacOS or Windows versions.
#'
#' `koCmd()` can only talk to Komodo if the SciViews-K socket server is
#' installed. This server is contained in the SciViews-K extension that you can
#' download from https://github.com/SciViews/sciviewsk. See Komodo documentation
#' to know how to install this extension (drag and drop of the extension on the
#' Komodo window works in most platforms).
#'
#' @note
#' Because of security concerns, the SciViews-K server only allows connections
#' from local clients (running on the same computer). This limitation would be
#' relatively easy to eliminate, but at your own risks!
#'
#' Data are returned from Komodo to R by using the JavaScript function
#' `sv.socket.serverWrite()`, see the examples bellow.
#'
#' @seealso [svSocket::startSocketServer()], [svSocket::processSocket()]
#' @export
#' @keywords IO
#' @concept interprocess communication Komodo
#'
#' @examples
#' \dontrun{
#' # Make sure you have started Komodo Edit or IDE with the SciViews-K extension
#' # installed on the same machine, and the socket server started and then...
#'
#' # Send JavaScript commands to Komodo
#' # Alert box in Komodo, and then reply to R
#' koCmd(c('alert("Hello from R!");',
#'   'sv.socket.serverWrite("Hello from OpenKomodo (" + ko.interpolate.currentFilePath() + ")");'))
#'
#' # Open a web page wih Komodo configuration
#' koCmd("ko.open.URI('about:config','browser');")
#'
#' # Get info from Komodo
#' koCmd("sv.socket.serverWrite(ko.logging.getStack());")
#'
#' # Passing a large amount of data to Komodo, and then, back to R
#' koCmd(paste0('sv.socket.serverWrite("', rep(paste(iris, collapse = "\\\\n"), 10), '");'))
#'
#' # It is easier to use 'data =' instead of paste() for constructing the JS command
#' koCmd('alert("<<<data>>>");', data = search())
#'
#' # Using a named list for data to replace in the cmd
#' koCmd('alert("This is R version <<<major>>>.<<<minor>>>");', R.version)
#'
#' # Sending incorrect JavaScript instruction
#' koCmd('nonexistingJSfunction();')
#' # Should return something like:
#' # "ReferenceError: nonexistingJSfunction is not defined"
#'
#' # Sending RJsonP (RJson with padding) instruction to Komodo
#' koCmd("Hello with RJsonP!", type = "rjsonp", pad = "alert")
#'
#' # This is more useful to pass complex R objects to Komodo
#' koCmd(head(iris), type = "rjsonp", pad = "sv.socket.serverWrite")
#'
#' # Send simple text (no evaluation) to the Komodo R console
#' koCmd("Hello again from R!", type = "output")
#' }
koCmd <- function(cmd, data = NULL, async = FALSE, host = getOption("ko.host"),
port = getOption("ko.port"), kotype = getOption("ko.kotype"), timeout = 2,
type = c("js", "rjsonp", "output"), pad = NULL, ...) {

  type <- match.arg(type)
  if (is.null(host))
    host <- "localhost"  # Default value
  if (is.null(port))
    port <- 7052         # Idem
  if (is.null(kotype))
    kotype <- "file"     # Idem
  cmd <- gsub("\n", "\\\\n", cmd)
  cmd <- paste(cmd, collapse = " ")
  if (is.na(cmd) || is.null(cmd) || length(cmd) == 0) {
    warning("No command supplied in cmd argument")
    return("")
  }
  # Do we need to paste data in the command?
  if (!is.null(data)) {
    "rework" <- function(data) {
      data <- as.character(data)
      data <- gsub("\n", "\\\\\\\\n", data)
      data <- paste(data, collapse = "\\\\n")
      return(data)
    }

    n <- names(data)
    if (is.null(n)) {
      # We assume that we replace '<<<data>>>'
      cmd <- gsub("<<<data>>>", rework(data), cmd)
    } else {# Named data
      # We replace each <<<name>>> in turn
      for (i in 1:length(n))
        cmd <- gsub(paste("<<<", n[i], ">>>", sep = ""),
          rework(data[[n[i]]]), cmd)
    }
  }
  # What type of data do we send?
  cmd <- switch(type,
    js = paste("<<<js>>>", cmd, sep = ""),
    rjsonp = paste("<<<rjsonp>>>", pad, "(",
      paste(toRjson(cmd, ...), collapse = " "), ")", sep = ""),
    cmd)

  otimeout <- getOption("timeout")
  options(timeout = timeout)  # Default timeout is 120 seconds
  # Do we use file or socket server?
  if (kotype == "file") {
    # File is .sv<port> in the temporary dir (/tmp for Linux and Mac OS X)
    tempdir <- "/tmp"
    if (.Platform$OS.type == "windows")
      tempdir <- dirname(tempdir())
    svfile <- file.path(tempdir, paste(".sv", port, sep = ""))
    svoutfile <- paste(svfile, "out", sep = ".")
    # Make sure the output file is deleted
    unlink(svoutfile)
    # Send data to Komodo
    # Need a time-of-day fingerprint in ms
    fp <- as.integer((as.numeric(Sys.time()) %% 24*60*60) * 1000)
    fp <- gsub(" ", "0", format(fp, width = "8", scientific = FALSE))
    cmd <- paste("<<<", fp, ">>>", cmd, sep = "")
    svcon <- file(svfile, open = "w", blocking = TRUE, encoding = "UTF-8")
    try(cat(paste(cmd, collapse = "\n"), file = svcon), silent = TRUE)
    try(close(svcon), silent = TRUE)
    # Wait for output file... or max timeout (add 500ms to this timeout)
    timeout <- Sys.time() + timeout + 0.5
    while (!file.exists(svoutfile) && Sys.time() < timeout)
      Sys.sleep(0.2) # To avoid using too much resources
    if (file.exists(svoutfile)) {
      res <- try(readLines(svoutfile), silent = TRUE)
      Encoding(res) <- "UTF-8"
    } else {
      res <- character(0)
    }
  } else {# This must be a socket server
    tryCatch({
      con <- socketConnection(host = host, port = port, blocking = TRUE)
      writeLines(cmd, con)
      res <- readLines(con)
      close(con)
    }, warning = function(e) {
      stop(simpleError("Komodo socket server is not available!", quote(koCmd)))
    })
#    ret <- try(writeLines(cmd, con), silent = TRUE)
#    if (!inherits(ret, "try-error"))
#      res <- try(readLines(con), silent = TRUE)
#    try(close(con), silent = TRUE)
  }
  options(timeout = otimeout)
  return(res)
}
