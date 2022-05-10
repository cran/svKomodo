#' Install and uninstall hooks for communicating with Komodo Edit/IDE
#'
#' Install functions in `SciViews:TempEnv` and callbacks required to communicate
#' with Komodo Edit with the SciViews-K extension (see
#' https://github.com/SciViews/sciviewsk).
#'
#' @return Returns nothing.
#'
#' @details
#' The minimum instruction to install the communication with Komodo/SciViews-K
#' (so called, SciViews Komodo) is to use:
#' `options(ko.serve = 8888); require(svKomodo)`. When the `ko.serve` option is
#' set, svKomodo loads {svSocket}, starts the socket server listening to the
#' port you have selected (8888 by default), and install the hooks and callbacks
#' required to communicate with SciViews Komodo.
#'
#' Before loading svKomodo, you can also set `option(ko.port = 7052)` or another
#' port number where the Komodo SciViews-K server is listening (7052 is the
#' default value). If the Komodo client is running on a different machine, you
#' should also set `ko.host = "xxx.xxx.xxx.xxx"`, where `xxx.xxx.xxx.xxx` is the
#' IP address of the Komodo client's machine, before loading svKomodo (note that
#' running R and Komodo on separate machines is not supported yet, but this is a
#' planned feature and corresponding configurations are already recognized;
#' just, distant server is currently locked until we will build a better
#' security mechanism in the server (SSL, TSL, ...).
#'
#' All these operations are done by Komodo if you start R from Komodo with
#' SciViews-K extension installed.
#'
#' @seealso [koCmd()]
#' @keywords misc
#' @concept interprocess communication Komodo
#' @export
koInstall <- function() {
# PhG: this is a mechanisms we don't implement at the end!
#	assignTemp(".guiCmd", function(command, ...) {
#		## TODO: define these commands
#		command <- switch(command,
#			load = "",
#			source = "",
#			save = "",
#			import = "",
#			export = "",
#			report = "",
#			setwd = "",
#			"")
#	})
#	# FIXME: these sv.* functions do not exist in SciViews-K!
#	assignTemp(".guiObjBrowse", function(id, data) {
#		koCmd('sv.objBrowse("<<<id>>>", "<<<dat>>>");',
#		list(id = id, dat = data))
#	})
#	assignTemp(".guiObjInfo", function(id, data) {
#		koCmd('sv.objInfo("<<<id>>>", "<<<dat>>>");',
#		list(id = id, dat = data))
#	})
#	assignTemp(".guiObjMenu", function(id, data) {
#		koCmd('sv.objMenu("<<<id>>>", "<<<dat>>>");',
#		list(id = id, dat = data))
#	})
#
#	## Functions specific to Komodo as a GUI client
#	assignTemp(".koCmd", function(command, ...) {
#		## This mechanism avoids dependence on svGUI for packages that provide
#		## functionalities that work with or without Komodo (like svUnit)
#		## Instead of calling koCmd() directly, we look if .koCmd is defined
#		## in tempenv and we run it.
#		## This allows also redefining koCmd() without changing code in the
#		## packages that depend on .koCmd()
#		koCmd(command, ...)
#	})

  # Register a TaskCallback to generate automatically informations for an
  # object browser.
  h <- getTemp(".svTaskCallbackManager", default = NULL, mode = "list")
  if (!is.null(h))
    h$add(koAutoRefresh, name = "koAutoRefresh")
}

#' @rdname koInstall
#' @export
koUninstall <- function() {
  # PhG: this is a mechanisms we don't implement at the end...
  #	# Eliminate .guiCmd
  #	rmTemp(".guiCmd")
  #	rmTemp(".guiObjBrowse")
  #	rmTemp(".guiObjInfo")
  #	rmTemp(".guiObjMenu")
  #
  #	rmTemp(".koCmd")

  # Unregister our own TaskCallback
  h <- get_temp(".svTaskCallbackManager", default = NULL, mode = "list")
  if (!is.null(h))
    try(h$remove("koAutoRefresh"), silent = TRUE)
}

