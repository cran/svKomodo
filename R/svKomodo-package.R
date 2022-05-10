#' @details
#' Before RStudio or R addins for vscode were developed and popularized, this
#' was an attempt to develop a complete IDE for R by using Komodo IDE, see
#' https://www.activestate.com/products/komodo-ide/. The additional code needed
#' to supplement Komodo IDE with R-related features was implemented in two
#' parts: (1) The SciViews-K (https://github.com/SciViews/sciviewsk) addin for
#' Komodo and (2) the present {svKomodo} R package that work hand in hand to
#' provide R code intelligence, an object explorer, an integrated R Console, a
#' document format similar to R Markdown, but using ASCIIDoc instead of
#' Markdown, and more.
#'
#' The development of this extension for Komodo IDE was stopped in 2016 because
#' it was too similar to RStudio that emerged at that time as one of the
#' preferred editor/IDE for R. The SciViews IDE using Komodo never was enough
#' advertised, nor documented to gain a significant useR base.
#'
#' Nevertheless, the SciViews-K Komodo addin and the {svKomodo} R package
#' implemented features that may be interesting in a different context. The
#' interaction of R and Komodo and the remote R Console in Komodo use
#' communication protocols developed in the {svSocket} and {svHttp} R packages.
#' As such, it is an example of R -JavaScript code to interact with R in a
#' non-blocking way that could be reused in any other software that implements
#' its UI using HTML + JavaScript. The consequent JavaScript code developed for
#' the R object explorer may be worth looking at, for instance.
#'
#' Both SciViews-K and {svKomodo} should be considered archived items. I keep
#' them "alive" in the hope that their code could be useful to others in similar
#' contexts. See the other man pages for further explanations.
#' @keywords IO
#' @concept interprocess communication Komodo
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
#' @import svMisc
#' @importFrom utils browseURL capture.output loadhistory rc.settings
NULL
