#' RDataXMan: Data Extraction Management
#'
#' There are 5 functions in this package.
#'
#' Functions \code{\link{genInclusion}}, \code{\link{genVariable}} and
#' \code{\link{extract_data}} are the 3 main functions that generates inclusion
#' criteria and variable lists based on target databses, and extract subset of
#' data for user.
#'
#' Functions \code{\link{initWkdir}} and \code{\link{initResearchFolder}} are
#' helper functions to set working directory and subfolders for specific data
#' requests.
#'
#' \strong{RDataXMan Software Requirements for Apple Mac Computers}
#'
#' For installation of the software on Apple computers running OS X please
#' follow the steps before attempting to install RDataXMan
#' \enumerate{
#' \item Install the Oracle Java 8 JDK from https://www.oracle.com/technetwork/java/javase/downloads/index.html (filename: jdk-8uXXX-macosx-x64.dmg)
#' \item Open the terminal and type the following commands:
#'  \itemize{
#'    \item \code{sudo R CMD javareconf -n}
#'    \item \code{sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib}
#'  }
#' \item Install https://cran.r-project.org/bin/macosx/el-capitan/contrib/3.5/rJava_0.9-10.tgz
#' }
#'
#' \strong{Linking to Oracle database}
#'
#' The \code{RDataXMan} package supports linking to the Oracle database via
#' packages \code{ORE} or \code{ROracle}, which are not available from CRAN and
#' requires additional configuration. For more details, please refer to the
#' RDataXMan User Manual available from
#' \url{http://blog.nus.edu.sg/dasa/rdataxman/}.
#'
#' @docType package
#' @name RDataXMan-package
NULL
