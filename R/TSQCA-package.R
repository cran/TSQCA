#' @keywords internal
"_PACKAGE"

#' TSQCA: Threshold-Sweep Extensions for Qualitative Comparative Analysis
#'
#' @description
#' The TSQCA package provides a systematic framework for analyzing threshold 
#' dependency in Qualitative Comparative Analysis (QCA). Rather than relying 
#' on a single calibration threshold, TSQCA systematically explores how 
#' sufficient conditions change across different threshold levels, revealing 
#' the dynamic structure of causal configurations.
#'
#' @details
#' ## Overview
#' 
#' TSQCA extends existing QCA methodology by transforming the calibration 
#' stage from a fixed prerequisite into an analytical object. The package 
#' utilizes existing QCA packages (particularly the \pkg{QCA} package's 
#' \code{truthTable()} and \code{minimize()} functions) internally, and 
#' focuses on the systematic exploration of threshold parameter space.
#' 
#' ## Core Philosophy
#' 
#' Traditional QCA assumes researchers select a single calibration threshold 
#' exogenously and extract sufficient conditions based on that threshold. 
#' TSQCA restructures this process by:
#' 
#' \itemize{
#'   \item Executing QCA repeatedly across multiple threshold values
#'   \item Systematically analyzing changes in obtained solutions
#'   \item Extracting solution stability, critical points, and hierarchical 
#'         causal structures
#' }
#' 
#' This makes TSQCA a complementary meta-analytical framework that sits 
#' above existing QCA, not a replacement for it.
#' 
#' ## Four Core Methods
#' 
#' \describe{
#'   \item{\strong{OTS-QCA} (Outcome Threshold Sweep)}{
#'     Varies the outcome threshold (e.g., Y >= 6, 7, 8, 9) to identify 
#'     how sufficient conditions change with different target levels of 
#'     the outcome.
#'   }
#'   \item{\strong{CTS-QCA} (Condition Threshold Sweep)}{
#'     Varies a single condition's threshold (e.g., X >= 5, 6, 7, 8) to 
#'     identify the "onset point" where the condition begins to demonstrate 
#'     causal efficacy.
#'   }
#'   \item{\strong{MCTS-QCA} (Multi-dimensional Condition Threshold Sweep)}{
#'     Simultaneously explores threshold combinations across multiple 
#'     conditions (Cartesian product space), visualizing regions of stable 
#'     solutions and critical boundaries where causal structures shift.
#'   }
#'   \item{\strong{DTS-QCA} (Dual Threshold Sweep)}{
#'     Simultaneously varies both outcome and condition thresholds in a 
#'     two-dimensional sweep, enabling analysis of how target outcome 
#'     levels and condition improvement levels interact.
#'   }
#' }
#' 
#' ## Key Advantages
#' 
#' \itemize{
#'   \item \strong{Addresses Threshold Dependency}: Makes calibration 
#'         uncertainty explicit rather than hidden
#'   \item \strong{Reveals Hierarchical Causality}: Identifies how 
#'         conditions operate at different threshold levels
#'   \item \strong{Detects Critical Points}: Locates threshold tipping 
#'         points where causal structures transform
#'   \item \strong{Enhances Robustness}: Tests solution stability across 
#'         threshold ranges
#'   \item \strong{Supports Theory Building}: Generates theoretical 
#'         insights from threshold variation patterns
#' }
#' 
#' ## Relationship with QCA Package
#' 
#' TSQCA is built on top of the \pkg{QCA} package (Duşa, 2024). All threshold 
#' sweep functions use \code{\link[QCA]{truthTable}} and \code{\link[QCA]{minimize}} 
#' internally. Function arguments such as \code{incl.cut}, \code{n.cut}, 
#' \code{pri.cut}, \code{include}, and \code{dir.exp} follow QCA package 
#' conventions. For detailed explanations of these parameters, please refer to 
#' the QCA package documentation:
#' 
#' \itemize{
#'   \item \code{\link[QCA]{truthTable}} - For threshold and frequency cutoffs 
#'         (\code{incl.cut}, \code{n.cut}, \code{pri.cut})
#'   \item \code{\link[QCA]{minimize}} - For minimization parameters 
#'         (\code{include}, \code{dir.exp})
#' }
#' 
#' This design ensures:
#' 
#' \itemize{
#'   \item \strong{QCA package's role}: Truth table generation, logical 
#'         minimization, consistency/coverage calculation
#'   \item \strong{TSQCA's role}: Systematic threshold exploration, 
#'         stability analysis, critical point detection
#'   \item \strong{Integration}: TSQCA calls QCA functions internally; 
#'         it does not reimplement core QCA algorithms
#'   \item \strong{Compatibility}: Works seamlessly with established QCA 
#'         workflows while adding new analytical capabilities
#' }
#' 
#' ## Typical Workflow
#' 
#' \enumerate{
#'   \item Prepare data with continuous or ordinal variables
#'   \item Define threshold sequences for conditions and/or outcomes
#'   \item Apply appropriate sweep method (OTS/CTS/MCTS/DTS)
#'   \item Analyze threshold-dependent solution changes
#'   \item Visualize stability regions and critical transitions
#'   \item Interpret hierarchical causal structures
#' }
#' 
#' ## Application Domains
#' 
#' TSQCA is particularly valuable in fields where:
#' 
#' \itemize{
#'   \item Continuous indicators are common (marketing, organizational research)
#'   \item Threshold choices lack strong theoretical foundation
#'   \item Exploratory theory building is the goal
#'   \item KPI-level thresholds have practical significance
#'   \item Solution robustness is critical
#' }
#'
#' @section Main Functions:
#' 
#' \describe{
#'   \item{\code{\link{otSweep}}}{Execute OTS-QCA (Outcome Threshold Sweep)}
#'   \item{\code{\link{ctSweepS}}}{Execute CTS-QCA (Condition Threshold Sweep)}
#'   \item{\code{\link{ctSweepM}}}{Execute MCTS-QCA (Multi-dimensional CTS)}
#'   \item{\code{\link{dtSweep}}}{Execute DTS-QCA (Dual Threshold Sweep)}
#' }
#'
#' @references 
#' Ragin, C. C. (2008). \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond}. 
#' Chicago: University of Chicago Press.
#' 
#' Dusa, A. (2024). \emph{QCA: Qualitative Comparative Analysis}. 
#' R package version 3.22. \url{https://CRAN.R-project.org/package=QCA}
#'
#' @seealso 
#' \itemize{
#'   \item GitHub repository: \url{https://github.com/im-research-yt/TSQCA}
#'   \item Bug reports: \url{https://github.com/im-research-yt/TSQCA/issues}
#'   \item \pkg{QCA} package for standard QCA analysis
#' }
#' 
#' @examples
#' \dontrun{
#' # Load package
#' library(TSQCA)
#' 
#' # Example: OTS-QCA with outcome threshold sweep
#' result_ots <- otSweep(
#'   dat = sample_data,
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2", "X3"),
#'   sweep_range = 6:9,
#'   thrX = c(X1 = 7, X2 = 7, X3 = 7)
#' )
#' 
#' # Example: CTS-QCA with single condition threshold sweep
#' result_cts <- ctSweepS(
#'   dat = sample_data,
#'   Yvar = "Y",
#'   Xvars = c("X1", "X2", "X3"),
#'   sweep_var = "X1",
#'   sweep_range = 5:8,
#'   thrY = 7,
#'   thrX_default = 7
#' )
#' 
#' # See vignettes for detailed tutorials
#' vignette("TSQCA_Tutorial_EN", package = "TSQCA")
#' }
#'

