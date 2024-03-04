
# Module documentation ----------------------------------------------------

# This is the user-facing documentation.

#' Interactive Module for Inter-Rater Reliability to False Positive Rate Conversion
#'
#' This module allows users to convert inter-rater reliability (IRR) to false positive rate (FPR)
#' as described in Bartoš and Martinková (2023).
#'
#' @author
#' František Bartoš
#'
#' @name IRR2FPR
#' @family SIAmodules
#'
#' @references Bartoš, F., & Martinková, P. (2022). Selecting applicants based on multiple ratings:
#' Using binary classification framework as an alternative to inter-rater reliability. \doi{10.48550/arXiv.2207.09101}
#'
NULL


# Module definition -------------------------------------------------------

## UI part ----------------------------------------------------------------

#' `IRR2FPR` module (internal documentation)
#'
#' This is the internal documentation of your module that is not included in the
#' help index of the package. You may include your notes here. For a
#' [user-facing help page][IRR2FPR], please edit the documentation
#' above.
#'
#' Note that even being internal, a curious user can still discover this
#' internal help page. To prevent that, include the `@noRd` `{roxygen2}` tag
#' below the line with `@keywords` tag.
#'
#' If your module uses any external packages, such as ggplot2,
#' **you have to declare the imports** with the `@importFrom` tag and include
#' the package in the DESCRIPTION. See
#' <https://r-pkgs.org/dependencies-in-practice.html> for more details.
#'
#' You can preview your module using
#' `SIAtools::preview_module("IRR2FPR")`.
#'
#' See `vignette("developing_modules", "SIAtools")` vignette for further details.
#'
#' @param id *character*, the ID assigned by ShinyItemAnalysis. **Do not set any
#'   default value!**
#' @param imports *list*, reactive objects exported by ShinyItemAnalysis. See
#'   `vignette("imports", "SIAtools")` for more details on how to use objects
#'   from the ShinyItemAnalysis app.
#' @param ... additional parameters (not used at the moment).
#'
#' @keywords internal
#' @rdname IRR2FPR_internal
#'
#' @import shiny
#'
sm_new_module_ui <- function(id, imports = NULL, ...) {
  ns <- NS(id) # shorthand for NS(id, <inputId>)
  # Any `inputId` and `outputId` of {shiny} UI elements MUST be "wrapped" in `ns()` call!

  tagList(
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    h3("Inter-Rater Reliability to False Positive Rate"),

    # description -------------------------------------------------------------

    p(
      "This module transforms the inter-rater reliability into the false-positive rate and
      other binary classification metrics. Details of the transformation are described in ",
      a(
        "Baroš and Martinková (2023)",
        href = "http://doi.org/10.48550/arXiv.2207.09101",
        target = "_blank", .noWS = "after"
      ), ". We use the NIH data set of Erosheva et al. (2021) for presentation."
    ),
    p(
      "Below, you may select the IRR and the proportion of selected candidates in the selection procedure.
      The left plot illustrates the variability in ratings for the whole dataset outshading the data which
      would be lost due to range-restriction. The right plot provides the estimates
      of the calculated inter-rater reliability estimates, defined by intraclass
      corelation in the simplest model including the ratee effect only. The estimates
      are accompanied by a bootstrapped 95% confidence interval based on 25 bootstrap samples.",
      class = "mb-5"
    ),


    # UI ----------------------------------------------------------------------

    fluidRow(
      column(
        2,
        sliderInput(
          inputId = ns("irr"),
          label = "IRR",
          min = 0,
          max = 1,
          step = 0.01,
          value = 0.34,
          animate = animationOptions(2000),
          post = "", width = "100%"
        ),
        sliderInput(
          inputId = ns("proportion_selected"),
          label = "Proportion",
          min = 0,
          max = 100,
          step = 1,
          value = 18,
          animate = animationOptions(2000),
          post = "%", width = "100%"
        )
      )
    )
   # fluidRow(
   #   column(6, plotlyOutput(ns("reliability_restricted_caterpillarplot"))),
   #   column(6, plotlyOutput(ns("reliability_restricted_iccplot"))),
   #   style = "padding-bottom: 20px;"
   # ),

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  )
}

## Server part ------------------------------------------------------------

#' @rdname sm_new_module_internal
#'
sm_new_module_server <- function(id, imports = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    # YOUR CODE FOR THE SERVER LOGIC GOES HERE
    # Please edit only the part inside the dashed lines.

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  })
}
