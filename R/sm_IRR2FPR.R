
# Module documentation ----------------------------------------------------

# This is the user-facing documentation.

#' @title Interactive Module for Inter-Rater Reliability to False Positive Rate Conversion
#'
#' @description This module allows users to convert inter-rater reliability (IRR) to false positive rate (FPR)
#' as described in Bartoš and Martinková (2022).
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

#' @title `IRR2FPR` module (internal documentation)
#'
#' @description This is the internal documentation of your module that is not included in the
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
#' @name IRR2FPR_internal
#'
#' @import shiny
NULL

#' @rdname IRR2FPR_internal
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
        "Barto\u0161 and Martinkov\u00e1 (2022)",
        href = "http://doi.org/10.48550/arXiv.2207.09101",
        target = "_blank", .noWS = "after"
      ), ". We use the inter-rater reliability and the proportion of selected candidates
      from NIH data set of ",
      a(
        "Erosheva et al. (2021)",
        href = "http://doi.org/10.1111/rssa.12681",
        target = "_blank", .noWS = "after"
      ), " as default values."
    ),
    p(
      "Below, you may change the IRR and the proportion of selected candidates in the selection procedure.
      Plot A visualizes the true positive rate of the selection procedure (which also corresponds
      to the F1 score). Plot B visualizes the false positive rate, and the plot C visualizes the
      false negative rate of the selection procedure. The thin line visualizes the corresponding
      metrics (y-axis) across the whole range of the proportion of selected candidates (x-axis), and the full point
      highlights the given estimate for the currently specified proportion of selected candidates.",
      class = "mb-5"
    ),


    # UI ----------------------------------------------------------------------

    fluidRow(
      column(
        6,
        sliderInput(
          inputId = ns("irr"),
          label   = "IRR",
          min     = 0.01,
          max     = 1,
          step    = 0.01,
          value   = 0.34,
          animate = animationOptions(2000),
          post    = "",
          width   = "50%"
        ),
        sliderInput(
          inputId = ns("proportion_selected"),
          label   = "Proportion Selected",
          min     = 1,
          max     = 99,
          step    = 1,
          value   = 18,
          animate = animationOptions(2000),
          post    = "%",
          width   = "50%"
        )
      )
    ),


    fluidRow(
      column(4, plotOutput(ns("TPRplot"))),
      column(4, plotOutput(ns("FPRplot"))),
      column(4, plotOutput(ns("FNRplot"))),
      style = "padding-bottom: 20px;"
    ),

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   # sample R code -----------------------------------------------------------

   h4("Selected R code"),
   tags$pre(includeText(system.file("sc/IRR2FPR.R", package = "SIAModuleIRR2FPR")),
            class = "language-r mb-4"
   ),


    # references --------------------------------------------------------
    h4("References"),
    HTML('<ul class = "biblio">

               <li>Barto\u0161, F., & Martinkov\u00e1, P. (2022).
                Selecting applicants based on multiple ratings: Using binary classification framework as an alternative to inter-rater reliability
               <a href = "https://doi.org/10.48550/arXiv.2207.09101", target = "_blank">doi:10.48550/arXiv.2207.09101</a>
               </li>

               <li>Erosheva, E., Martinkov\u00e1, P., & Lee, C. (2021).
                When zero may not be zero: A cautionary note on the use of inter-rater
                reliability in evaluating grant peer review. Journal of the Royal Statistical Society - Series A,
                184(3), 904-919.
               <a href = "https://doi.org/10.1111/rssa.12681", target = "_blank">doi:10.1111/rssa.12681</a>
               </li>
             </ul>')

  )
}

## Server part ------------------------------------------------------------

#' @rdname IRR2FPR_internal
sm_new_module_server <- function(id, imports = NULL, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


    output$TPRplot <- renderPlot({

      graphics::par(mar=c(4,4,0.1, 0.1))
      graphics::plot(NA, type = "n", axes = TRUE, bty = "n", xlab = "Proportion selected", ylab = "True positive rate", xlim = c(0, 1), ylim = c(0, 1), las = 1)
      x_seq <- seq(0, 1, 0.01)

      graphics::lines(x_seq, compute_true_positive_rate(IRR = input$irr, proportion_selected = x_seq), lwd = 2)
      graphics::points(input$proportion_selected/100, compute_true_positive_rate(IRR = input$irr, proportion_selected = input$proportion_selected/100), pch = 16, cex = 1.5)

      graphics::mtext("A", line = -2, side = 3, font = 2, at = 0, cex = 2)
      graphics::mtext(paste0("TPR = ", round(compute_true_positive_rate(IRR = input$irr, proportion_selected = input$proportion_selected/100), 2)), line = -2, side = 1, font = 2, at = 1, cex = 1, adj = 1)

    })

    output$FPRplot <- renderPlot({

      graphics::par(mar=c(4,4,0.1, 0.1))
      graphics::plot(NA, type = "n", axes = TRUE, bty = "n", xlab = "Proportion selected", ylab = "False positive rate", xlim = c(0, 1), ylim = c(0, 1), las = 1)
      x_seq <- seq(0, 1, 0.01)

      graphics::lines(x_seq, compute_false_positive_rate(IRR = input$irr, proportion_selected = x_seq), lwd = 2)
      graphics::points(input$proportion_selected/100, compute_false_positive_rate(IRR = input$irr, proportion_selected = input$proportion_selected/100), pch = 16, cex = 1.5)

      graphics::mtext("B", line = -2, side = 3, font = 2, at = 0, cex = 2)
      graphics::mtext(paste0("FPR = ", round(compute_false_positive_rate(IRR = input$irr, proportion_selected = input$proportion_selected/100), 2)), line = -2, side = 1, font = 2, at = 0, cex = 1, adj = 0)

    })

    output$FNRplot <- renderPlot({

      graphics::par(mar=c(4,4,0.1, 0.1))
      graphics::plot(NA, type = "n", axes = TRUE, bty = "n", xlab = "Proportion selected", ylab = "False negative rate", xlim = c(0, 1), ylim = c(0, 1), las = 1)
      x_seq <- seq(0, 1, 0.01)

      graphics::lines(x_seq, compute_false_negative_rate(IRR = input$irr, proportion_selected = x_seq), lwd = 2)
      graphics::points(input$proportion_selected/100, compute_false_negative_rate(IRR = input$irr, proportion_selected = input$proportion_selected/100), pch = 16, cex = 1.5)

      graphics::mtext("C", line = -2, side = 3, font = 2, at = 0, cex = 2)
      graphics::mtext(paste0("FNR = ", round(compute_false_negative_rate(IRR = input$irr, proportion_selected = input$proportion_selected/100), 2)), line = -2, side = 1, font = 2, at = 1, cex = 1, adj = 1)
    })

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  })
}
