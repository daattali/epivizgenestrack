#' @export
genes_track <- function(
  id = NULL,
  json_data = NULL,
  chart_colors = NULL,
  ...
) {

  dependencies <- list(
    shinywc::dependency_shinywc(),
    shinywc::dependency_jqueryui(),
    shinywc::dependency_webcomponentsjs(version = "1.1.0")
  )

  params <- as.list(environment())
  params_extra <- eval(substitute(alist(...)))

  shiny::addResourcePath("epiviz", system.file("webcomponent", package = "epivizgenestrack"))

  shiny::tagList(
    shinywc::shinywc_ui(
      tag = "epiviz-genes-track",
      params = params,
      params_extra = params_extra,
      attributes = list("json-data", "chart-colors"),
      required = list("json-data"),
      events = list("dimChanged", "hover", "unHover"),
      dependencies = dependencies
    ),
    htmltools::singleton(htmltools::tags$head(
      htmltools::tags$link(rel = "import", href = "epiviz/epiviz-components.html")
    ))
  )
}

Shinywc_genes_track <- R6::R6Class(
  "Shinywc_genes_track",
  inherit = shinywc::ShinywcProxy,

  public = list(

    initialize = function(id, session = shiny::getDefaultReactiveDomain()) {
      super$initialize(tag = "epiviz-genes-track", id = id, session = session)
    },

    event_dimChanged = function() {
      super$listen_event("dimChanged")
    },
    event_hover = function() {
      super$listen_event("hover")
    },
    event_unHover = function() {
      super$listen_event("unHover")
    },

    get_json_data = function() {
      super$get_attr("json-data")
    },
    set_json_data = function(value) {
      super$set_attr("json-data", value)
    },
    get_chart_colors = function() {
      super$get_attr("chart-colors")
    },
    set_chart_colors = function(value) {
      super$set_attr("chart-colors", value)
    },
    get_chartColors_prop = function(cb) {
      super$get_prop("chartColors", cb)
    },
    set_chartColors_prop = function(value) {
      super$set_prop("chartColors", value)
    },
    get_chartSettings_prop = function(cb) {
      super$get_prop("chartSettings", cb)
    },
    set_chartSettings_prop = function(value) {
      super$set_prop("chartSettings", value)
    },
    call_hostHovered = function() {
      super$call_method("hostHovered")
    },
    call_hostUnhovered = function() {
      super$call_method("hostUnhovered")
    },
    call_hover = function(data) {
      super$call_method("hover", list(data))
    },
    call_unHover = function() {
      super$call_method("unHover")
    }
  )

)

#' @export
genes_track_proxy <- function(id, session = shiny::getDefaultReactiveDomain()) {
  Shinywc_genes_track$new(id = id, session = session)
}
