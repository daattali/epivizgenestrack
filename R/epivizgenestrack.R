#' @export
epivizGenesTrackUI <- function(
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

#' @export
EpivizGenesTrack <- R6::R6Class(
  "EpivizGenesTrack",

  private = list(
    .id = NULL,
    .id_noNS = NULL,
    .session = NULL,
    .attributes = list(),

    set_attr = function(attr, value) {
      private$.session$sendCustomMessage('shinywc-attr-set', list(
        id = private$.id,
        attr = attr,
        value = value
      ))
    },

    get_attr = function(attr) {
      if (!attr %in% names(private$.attributes)) {
        return(NULL)
      }
      private$.attributes[[attr]]
    },

    set_prop = function(prop, value) {
      private$.session$sendCustomMessage('shinywc-prop-set', list(
        id = private$.id,
        prop = prop,
        value = value
      ))
    },

    get_prop = function(prop, cb) {
      cbid_noNS <- paste0("__epiviz-genes-track-", prop, "-", sample(1e9, 1))
      cbid <- private$.session$ns(cbid_noNS)
      private$.session$sendCustomMessage('shinywc-prop-get', list(
        id = private$.id,
        prop = prop,
        cbid = cbid
      ))
      shiny::observeEvent(private$.session$input[[cbid_noNS]], once = TRUE, {
        cb(private$.session$input[[cbid_noNS]])
      })
    },

    call_method = function(method, params = list()) {
      private$.session$sendCustomMessage('shinywc-call-method', list(
        id = private$.id,
        method = method,
        params = params
      ))
    }
  ),

  public = list(

    initialize = function(id, session = shiny::getDefaultReactiveDomain()) {
      if (is.null(session)) {
        stop("EpivizGenesTrack can only be initialized in a Shiny environment")
      }
      private$.session <- session
      private$.id_noNS <- id
      private$.id <- private$.session$ns(private$.id_noNS)

      private$.session$sendCustomMessage('shinywc-init-component', list(
        id = private$.id
      ))

      shiny::observeEvent(private$.session$input[[paste0(private$.id_noNS, "_epiviz-genes-track-attr-change")]], {
        evt <- private$.session$input[[paste0(private$.id_noNS, "_epiviz-genes-track-attr-change")]]
        attr_name <- names(evt[1])
        attr_val <- evt[[1]]
        private$.attributes[[attr_name]] <- attr_val
      })
    },

    id = function() {
      private$.id_noNS
    },

    event_dimChanged = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_dimChanged")]]
    },
    event_hover = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_hover")]]
    },
    event_unHover = function() {
      private$.session$input[[paste0(private$.id_noNS, "_event_unHover")]]
    },

    get_json_data = function() {
      private$get_attr("json-data")
    },
    set_json_data = function(value) {
      private$set_attr("json-data", value)
    },
    get_chart_colors = function() {
      private$get_attr("chart-colors")
    },
    set_chart_colors = function(value) {
      private$set_attr("chart-colors", value)
    },
    get_chartColors_prop = function(cb) {
      private$get_prop("chartColors", cb)
    },
    set_chartColors_prop = function(value) {
      private$set_prop("chartColors", value)
    },
    get_chartSettings_prop = function(cb) {
      private$get_prop("chartSettings", cb)
    },
    set_chartSettings_prop = function(value) {
      private$set_prop("chartSettings", value)
    },
    call_hostHovered = function() {
      private$call_method("hostHovered")
    },
    call_hostUnhovered = function() {
      private$call_method("hostUnhovered")
    },
    call_hover = function(data) {
      private$call_method("hover", list(data))
    },
    call_unHover = function() {
      private$call_method("unHover")
    }
  )

)
