# epivizgenestrack

A Epiviz Genes Track web component for Shiny apps, using the {shinywc} framework.

```
library(shiny)

jsondata <-
  "{
    \"rows\": {
        \"end\": [132206716.0, 606321.0, 902355.0, 111488.0, 194772.0, 72774.0, 23260.0, 5574.0, 6559.0, 45383.0, 71016.0, 35594.0],
        \"start\": [131240371.0, 1044504.0, 1425642.0, 74668.0, 153635.0, 83517.0, 72224.0, 23612.0, 5261.0, 22841.0, 55493.0, 46630.0],
        \"chr\": [\"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\", \"chr11\"],
        \"id\": null,
        \"strand\": [\"+\", \"-\", \"-\", \"-\", \"+\", \"-\", \"+\", \"-\", \"+\", \"+\", \"+\", \"-\"],
        \"metadata\": {
            \"exon_ends\": [\"131240783,131781542,132016408,132082041,132177717,132180126,132184597,132206716\", \"132290187,132306131,132306673,132307253,132399080,132527214,132813037\", \"133710818,133712000,133712457,133714232,133714535,133715085,133715392\", \"133788197,133789000,133791292,133792117,133792625,133794799,133795860,133796986,133799677,133801029,133801464,133801690,133802108,133805657,133806089,133807388,133807856,133814261,133816153,133826880\", \"133939054,134009811,134010665,134014288,134014889,134015940,134018571,134018713,134021652\", \"134022947,134023095,134023258,134026990,134027962,134028390,134029969,134031786,134038090,134038497,134038949,134046281,134047352,134048636,134048831,134051079,134054647,134054895,134055421,134062804,134064045,134064633,134072800,134073093,134073801,134074066,134074866,134076627,134078845,134079100,134079371,134080348,134086992,134090620,134094426\", \"134095239,134104947,134110049,134113188,134114974,134117686\", \"134118378,134118853,134119156,134119814,134120237,134121202,134122810,134123260\", \"134123603,134126542,134128518,134129819\", \"134146657,134147345,134147806,134151339,134152014,134153739,134158784,134162107,134163103,134175202\", \"134202041,134212845,134214349,134215023,134217327,134226287,134229035,134234292,134237235,134238675,134239779,134240293,134241042,134241386,134241724,134243702,134244150,134244612,134246218\", \"134250641,134251918,134252900,134254082,134257588,134257834,134281812\"],
            \"gene\": [\"NTM\", \"OPCML\", \"SPATA19\", \"IGSF9B\", \"JAM3\", \"NCAPD3\", \"VPS26B\", \"THYN1\", \"ACAD8\", \"GLB1L3\", \"GLB1L2\", \"B3GAT1\"],
            \"entrez\": [\"50863\", \"4978\", \"219938\", \"22997\", \"83700\", \"23310\", \"112936\", \"29087\", \"27034\", \"112937\", \"89944\", \"27087\"],
            \"exon_starts\": [\"131240371,131781458,132016176,132081916,132177583,132180006,132184446,132204940\", \"132284875,132305980,132306553,132307116,132398955,132526982,132812821\", \"133710517,133711925,133712380,133714141,133714404,133715029,133715264\", \"133785185,133788919,133789637,133792072,133792464,133794715,133795634,133796811,133799566,133800879,133801348,133801550,133801966,133805512,133805948,133807271,133807705,133814115,133815956,133826586\", \"133938820,134009746,134010552,134014136,134014687,134015841,134018442,134018659,134019041\", \"134022337,134023035,134023184,134026913,134027823,134028224,134029787,134031676,134037891,134038364,134038812,134046202,134047114,134048529,134048717,134050972,134054532,134054798,134055230,134062584,134063911,134064560,134072711,134073037,134073549,134073941,134074794,134076494,134078758,134079039,134079207,134080164,134086830,134090466,134093757\", \"134094561,134104791,134109885,134113013,134114832,134115338\", \"134118173,134118703,134119061,134119722,134120169,134121024,134122734,134123193\", \"134123434,134126442,134128409,134128902\", \"134146275,134147220,134147594,134151271,134151919,134153631,134158692,134162026,134163039,134175009\", \"134201768,134212648,134214281,134214928,134217219,134226195,134228954,134234222,134237151,134238538,134239699,134240187,134240902,134241315,134241646,134243615,134244039,134244496,134244866\", \"134248398,134251818,134252604,134253574,134257442,134257721,134281701\"]
        }
    }
  }"

test_ui <- function(id) {
  ns <- NS(id)
  tagList(
    epivizgenestrack::genes_track(
      id = ns("testtrack"),
      json_data = jsondata
    ),
    
    fluidRow(
      column(
        2,
        h3("Get attribute"),
        selectInput(ns("get_attr_id"), "Attribute", c("json_data", "chart_colors")),
        actionButton(ns("get_attr"), "Show attribute"),
      ),
      column(
        2,
        h3("Set attribute"),
        selectInput(ns("set_attr_id"), "Attribute", c("json_data", "chart_colors")),
        textInput(ns("set_attr_val"), "value", ""),
        actionButton(ns("set_attr"), "Set")
      ),
      column(
        2,
        h3("Get property"),
        selectInput(ns("get_prop_id"), "Property", c("chartColors", "chartSettings")),
        actionButton(ns("get_prop"), "Show property"),
      ),
      column(
        2,
        h3("Set property"),
        selectInput(ns("set_prop_id"), "Property", c("chartColors", "chartSettings")),
        textInput(ns("set_prop_val"), "Value", ""),
        selectInput(ns("set_prop_type"), "Property type", c("Plain text" = "text", "Array/Object (provide value in JSON format)" = "json")),
        actionButton(ns("set_prop"), "Set")
      ),
      column(
        2,
        h3("Call method"),
        selectInput(ns("method"), "Method", c("hostHovered", "hostUnhovered", "hover (doesn't work because requires a complex argument)", "unHover")),
        actionButton(ns("call"), "Go")
      ),
      column(
        2,
        h3("Events"),
        h4("dimChanged:"), textOutput(ns("event_dimChanged")),
        h4("hover:"), textOutput(ns("event_hover")),
        h4("unHover:"), textOutput(ns("event_unHover")
        )
      )
    )
  )
}

test_server <- function(input, output, session) {
  track <- epivizgenestrack::genes_track_proxy("testtrack")
  
  observeEvent(input$get_attr, {
    val <- track[[input$get_attr_id]]
    shinyalert::shinyalert(text = val)
  })
  
  observeEvent(input$set_attr, {
    track[[input$get_attr_id]] <- input$set_attr_val
  })
  
  observeEvent(input$get_prop, {
    fnx <- paste0("get_", input$get_prop_id, "_prop")
    track[[fnx]](function(x){ shinyalert::shinyalert(text = as.character(jsonlite::toJSON(x))) })
  })
  
  observeEvent(input$set_prop, {
    fnx <- paste0("set_", input$set_prop_id, "_prop")
    val <- input$set_prop_val
    if (input$set_prop_type == "json") {
      val <- jsonlite::fromJSON(val)
    }
    track[[fnx]](val)
  })
  
  observeEvent(input$call, {
    fnx <- paste0("call_", input$method)
    track[[fnx]]()
  })
  
  output$event_dimChanged <- renderText({
    req(track$event_dimChanged())
    paste(Sys.time(), track$event_dimChanged())
  })
  output$event_hover <- renderText({
    req(track$event_hover())
    paste(Sys.time(), track$event_hover())
  })
  output$event_unHover <- renderText({
    req(track$event_unHover())
    paste(Sys.time(), track$event_unHover())
  })
}

ui <- fluidPage(
  h1("<epiviz-genes-track> web component Shiny demo"),
  test_ui("test")
)

server <- function(input, output, session) {
  callModule(test_server, "test")
}

shinyApp(ui, server)
```
