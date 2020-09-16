
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Scales and modes"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Scales", tabName = "allscales"),
            menuItem("Modes", tabName = "widgets")
        )
    ),

        dashboardBody(
            
            tags$head(tags$style(HTML('
      .main-sidebar {
        font-family: "Computer Modern", Times, "Times New Roman", serif;
        font-size: 20px;
      }
    '))),
            
            tags$head(tags$style(HTML('
      .main-body {
        font-family: "Computer Modern", Times, "Times New Roman", serif;
        font-size: 20px;
      }
    '))),
            
            
            tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Computer Modern", Times, "Times New Roman", serif;
        font-size: 24px;
      }
    '))),
        
        tabItems(
            # First tab content
            tabItem(tabName = "allscales",
                    fluidRow(
                        box(title="Select the keynote and the scale",
                            background = "light-blue", solidHeader = TRUE,
                            selectInput("knt", "Keynote:",
                                        c("C" = "cs",
                                          "D" = "ds",
                                          "E" = "es",
                                          "F" = "fs",
                                          "G" = "gs",
                                          "A" = "as",
                                          "B" = "bs")),
                            
                            radioButtons("scl", "Scale:",
                                         c("Major" = "mj",
                                           "Natural minor" = "nm",
                                           "Harmonic minor" = "hm",
                                           "Melodic minor" = "mm"))),
                        
                        box(
                            title = "Notes that belong to scale", status = "primary",
                            solidHeader=TRUE,
                            textOutput("notes")
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                
                    fluidRow(
                        box(title="Select the keynote and the mode",
                            background = "light-blue", solidHeader = TRUE,
                            selectInput("key", "Keynote:",
                                        c("C" = "cs",
                                          "D" = "ds",
                                          "E" = "es",
                                          "F" = "fs",
                                          "G" = "gs",
                                          "A" = "as",
                                          "B" = "bs")),
                            
                            radioButtons( "md", "Mode:",
                                         c("Ionian" = "io",
                                           "Dorian" = "do",
                                           "Phrygian" = "ph",
                                           "Lydian" = "ly",
                                           "Mixolydian" = "mi",
                                           "Aeolian" = "ae",
                                           "Locrian" = "lo"))),
                        
                        box(
                            title = "Notes that belong to the mode", status = "primary",
                            solidHeader=TRUE,
                            textOutput("oikein")
                        )
                    )
            )
        )
    
    )
)

server <- function(input, output) {

    

    kn <-reactive({input$knt})
    sc <-  reactive({input$scl})
    
    
    output$notes <- renderText({
        if ((kn()=="cs")&& (sc()=="mj")) {
            "C, D, E, F, G, A, B"
        }else if ((kn()=="ds")&& (sc()=="mj")) {
            "D, E, F#, G, A, B, C#"
        }else if ((kn()=="es")&& (sc()=="mj")) {
            "E, F#, G#, A, B, C#, D#"
        }else if ((kn()=="fs")&& (sc()=="mj")) {
            "F, G, A, Bb, C, D, E"
        }else if ((kn()=="gs")&& (sc()=="mj")) {
            "G, A, B, C, D, E, F???"
        }else if ((kn()=="as")&& (sc()=="mj")) {
            "A, B, C#, D, E, F#, G#"
        }else if ((kn()=="bs")&& (sc()=="mj")) {
            "B, C#, D#, E, F#, G#, A#"
        }else if ((kn()=="cs")&& (sc()=="nm")) {
            "C, D, Eb, F, G, Ab, Bb"
        }else if ((kn()=="ds")&& (sc()=="nm")) {
            "D, E, F, G, A, Bb, C"
        }else if ((kn()=="es")&& (sc()=="nm")) {
            "E, F#, G, A, B, C, D"
        }else if ((kn()=="fs")&& (sc()=="nm")) {
            "F, G, Ab, Bb, C, Db, Eb"
        }else if ((kn()=="gs")&& (sc()=="nm")) {
            "G, A, Bb, C, D, Eb, F"
        }else if ((kn()=="as")&& (sc()=="nm")) {
            "A, B, C, D, E, F, G"
        }else if ((kn()=="bs")&& (sc()=="nm")) {
            "B, C#, D, E, F#, G, A"
            
        }
        
        else if ((kn()=="cs")&& (sc()=="hm")) {
            "C, D, Eb, F, G, Ab, B"
        }else if ((kn()=="ds")&& (sc()=="hm")) {
            "D, E, F, G, A, Bb, C#"
        }else if ((kn()=="es")&& (sc()=="hm")) {
            "E, F#, G, A, B, C, D#"
        }else if ((kn()=="fs")&& (sc()=="hm")) {
            "F, G, Ab, Bb, C, Db, E"
        }else if ((kn()=="gs")&& (sc()=="hm")) {
            "G, A, Bb, C, D, Eb, F#"
        }else if ((kn()=="as")&& (sc()=="hm")) {
            "A, B, C, D, E, F, G#"
        }else if ((kn()=="bs")&& (sc()=="hm")) {
            "B, C#, D, E, F#, G, A#"
        }
        
        else if ((kn()=="cs")&& (sc()=="mm")) {
            "C, D, Eb, F, G, A, B, C, Bb, Ab, G, F, Eb, D, C"
        }else if ((kn()=="ds")&& (sc()=="mm")) {
            "D, E, F, G, A, B, C#, D, C, Bb, A, G, F, E, D"
        }else if ((kn()=="es")&& (sc()=="mm")) {
            "E, F#, G, A, B, C#, D#, E, D, C, B, A, G, F#, E"
        }else if ((kn()=="fs")&& (sc()=="mm")) {
            "F, G, Ab, Bb, C, D, E, F, Eb, Db, C, Bb, Ab, G, F"
        }else if ((kn()=="gs")&& (sc()=="mm")) {
            "G, A, Bb, C, D, E, F#, G, F, Eb, D, C, Bb, A, G"
        }else if ((kn()=="as")&& (sc()=="mm")) {
            "A, B, C, D, E, F#, G#, A, G, F, E, D, C, B, A"
        }else if ((kn()=="bs")&& (sc()=="mm")) {
            "B, C#, D, E, F#, G#, A#, B, A, G, F#, E, D, C#, B"
        }else {
            "Error"
        }
      
    })
    
    
    
    key <-reactive({input$key})
    md <-  reactive({input$md})
    
    
    output$oikein <- renderText({if ((key()=="cs")&& (md()=="io")) {
      "C, D, E, F, G, A, B"
    }else if ((key()=="ds")&& (md()=="io")) {
      "D, E, F#, G, A, B, C#"
    }else if ((key()=="es")&& (md()=="io")) {
      "E, F#, G#, A, B, C#, D#"
    }else if ((key()=="fs")&& (md()=="io")) {
      "F, G, A, Bb, C, D, E"
    }else if ((key()=="gs")&& (md()=="io")) {
      "G, A, B, C, D, E, F#"
    }else if ((key()=="as")&& (md()=="io")) {
      "A, B, C#, D, E, F#, G#"
    }else if ((key()=="bs")&& (md()=="io")) {
      "B, C#, D#, E, F#, G#, A#"
    }else if ((key()=="cs")&& (md()=="do")) {
      "C, D, Eb, F, G, A, Bb"
    }else if ((key()=="ds")&& (md()=="do")) {
      "D, E, F, G, A, B, C"
    }else if ((key()=="es")&& (md()=="do")) {
      "E, F#, G, A, B, C#"
    }else if ((key()=="fs")&& (md()=="do")) {
      "F, G, Ab, Bb, C, D, Eb"
    }else if ((key()=="gs")&& (md()=="do")) {
      "G, A, Bb, C, D, E, F"
    }else if ((key()=="as")&& (md()=="do")) {
      "A, B, C, D, E, F#, G"
    }else if ((key()=="bs")&& (md()=="do")) {
      "B, C#, D, E, F#, G#, A"
    }else if ((key()=="cs")&& (md()=="ph")) {
      "C, Db, Eb, F, G, Ab, Bb"
    }else if ((key()=="ds")&& (md()=="ph")) {
      "D, Eb, F, G, A, Bb, C"
    }else if ((key()=="es")&& (md()=="ph")) {
      "E, F, G, A, B, C, D"
    }else if ((key()=="fs")&& (md()=="ph")) {
      "F, Gb, Ab, Bb, C, Db, Eb"
    }else if ((key()=="gs")&& (md()=="ph")) {
      "G, Ab, Bb, C, D, Eb, F"
    }else if ((key()=="as")&& (md()=="ph")) {
      "A, Bb, C, D, E, F, G"
    }else if ((key()=="bs")&& (md()=="ph")) {
      "B, C, D, E, F#, G, A"
    }else if ((key()=="cs")&& (md()=="ly")) {
      "C, D, E, F#, G, A, B"
    }else if ((key()=="ds")&& (md()=="ly")) {
      "D, E, F#, G#, A, B, C#"
    }else if ((key()=="es")&& (md()=="ly")) {
      "E, F#, G#, A#, B, C#, D#"
    }else if ((key()=="fs")&& (md()=="ly")) {
      "F, G, A, B, C, D, E"
    }else if ((key()=="gs")&& (md()=="ly")) {
      "G, A, B, C#, D, E, F#"
    }else if ((key()=="as")&& (md()=="ly")) {
      "A, B, C#, D#, E, F#, G#"
    }else if ((key()=="bs")&& (md()=="ly")) {
      "B, C#, D#, E#, F#, G#, A#"
    }else if ((key()=="cs")&& (md()=="mi")) {
      "C, D, E, F, G, A, Bb"
    }else if ((key()=="ds")&& (md()=="mi")) {
      "D, E, F#, G, A, B"
    }else if ((key()=="es")&& (md()=="mi")) {
      "E, F#, G#, A, B, C#"
    }else if ((key()=="fs")&& (md()=="mi")) {
      "F, G, A, Bb, C, D, Eb"
    }else if ((key()=="gs")&& (md()=="mi")) {
      "G, A, B, C, D, E, F"
    }else if ((key()=="as")&& (md()=="mi")) {
      "A, B, C#, D, E, F#, G"
    }else if ((key()=="bs")&& (md()=="mi")) {
      "B, C#, D#, E, F#, G#, A"
    }else if ((key()=="cs")&& (md()=="ae")) {
      "C, D, Eb, F, G, Ab, Bb"
    }else if ((key()=="ds")&& (md()=="ae")) {
      "D, E, F, G, A, Bb, C"
    }else if ((key()=="es")&& (md()=="ae")) {
      "E, F#, G, A, B, C, D"
    }else if ((key()=="fs")&& (md()=="ae")) {
      "F, G, Ab, Bb, C, Db, Eb"
    }else if ((key()=="gs")&& (md()=="ae")) {
      "G, A, Bb, C, D, Eb, F"
    }else if ((key()=="as")&& (md()=="ae")) {
      "A, B, C, D, E, F, G"
    }else if ((key()=="bs")&& (md()=="ae")) {
      "B, C#, D, E, F#, G, A"
    }else if ((key()=="cs")&& (md()=="lo")) {
      "C, Db, Eb, F, Gb, Ab, Bb"
    }else if ((key()=="ds")&& (md()=="lo")) {
      "D, Eb, F, G, Ab, Bb, C"
    }else if ((key()=="es")&& (md()=="lo")) {
      "E, F, G, A, Bb, C, D"
    }else if ((key()=="fs")&& (md()=="lo")) {
      "F, Gb, Ab, Bb, C, Db, Eb"
    }else if ((key()=="gs")&& (md()=="lo")) {
      "G, Ab, Bb, C, Db, Eb, F"
    }else if ((key()=="as")&& (md()=="lo")) {
      "A, Bb, C, D, E, F, G"
    }else if ((key()=="bs")&& (md()=="lo")) {
      "B, C, D, E, F, G, A"
    }
      })
      
      
      
    
    

    
    
    
    
}

shinyApp(ui, server)