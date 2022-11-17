library(leaflet)

shinyUI(pageWithSidebar(
    headerPanel("Polling Predictions and Election Results"),
    sidebarPanel(
        width = 2,
        splitLayout(
            actionButton("submit", "Submit",
                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            textInput("state2", "States abbr", value = "")
        ),
        selectInput("xsort", "Sort by",
                    choice   = c("(no sort)","AREA","DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2","DEM_SH","REP_SH","MAR_SH","TOT_SH","FLIP"),
                    selected = "(no sort"), 
        radioButtons("xsortdir", NULL, c("Ascending","Desc"), "Ascending", inline = TRUE),
        splitLayout(
            checkboxInput("showflips","Show flips",value = TRUE),
            textInput("xclose", "Close %", value = "0")
        ),
        checkboxInput("showall","Show all",value = FALSE),
        selectInput("measure", "Measure",
                    choices = c("Shift","Percent change","Percent ratio"),
                    selected = "Shift",
                    multiple = FALSE),
        selectInput("party", "Party",
                    choices = c("Democrat","Republican","Margin","Total"),
                    selected = "Margin",
                    multiple = FALSE),
        selectInput("units", "Units",
                    choices = c("Count","Percent"),
                    selected = "Percent",
                    multiple = FALSE),
        selectInput("racex", "Races, x & y axes",
                    choices = c("President_538","Senate_538","House_538","Governor_538","President","Senate","House","Governor","Registered"),
                    selected = "House_538",
                    multiple = FALSE),
        selectInput("racey", NULL,
                    choices = c("President_538","Senate_538","House_538","Governor_538","President","Senate","House","Governor","Registered"),
                    selected = "House",
                    multiple = FALSE),
        selectInput("model", "538 Model",
                    choices = c("_lite","_classic","_deluxe"),
                    selected = "_deluxe",
                    multiple = FALSE),
        splitLayout(
            numericInput("yearx", "Year, x-axis", 2022),
            numericInput("yeary", "Year, y-axis", 2022)
        ),
        textInput("tnote", "Title note", value = ""),
        selectInput("distype", "District type",
                    choices = c("2-party","1 & 2-party","Non-zero","All"),
                    selected = "2-party",
                    multiple = FALSE),
        textInput("fpop", "Min pop(k)", value = "0"),
        checkboxInput("fullstate","Show full state",value = FALSE),
        checkboxInput("dronly","Dem+Rep Only",value = FALSE),
        checkboxInput("createfiles","Create Data Files",value = FALSE),
        checkboxInput("writeoutput","Write Output",value = FALSE)
    ),
    mainPanel(
        tabsetPanel(id = "tabs",
            tabPanel("Output",
                mainPanel(
                    width = 12,
                    verbatimTextOutput("myText")
                )
            ),
            tabPanel("Plot",
                sidebarPanel(
                    width = 3,
                    splitLayout(
                      checkboxInput("showrow","Show row",value = FALSE),
                      checkboxInput("flipy","Flip y",value = FALSE)
                    ),
                    textInput("pos1", "Position above", value = ""),
                    textInput("pos3", "Position below", value = ""),
                    textInput("xscale", "X From,To,Step,Tick", value = ""),
                    textInput("yscale", "Y From,To,Step,Tick", value = ""),
                    textInput("xlimit","Limit",value = "-9,-3,3,9"),
                    textInput("xcolor","Color",value = "red3,orange,green3,violet,blue3"),
                    textInput("xparty","Party",value = "1_Solid R,2_Leans R,3_Toss-Up,4_Leans D,5_Solid D"),
                    selectInput("noparty", "No-party",
                                choices = c("Count as Dem","Split 50/50","Split by Ratio","Count as Rep"),
                                selected = "Split by Ratio",
                                multiple = FALSE),
                    textInput("vlimit","Vote Limit (1000s)",value = "1,10,100,1000"),
                    textInput("vshape","Vote Shape",value = "1,10,16,17,15"),
                    textInput("vdesc","Vote Desc",value = "< 1k,>=    1k,>=   10k,>=  100k,>= 1000k"),
                    checkboxInput("vuse2","Use Race2 Votes",value = FALSE),
                    splitLayout(
                        numericInput("plotload", "Load", 1),
                        actionButton("plotsave", "Save")
                    )
                ),
                mainPanel(
                    width = 9,
                    plotOutput("myPlot")
                )
            ),
            tabPanel("Plot2",
                     sidebarPanel(
                         width = 3,
                         splitLayout(
                             numericInput("minyear", "Min Year", 1976),
                             numericInput("maxyear", "Max Year", 2020)
                         ),
                         selectInput("races", "Races",
                                     choices = c("President","Senate","House","Governor"),
                                     selected = "President",
                                     multiple = TRUE),
                         textInput("state2_2", "State abbr(s)", value = "IA"),
                         textInput("district2", "District(s)", value = "1"),
                         textInput("xscale", "X From,To,Step,Tick", value = ""),
                         textInput("yscale", "Y From,To,Step,Tick", value = ""),
                         textInput("xlimit","Limit",value = "-9,-3,3,9"),
                         textInput("xcolor","Color",value = "red3,orange,green3,violet,blue3"),
                         textInput("xparty","Party",value = "1_Solid R,2_Leans R,3_Toss-Up,4_Leans D,5_Solid D"),
                         selectInput("noparty", "No-party",
                                     choices = c("Count as Dem","Split 50/50","Split by Ratio","Count as Rep"),
                                     selected = "Split by Ratio",
                                     multiple = FALSE),
                         textInput("vlimit","Vote Limit (1000s)",value = "1,10,100,1000"),
                         textInput("vshape","Vote Shape",value = "1,10,16,17,15"),
                         textInput("vdesc","Vote Desc",value = "< 1k,>=    1k,>=   10k,>=  100k,>= 1000k"),
                         checkboxInput("vuse2","Use Race2 Votes",value = FALSE),
                         splitLayout(
                             numericInput("plotload", "Load", 1),
                             actionButton("plotsave", "Save")
                         )
                     ),
                     mainPanel(
                         width = 9,
                         plotOutput("myPlot2")
                     )
            ),
            tabPanel(
                "Map",
                sidebarPanel(
                    width = 3,
                    selectInput("maplimitset", "Map Limits",
                                choices = c("Auto set to min,max",
                                            "Auto set balanced",
                                            "Use value(s) below"),
                                selected = "Auto set balanced",
                                multiple = FALSE),
                    textInput("maplimits", NULL, value = "-100,100"),
                    selectInput("mapvar", "Map Variable",
                                choices = c("DEM1","REP1","MARGIN1","TOTAL1",
                                            "DEM2","REP2","MARGIN2","TOTAL2",
                                            "DEM_SH","REP_SH","MAR_SH","TOT_SH",
                                            "FLIP","DEM1_N","REP1_N","MAR1_N",
                                            "TOT1_N"),
                                selected = "MAR_SH",
                                multiple = FALSE),
                    splitLayout(
                        numericInput("mapyear", "Map Year", 2020)
                    ),
                    textInput("mapcolors", "Map Colors", value = "RdBu"),
                    splitLayout(
                        numericInput("mapload", "Load", 1),
                        actionButton("mapsave", "Save")
                    )
                ),
                mainPanel(
                    width = 9,
                    leafletOutput("myggMap", height = 500, width = 800)
                )
            ),
            tabPanel("Data",
                sidebarPanel(
                    width = 3,
                    splitLayout(
                        numericInput("year_first", "First Year", 2020),
                        numericInput("year_last", "Last Year", 2012)
                    ),
                    numericInput("year_step", "Step Year", -2)
                ),
                mainPanel(
                    width = 9,
                    verbatimTextOutput("myVoteData")
                )
            ),
            tabPanel("Usage",
                     htmlOutput(outputId = "myUsage")
            )
        ),
        mainPanel(
            
        )
    )
))