# Load packages
library(shiny)
library(shinythemes)
library(DT)


load("oral_prep_final_model_ensemble_ft_eng_v3.RData")

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("Consistently PrEPed",
                           tabPanel("Information Capture",
                                    sidebarPanel(
                                      tags$h3("Please answer the following questions:"),
                                      
                                      selectInput("Population", "Key Population", choices = unique(df2$Population)),
                                      
                                      selectInput("Biological.sex","Gender", c("Female","Male", "Prefer not to say")),
                                      
                                      selectInput("County", "County of Residence", c("Nairobi","Migori","Mombasa","Kisumu")),
                                      
                                      selectInput("e1demoend1cp", "Place of Residence", choices = unique(df2$e1demoend1cp)[-7]),

                                      selectInput("Clinic", "Referral Clinic","Please select a county first"),
                                 
                                      selectInput("e1demoend1q18", "Have you done any work in the last seven days for which you received a paycheck, cash or goods as payment?", c("Yes", "No")),
                                      
                                      selectInput("e1end11e2hseq26", "What is your marital status?", choices = unique(df2$e1end11e2hseq26)[-7]),
                                      
                                      selectInput("e1end11e2rand3_q127","My friends influence my decision to use HIV prevention methods", choices = c("Strongly disagree","Disagree","Neutral","Agree", "Strongly Agree")),
                                      
                                      selectInput("e1end11e2rand4_q130", "If you could choose between the two options which would you take?", choices = unique(df2$e1end11e2rand4_q130)),
                                      
                                      selectInput("e1end11e2Disc_q153","How frequently would you like to get PrEP from your clinic?", choices = unique(df2$e1end11e2Disc_q153))
                                    ),
                                  
                                  mainPanel(
                                    h3("Your answers"),
                                    h4("Population:"),textOutput("pop"),
                                    h4("Gender:"),textOutput("gender"),
                                    h4("County:"),textOutput("county"),
                                    h4("Residence:"),textOutput("residence"),
                                    h4("Clinic:"),textOutput("clinic"),
                                    h4("Own paycheck:"),textOutput("pay"),
                                    h4("Marital status:"),textOutput("marital"),
                                    h4("Peer influence on HIV prevention methods:"),textOutput("HIVprevention"),
                                    h4("Chosen option:"),textOutput("option1"),
                                    h4("Frequency of getting PrEP:"),textOutput("frequency"),
                                    
                                    tags$hr(),
                                    actionButton("submit","Submit"),
                                    tags$p(),
                                    tags$b(uiOutput("prediction")),
                                    tags$br(),
                                    tags$i(uiOutput("disclaimer")),
                                    tags$hr(),
                                    uiOutput("persona")
                                  )),
                           
                           tabPanel("About This Tool",
                                   
                                    # div(style="float:left",
                                    #   img(src="logos.png", style="padding-right:10px;width:450px;"),
                                    #   h3("Introduction"),
                            fluidRow(
                              column(4,HTML("
                                    <div style=float:left;>
                                    
                                    <h3>Introduction</h3>
                                    
                                    <p><a href=https://www.busaracenter.org/ target=_blank>Busara</a> in collaboration with <a href=https://www.jhpiego.org/ target=_blank>Jhpiego</a> 
                                    has developed a predictive tool that uses machine learning to predict how long one will be on the oral PrEP drug.</p> 
                                    
                                    <p>This tool also acts as a commitment device that health providers can use to reinforce the habit of 
                                    consistent oral PrEP use from the time someone is enrolled onto the Jilinde program.</p>
                                    
                                    <p>It predicts whether someone will use oral PrEP for a long period <b>(responsive)</b> or a short period <b>(non-responsive)</b>, 
                                    after answering a series of 10 questions, allowing the healthcare provider to adjust the treatment program so as 
                                    to encourage continued use of the drug.</p>
                                    
                                    <p>The 10 questions were identified to be most influential in determining one's stay in the program after a series of analysis.</p>
                                    
                                    <img src=logos.png style=width:450px;padding-left:0px>
                                    
                                    <p style=text-align:center;>
                                    &#169; <script>document.write(new Date().getFullYear())</script>
                                    </p>
                                    
                                     </div>")),
                              column(8,DT::dataTableOutput('clinics'))))
                           
                           ))



