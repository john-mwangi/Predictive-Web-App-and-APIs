
library(shiny)
library(tidyverse)
library(fastDummies)
library(caret)
library(rsconnect)
library(kernlab)
library(modeltools)
library(party)
library(randomForest)
library(DT)


Shinyserver <- function(input, output,session) {
  
  #Pre-loaded objects
  load("oral_prep_final_model_ensemble_ft_eng_v3.RData")
  
  output$clinics <- DT::renderDT(df_clinics)
  
  output$pop <- renderText(input$Population)
  output$gender <- renderText(input$Biological.sex)
  output$county <- renderText(input$County)
  output$clinic <- renderText(input$Clinic)
  output$residence <- renderText(input$e1demoend1cp)
  output$pay <- renderText(input$e1demoend1q18)
  output$marital <- renderText(input$e1end11e2hseq26)
  output$HIVprevention <- renderText(input$e1end11e2rand3_q127)
  output$option1 <- renderText(input$e1end11e2rand4_q130)
  output$frequency <- renderText(input$e1end11e2Disc_q153)


  #Dynamically update the list of clinics based on selected county  
  observe({
    updated_clinics <- list_updates$Clinic[list_updates$County==input$County]
    updateSelectInput(session,inputId = "Clinic",choices = unique(updated_clinics))
  })
  
  observe({
    updated_residence <- list_updates$e1demoend1cp[list_updates$County==input$County]
    updateSelectInput(session,inputId = "e1demoend1cp",choices = unique(updated_residence))
  })
  
  observe({
    updated_gender <- df2$`Biological sex`[df2$Population==input$Population]
    updated_gender <- c(unique(updated_gender),"Prefer not to say")
    updateSelectInput(session,inputId = "Biological.sex",choices = updated_gender)
  })

  
dfInput = reactive({
  
  if (input$submit>0) {
               df2_subject <- data.frame(Population = as.character(input$Population),
                                         `Biological sex`= as.character(input$Biological.sex),
                                         County = as.character(input$County),
                                         Clinic = as.character(input$Clinic),
                                         e1demoend1cp = as.character(input$e1demoend1cp),
                                         e1demoend1q18 = as.character(input$e1demoend1q18),
                                         e1end11e2hseq26 = as.character(input$e1end11e2hseq26),
                                         e1end11e2rand3_q127 = as.character(input$e1end11e2rand3_q127),
                                         e1end11e2rand4_q130 = as.character(input$e1end11e2rand4_q130),
                                         e1end11e2Disc_q153 = as.character(input$e1end11e2Disc_q153)
                           )
  

               
#Rename some variables
names(df2_subject)[names(df2_subject) == "Biological.sex"] <- "Biological sex"

#### Checks are also labelled as predictions and can be commented out.
#Basic checks
prediction <- dim(df2_subject)
prediction <- class(df2_subject)
prediction <- as.data.frame(lapply(df2_subject,class))
prediction <- df2_subject
 
 
#Append the row (for dummification purposes)
df2_t10 <- df2[,names(df2) %in% c(as.vector(imp_var_all_top),"Biological sex")]
prediction <- names(df2_t10)
df2_ft_2 <- rbind(df2_t10,df2_subject)
prediction <- dim(df2_ft_2)

#Dummify the data then apply make.names()
#binary categorical vars
bin.vars.dum_2 <- df2_ft_2 %>%
  select_if(is.character) %>% 
  select_if(.predicate = function(x) length(unique(x))==2) %>% 
  dummy_cols(remove_first_dummy = T, remove_selected_columns = T)

#dummify non-binary categorical vars
nbin.vars.dum_2 <- df2_ft_2 %>% 
  select_if(is.character) %>% 
  select_if(function(x) length(unique(x))>2) %>%  
  dummy_cols(remove_first_dummy = F, remove_selected_columns = T)

df2_all_ft_2 <- cbind(bin.vars.dum_2,nbin.vars.dum_2)

colnames(df2_all_ft_2) <- make.names(colnames(df2_all_ft_2))

prediction <- names(df2_all_ft_2) #52 cols expected

#Select the last row
df2_subject_dum <- df2_all_ft_2[nrow(df2_all_ft_2),]

#In some computers, this variable is not created after dummification
df2_subject_dum$e1end11e2rand4_q130_A.50..chance.to.gain.KSH.1.250.and.a.50..chance.to.gain.nothing = 1 - df2_subject_dum$e1end11e2rand4_q130_A.sure.gain.of.KSH..500


#Obtain probabilities from base models of this new observation
preds_ct_ft_prob_tr_2 <- predict(object = model_ct_ft_all_t10, newdata = df2_subject_dum, type = 'prob')
preds_rf_ft_prob_tr_2 <- predict(object = model_rf_ft_all_t10, newdata = df2_subject_dum, type = 'prob')
preds_svmP_ft_prob_tr_2 <- predict(object = model_svmP_ft_all_t10, newdata = df2_subject_dum, type = 'prob')
preds_svmR_ft_prob_tr_2 <- predict(object = model_svmR_ft_all_t10, newdata = df2_subject_dum, type = 'prob')


#Check
prediction <- preds_svmR_ft_prob_tr_2

model_probs_ft_2 <- as.data.frame(cbind(preds_ct_ft_prob_tr_2$Low_Continuation,
                                      preds_rf_ft_prob_tr_2$Low_Continuation,
                                      preds_svmP_ft_prob_tr_2$Low_Continuation,
                                      preds_svmR_ft_prob_tr_2$Low_Continuation))

colnames(model_probs_ft_2) <- c('ctree','rf','svmP','svmR')

prediction <- model_probs_ft_2

#Probabilities of prediction from the neural network
preds_ens_nnet_2 <- predict(object = model_nnet, newdata = model_probs_ft_2, type = "prob")

#Checks
prediction <- preds_ens_nnet_2
prediction <- th_nn
prediction <- paste(ls(), collapse = "<br>")

#Final prediction
prediction <- ifelse(test = preds_ens_nnet_2$Low_Continuation>th_nn, yes = 'Non-responsive', no = 'Responsive')
probability <- preds_ens_nnet_2$Low_Continuation
probability <- round(probability,3)


#Return variable
#return(list(prediction=prediction))
return(list(prediction=prediction,probability=probability))
  }                                       
})                                       

output$prediction <- renderText({
  #paste0(dfInput()$prediction," ", dfInput()$probability)
  paste0(dfInput()$prediction)
})

output$disclaimer <- renderText({
  ifelse(dfInput()$prediction!="","Disclaimer: Whilst every effort has been taken during the 
          development of this tool to make it as accurate and reliable as possible, 
         it is still a prediction and not an absolute.","")
})


output$persona <- renderText({
  ifelse(test = dfInput()$prediction=="", 
         "Click the submit button",
         ifelse(test = (dfInput()$probability>=0 & dfInput()$probability<=0.365) & input$Population=="Female Sex   Workers (FSWs)", 
                yes = 
                  
                  "<p style=font-size:30px;>TINA</p>
                     
                     <img src=Tina.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> Youngest: 32 years old
                     <li> Likely to have completed primary school
                     <li> Engaged in sex work
                     <li> More likely to have a mean of 21 sexual partners over a 6 month period
                     <li> More likely to have sex with a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Supportive social network
                     <li> Put in place reminders of when to take the pill
                     <li> Ability to discuss safe sex and oral PrEP use with partner and clients
                     
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> Belief that oral PrEP should only be taken while at risk
                      <li> Concerns about the packaging of the pill
                      <li> Distance to the clinic
                      <li> Intoxication while on oral PrEP",
          ifelse(test = (dfInput()$probability>=0.366 & dfInput()$probability<=0.619) & input$Population=="Female Sex   Workers (FSWs)", 
                       yes = 
                         
                    "<p style=font-size:30px;>LILY</p>
                       
                     <img src=Lily.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                       
                     <b>Background</b><br>
                     <li> 31 years old
                     <li> Likely to have completed primary school
                     <li> Engaged in sex work with some irregular source of income
                     <li> More likely to have a mean of 43 sexual partners over a 6 month period
                     <li> More likely to have sex with a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Supportive social network
                     <li> Perceived high risk of contracting HIV
                     <li> Good experience at the clinic
                     
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                     <li> Traveling
                     <li> Forgetfulness
                     <li> Concerns about the packaging of the pill
                     <li> Perceived harm to unborn babies",
                       
                       
           ifelse(test = (dfInput()$probability>=0.62 & dfInput()$probability<=0.834) & input$Population=="Female Sex   Workers (FSWs)", 
                         yes = 
                                
                         "<p style=font-size:30px;>NOVELA</p>
                     
                         <img src=Novela.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                                                 
                          <b>Background</b><br>
                          <li> 32 years old
                          <li>Likely to have completed primary school
                          <li> Engaged in sex work
                           <li> More likely to have a mean of 85 sexual partners over a 6 month period
        
                           <br><br><b>Motivation for oral PrEP use</b><br>
                           <li> Believes in the efficacy of oral PrEP
                           <li> Supportive social network
                           <li> Perceived high risk of contracting HIV
                           <li> Good experience at the clinic
                           <li> Lack agency to negotiate condom use
                                                 
                            <br><br><b>Barriers to oral PrEP use</b><br>
                            <li> HIV related stigma from neighbors and peers  collecting the pill from clinic
                            <li> Forgetfulness
                            <li> Intoxication while on oral PrEP use
                            <li> Concerns about side effects
                            <li> Concerns about the packaging of the pill",
                              
            ifelse(test = (dfInput()$probability>=0.835 & dfInput()$probability<=1) & input$Population=="Female Sex   Workers (FSWs)", 
                                              yes = 
                                       
                                                 "<p style=font-size:30px;>FURAHA</p>
                                                 
                                                 <img src=Furaha.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                                                 
                                                 <b>Background</b><br>
                                                 <li> 26 years old
                                                 <li> Likely to have completed primary school
                                                 <li> Engaged in sex work with some irregular source of income
                                                 <li> More likely to have a mean of 76 sexual partners over a 6 month period
                                                 <li> Likely to have sex with a condom
        
                                                 <br><br><b>Motivation for oral PrEP use</b><br>
                                                 <li> Believes in the efficacy of oral PrEP
                                                 <li> Good experience at the clinic
                                                 <li> Perceived high risk of contracting HIV
                                     
                                                 <br><br><b>Barriers to oral PrEP use</b><br>
                                                  <li> HIV related stigma from friends, peers and neighbors
                                                  <li> HIV related stigma associated with collecting the pill from clinic
                                                  <li> Distance to the clinic
                                                  <li> Difficulty in consistently taking the pill
                                                  <li> Concerns about the packaging of the pill",
              ifelse(test = (dfInput()$probability>=0 & dfInput()$probability<=0.365) & input$Population=="AGYWs", 
                      yes = 
                                              
                     "<p style=font-size:30px;>LINDA</p>
                     
                     <img src=Linda.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> Older: 20-34 years
                     <li> Likely to have completed primary school
                     <li> Engaged in casual work (non-farm)
                     <li> Most likely to have one sexual partner
                     <li> Likely to have sex with a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Have supportive friends and family members
                     <li> Good experience at the clinic
                     <li> Access to long term prescriptions (3 months)
                     <li> Has set up reminders of when to take the pill
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> Concerns about side effects
                      <li> Concerns about packaging
                      <li> Distance to the clinic",
                                            
            ifelse(test = (dfInput()$probability>=0.366 & dfInput()$probability<=0.619) & input$Population=="AGYWs", 
                    yes = 
                                                     
                    "<p style=font-size:30px;>VALARY</p>
                     
                     <img src=Valary.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> Slightly older: 17-21 years
                     <li> Likely to have completed secondary school
                     <li> Engaged in casual work (non-farm)
                     <li> Most likely to have one sexual partner
                     <li> Minority have upto four sexual partners
                     <li> Likely to have sex without a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Have supportive friends and family members
                     <li> Partner’s infidelity
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> Concerns about side effects
                      <li> Concerns about packaging
                      <li> Concerns about size of pill",
                                                   
           ifelse(test = (dfInput()$probability>=0.62 & dfInput()$probability<=0.834) & input$Population=="AGYWs", 
                     yes = 
                     
                     "<p style=font-size:30px;>BRENDA</p>
                     
                     <img src=Brenda.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> Youngest: 16-21 years
                     <li> Likely to have completed secondary school
                     <li> Engaged in casual work (non-farm)
                     <li> More likely to have one sexual partner
                     <li> Likely to have sex without a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Desire to complete her studies and take control of her life
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> More likely to lack a supportive partner
                      <li> Concerns about side effects
                      <li> HIV related stigma from friends, peers and neighbors
                      <li> Difficulty in consistently taking the pill
                      <li> School obligations",
                                                          
              ifelse(test = (dfInput()$probability>=0.835 & dfInput()$probability<=1) & input$Population=="AGYWs", 
                    yes = 
                    
                    "<p style=font-size:30px;>BRENDA</p>
                     
                     <img src=Brenda.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> Youngest: 16-21 years
                     <li> Likely to have completed secondary school
                     <li> Engaged in casual work (non-farm)
                     <li> More likely to have one sexual partner
                     <li> Likely to have sex without a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Desire to complete her studies and take control of her life
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> More likely to lack a supportive partner
                      <li> Concerns about side effects
                      <li> HIV related stigma from friends, peers and neighbors
                      <li> Difficulty in consistently taking the pill
                      <li> School obligations",
              ifelse(test = (dfInput()$probability>=0 & dfInput()$probability<=0.365) & input$Population=="Men who have sex with Men (MSMs)", 
                     yes =
                     
                     "<p style=font-size:30px;>FRED</p>
                     
                     <img src=Fred.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> 28 years old
                     <li> Likely to have completed secondary school
                     <li> Engaged in sex work
                     <li> More likely to have a mean of 4 sexual partners over a 6 month period
                     <li> Likely to have sex without a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Has put in place reminders of when to take the pill
                     <li> Perceived high risk of HIV acquisition
                     <li> Good experience at the clinic
                     <li> Presence of a supportive social network
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> Difficulty in consistently taking the pill
                      <li> Concerns about side effects
                      <li> Forgetfulness
                      <li> Intoxication while on oral PrEP",
                                                                        
          ifelse(test = (dfInput()$probability>=0.366 & dfInput()$probability<=0.619) & input$Population=="Men who have sex with Men (MSMs)", 
                    yes = 
                    
                    "<p style=font-size:30px;>FRED</p>
                     
                     <img src=Fred.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> 28 years old
                     <li> Likely to have completed secondary school
                     <li> Engaged in sex work
                     <li> More likely to have a mean of 4 sexual partners over a 6 month period
                     <li> Likely to have sex without a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Has put in place reminders of when to take the pill
                     <li> Perceived high risk of HIV acquisition
                     <li> Good experience at the clinic
                     <li> Presence of a supportive social network
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> Difficulty in consistently taking the pill
                      <li> Concerns about side effects
                      <li> Forgetfulness
                      <li> Intoxication while on oral PrEP",                                 
            ifelse(test = (dfInput()$probability>=0.62 & dfInput()$probability<=0.834) & input$Population=="Men who have sex with Men (MSMs)", 
                   yes =
                   
                   "<p style=font-size:30px;>ROCKY</p>
                     
                     <img src=Rocky.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> 26 years old
                     <li> Likely to have completed secondary school
                     <li> Engaged in casual work
                     <li> More likely to have a mean of 16 sexual partners over a 6 month period
                     <li> Likely to have sex with a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Has put in place reminders of when to take the pill
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> HIV related stigma from friends, peers and neighbors
                      <li> Would not tell partner they are on oral PrEP
                      <li> Difficulty in consistently taking the pill
                      <li> Concerns about side effects
                      <li> Intoxication while on oral PrEP",
        ifelse(test = (dfInput()$probability>=0.835 & dfInput()$probability<=1) & input$Population=="Men who have sex with Men (MSMs)s", 
                   yes = 
                   "<p style=font-size:30px;>FELLOH</p>
                     
                     <img src=Felloh.jpg style=padding-right:20px;padding-bottom:20px;float:left;width:230px;height:400px;> 
                     
                     <b>Background</b><br>
                     <li> 25 years old
                     <li> Likely to have completed college
                     <li> Engaged in sex work
                     <li> More likely to have a mean of 10 sexual partners over a 6 month period
                     <li> Likely to have sex with a condom
                     
                     <br><br><b>Motivation for oral PrEP use</b><br>
                     <li> Believes in the efficacy of oral PrEP
                     <li> Has put in place reminders of when to take the pill
                     <li> Perceived high risk of HIV acquisition
                     <li> Good experience at the clinic
                     <li> Presence of a supportive social network
                     
                     <br><br><b>Barriers to oral PrEP use</b><br>
                      <li> Forgetfulness
                      <li> Uncomfortable discussing safe sex with partner
                      <li> Intoxication while on oral PrEP",NULL)
        ))))))))))))


})

}

#test





