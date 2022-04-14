
# # ####################################################
# # # This script makes a psychTestR implementation of
# # # a "one-minute science" pilot study assessing SMT
# # # 
# # # Date:14/4- 2022
# # # Author: Cecilie Møller
# # # Project group: Above + Jan Stupacher, Alexandre Celma-Miralles, Peter Vuust
# # ###################################################


library(htmltools)
library(psychTestR)
library(tibble)



jspsych_dir <- "jspsych-6.1.0"

head <- tags$head(
  # jsPsych files
  
  
  # # If you want to use original jspsych.js, use this:
  # includeScript(file.path(jspsych_dir, "jspsych.js")),
  
  # If you want to display text while preloading files (to save time), specify your intro_text
  # in jsPsych.init (in run-jspsych.js) and call jspsych_preloadprogressbar.js here:
  includeScript(file.path(jspsych_dir, "jspsych_preloadprogressbar.js")),
  
  includeScript(
    file.path(jspsych_dir, "plugins/jspsych-html-button-response.js")
  ),
  
  includeScript(
    file.path(jspsych_dir, "plugins/jspsych-audio-button-response.js")
  ),
  
  includeScript(
    file.path(jspsych_dir, "plugins/jspsych-html-slider-response.js")
  ),
  
  # Custom files
  includeScript(
    file.path(
      jspsych_dir,
      "plugins/jspsych_BPM/jspsych-audio-bpm-button-response.js"
    )
  ),
  includeCSS(file.path(jspsych_dir, "css/jspsych.css")),
  includeCSS("css/style.css")
)


ui_exp <- tags$div(
  head,
  includeScript("exp-rand.js"),
  includeScript("new-timeline.js"),
  includeScript("run-jspsych.js"),
  tags$div(id = "js_psych")
)


poly_ratio <- page(               #NB - this experiment is pitch only but named "poly_ratio" because it used to be part of the ratio/tempo/pitch study, and because we want to concatenate this data with the original pitch data
  ui = ui_exp,
  label = "poly_ratio",
  get_answer = function(input, ...)
    input$jspsych_results,
  validate = function(answer, ...)
    nchar(answer) > 0L,
  save_answer = TRUE
)

# PsychTestR elements


intro <- c(
welcome <-
  one_button_page(div(
    HTML("<img src='img/au_logo.png'></img> <img src='img/mib_logo.png'></img>"),
    div(
      h3(strong("One-minute science!")),
      p(" Reading this text literally takes longer than participating in this pilot experiment on spontaneous motor tempo. We we will use your data to explore research ideas worth pursuing within the realm of musical beat perception. Your participation is much appreciated. Thank you!"),
      p("If possible, please use a device with a touchscreen, alternatively a clickpad or mouse, rather than a laptop touchpad. You can not use a keyboard."),
      
      HTML("<br>"),
      HTML("<p style= 'font-size:14px'> <em>The research pilot data you generate is completely anonymous. Questions? Contact cecilie@clin.au.dk. By clicking the button below (I understand. Continue!) you confirm that you are at least 18 years old and you give your consent to participate in this pilot experiment.</em></p>"),
    )
  ),
  button_text = "I understand. Continue!"
       
    ),


# PAGES
device <-dropdown_page(
  label = "device",
  prompt = div(h4(strong("Device")),
               p("Which device you are using now?")),
              save_answer=TRUE,
  choices = c("Select current device", "Smartphone (touchscreen)","Tablet (touchscreen)","Laptop (click button/clickpad)", "Laptop (external mouse)", "Desktop (external mouse)"),
  alternative_choice = TRUE,
  alternative_text = "Other - please state which?",
  next_button_text = "Next",
  max_width_pixels = 250,
  validate = function(answer, ...) {
    if (answer=="Select current device")
      "Which device are you using to take the test? Click the small arrow on the right of the first box to see the options. We ask because it matters for the analyses of the data you provide."
    else if (answer=="") 
      "Please tell us which device you are currently using. If you select 'Other' at the bottom of the list, please state in the designated field which type of device you use to take the test."
    else TRUE
  },
  on_complete = function(answer, state, ...) {
    set_global(key = "device", value = answer, state = state)
  }     
),

browser <- dropdown_page(
  label = "browser",
  prompt = div(h4(strong("Browser")),
               p("Which browser are you using?"),
               # HTML("<br>"),
                p("PLEASE NOTE: Safari may not work, depending on your settings."),
                  ),
  save_answer=TRUE,
  choices = c("Select current browser","Firefox", "Chrome","Edge","Internet Explorer","Safari", "Opera", "I do not know"),
  alternative_choice = TRUE,
  alternative_text = "Other - please state which?",
  next_button_text = "Next",
  max_width_pixels = 250,
  validate = function(answer, ...) {
    if (answer=="Select current browser")
      "Please tells us which browser you use (click the small arrow on the right of the first box to see the options). We ask because it matters for the analyses of the data you provide."
    else if (answer=="") 
      "Please answer the question. If you select 'Other' at the bottom of the list, please state the name of your browser in the designated field."
    else TRUE
  },
  on_complete = function(answer, state, ...) {
    set_global(key = "browser", value = answer, state = state)
  }  
)

)

# DEMOGRAPHICS

age <-dropdown_page(
  label = "age",
  prompt = div(h4(strong("Thanks! Two super quick questions before we're done.")),
              
               p(strong ("What is your age?")),
               ),
  save_answer=TRUE,
  choices = c("Please select","18-19 years","20-21","22-23","24-25","26-27","28-29","30-31","32-33","34-35","36-37","38-39","40-41","42-43","44-45","46-47","48-49","50-51","52-53","54-55","56-57","58-59","60-61","62-63","64-65","66-67","68-69","70-71","72-73","74-75","76-77","78-79","80 years or above"),
  next_button_text = "Next",
  max_width_pixels = 250,
  validate = function(answer, ...) {
    if (answer=="Please select")
      "Please state your age (click the small arrow on the right of the box to see the options). We ask because it matters for the analyses of the data you provide."
    #else if (answer=="") 
   #   "Please answer the question. If you select 'Other' at the bottom of the list, please state the name of your browser in the designated field."
    else TRUE
  },
  on_complete = function(answer, state, ...) {
    set_global(key = "age", value = answer, state = state)
  }  
)

# 
# gender<-NAFC_page(
#   label = "gender",
#   prompt = p(strong ("Whats is your gender?")), 
#   choices = c("Female", "Male","Other","I prefer not to tell you"),
# )

# MUSICAL EXPERIENCE

music_exp <- 
  # c(


# ollen
ollen<-NAFC_page(
  label = "ollen",
  prompt = p(strong ("Which title best describes you?")), 
  choices = c("Nonmusician", "Music-loving nonmusician","Amateur musician","Serious amateur musician","Semiprofessional musician","Professional musician"),
  on_complete = function(answer, state, ...) {
    set_global(key = "ollen", value = answer, state = state)
    # if (answer == "Nonmusician"|answer =="Music-loving nonmusician") skip_n_pages(state,3)
  }
  )
# COMMENTS

duplets <- dropdown_page(
  label = "duplets",
  prompt = p(strong("Did you take part in this exact same pilot experiment before?")),
  save_answer=TRUE,
  choices = c("Please select", "No", "Yes, once before", "Yes, twice before",	"Yes, three times before",	"Yes, four times before",	"Yes, five times before",	"Yes, six or more times before"),
  # alternative_choice = TRUE,
  # alternative_text = "I prefer not to tell you",
  next_button_text = "Next",
  max_width_pixels = 250,
  validate = function(answer, ...) {
    if (answer=="Please select")
      "Please let us know if you tried this exact same experiment before. We ask because it matters for the analyses of the data you provide. If you like, you can provide additional comments in the next and final question."
    else TRUE
  },
  on_complete = function(answer, state, ...) {
    set_global(key = "duplets", value = answer, state = state)
    if (answer != "No") skip_n_pages(state,1)
  }  
)


code_1 <- text_input_page(
  label = "code",
  # one_line = TRUE,
  width = "400px",
  prompt = div(
    HTML("<br>"),
    p(strong("Personal code")),
    HTML("<br>"),
    p("Please enter a simple code of your choice that you will be able to reproduce in an hour, a day or a week or so if you agree to repeat this experiment."),
    p("Choose e.g., the name of your favourite pet, the first and last two letters of your grandmothers name, or anything else that you will be able to remember."),
    p("Please do not write any personal information such as your full name, email address, phone number etc.")
    
  ),
  save_answer = T,
  button_text = "Next",
  validate = function(answer, ...) {
    if (answer=="")
      "Please enter any combination of letters, numbers, or symbols. No restrictions. Just keep it simple, so you can remember your code later if prompted to do so."
    else TRUE
  },
  on_complete = function(answer, state, ...) {
    set_global(key = "code_1", value = answer, state = state)
    if (answer != "No") skip_n_pages(state,1)
  }  
)

code_2 <- text_input_page(
  label = "code",
  # one_line = TRUE,
  width = "400px",
  prompt = div(
    HTML("<br>"),
    p(strong("Personal code")),
    HTML("<br>"),
    p("Please enter the self-chosen code you used last time you took part in this pilot experiment.")
  ),
  save_answer = T,
  button_text = "Next",
  validate = function(answer, ...) {
    if (answer=="")
      "Please enter the code you have previously used in this experiment."
    else TRUE
  },
  on_complete = function(answer, state, ...) {
    set_global(key = "code_2", value = answer, state = state)
    if (answer != "") skip_n_pages(state,2)
  } 
)
thanks<-final_page(div(
      HTML("<img src='img/au_logo.png'></img> <img src='img/mib_logo.png'></img>"),
               div(
            h3(strong("Thanks again!")),
            p("You are more than welcome to repeat this pilot experiment as many times as you like. Just make sure to enter exactly the same code every time, so we can link your data."),
            p("We may even ask you to repeat the experiment if we happen to know you personally. Just email cecilie@clin.au.dk if you do not want to be asked."),
            p("............."),
            HTML("<br>"),
            
            p("Your data has been saved now and you can safely close the browser window.")
             )
            ))


elts <- join(
  
   intro,
   poly_ratio,
   elt_save_results_to_disk(complete = FALSE),
   duplets,
   code_1,
   code_2,
   age,
   ollen,
   elt_save_results_to_disk(complete = TRUE),
   thanks
)


 make_test(
     elts = elts,
     opt = test_options(title="SMT consistency, pilot 2022",
                        admin_password="", # write a secret password here
                        enable_admin_panel=TRUE,
                        researcher_email="cecilie@clin.au.dk",
                        problems_info="Problems? Contact cecilie@clin.au.dk",
                        display = display_options(
                         full_screen = TRUE,
                         css = c(file.path(jspsych_dir, "css/jspsych.css"),"css/style.css")
         )))

# shiny::runApp(".")
 
 # # EXTRACT TAPS
 # TAP<-fromJSON(rawdata[["results"]][["poly_ratio"]])
 # View(TAP)


