# ------------------
#  BANDIT TASK
#  Implimented in Shiny as ShinyBandit by Nathaniel D. Phillips
# 
#  http://ndphillips.github.io, Nathaniel.D.Phillips.is@gmail.com
#
#   CODE SECTIONS
#
#   0: Load libraries
#   A: Setup Game
#     A1: Game parameters
#     A2: Data saving
#     A3: Reformat data
#     A4: Miscellaneous code
#   B: Overall layout
#   C: Reactive values
#   D: Page layouts
#   E: Game display
#     E1: Box and tickets
#     E2: Main game display
#   F: Event (button) actions
#     F1: Option selection
#     F2: Page navigation buttons
#     F3: Event tracking
#   G: Save data
# ------------------



# --------------------------
# Section 0: Load libraries
# --------------------------

library(shiny)
#library(rdrop2)
#library(sendmailR)
library(digest)
library(yarrr)

options(shiny.usecairo = FALSE)
# --------------------------
# Section A: Setup game
# --------------------------

# Section A1: GAME PARAMETERS

distributions <- round(
  cbind(runif(n = 1e5, min = 100, max = 120),
        runif(n = 1e5, min = 90, max = 110),
        runif(n = 1e5, min = 70, max = 90)), 0)

goal<-20
trials.n <- 50                          # Trials per game
practice.n <- 10                        # Trials in practice game
games.n <- 1                            # Number of games
randomize.locations <- FALSE             # Should the location of options be randomized?
randomize.outcomes <- FALSE              # Should the order of outcomes be randomized?
saveDataLocation <- "dropbox"           # Either dropbox, email, or local
outputDir <- "banditdata/SUFE_demo"  # Directory to save data

# Section A2: DATA SAVING
if(saveDataLocation == "dropbox") {
  
  droptoken <- readRDS("droptoken.rds")        # Reads in authentication for dropbox
  
}
if(saveDataLocation == "email") {  # not working yet!!!!!
  
  # See https://goo.gl/kQLrTk for guide
  
  from <- "nathaniel.phillips@unibas.ch"
  to <- "nathaneil.phillips@unibas.ch"
  subject <- "Email Subject"
  body <- "Email body."       
  mailControl <- list(smtpServer = "serverinfo")
  
}
if(saveDataLocation == "local") {   # not working yet!
  
}

# Section A3: Reformat data
options.n <- ncol(distributions)   # Number of options
outcomes.ls <- lapply(1:games.n, FUN = function(x) {return(distributions)})
if(randomize.locations) {
  
  locations.r <- lapply(1:games.n, FUN = function(x) {sample(options.n)})
  
  for(game.i in 1:games.n) {
    
    locations.i <- locations.r[[game.i]]
    
    outcomes.ls[[game.i]] <- outcomes.ls[[game.i]][,locations.i]
  }
}
if(randomize.outcomes) {
  
  for(game.i in 1:games.n) {
    
    outcomes.ls[[game.i]] <- outcomes.ls[[game.i]][sample(nrow(distributions)),]
  }
}

# Section A4: Miscellaneous code

# Calculate study completion code
completion.code <- paste("EP-BANDIT", sample(100:999, size = 1), 
                         sample(100:999, size = 1),
                         sample(100:999, size = 1), sep = "-")

# --------------------------------
# Section B: Define overall layout
# --------------------------------

ui <- fixedPage(
  
  title = "ShinyBandit",
  uiOutput("MainAction"),
  tags$style(type = "text/css", ".recalculating {opacity: 1.0;}")   # Prevents gray screen during Sys.sleep()
  
)

server <- function(input, output, session) {
  
  output$MainAction <- renderUI( {
    PageLayouts()
  
})

# --------------------------------
# Section C: Define Reactive Values
#   These store the main values in the game
# --------------------------------

# CurrentValues stores scalers representing the latest game outcomes
CurrentValues <- reactiveValues(page = "instructions",     # Current page
                                game = 0,             # Current game
                                trial = 1,            # Current trial
                                selection = 0,        # Selection
                                outcome = 0,          # Outcome
                                points.cum = 0)       # Points earned

# GameValues stores vectors of histories

previous.display <- function(){
  par(mar = c(1, 1, 1, 1))
  layout(matrix(c(1, 1), nrow = 1, ncol = 1, byrow = TRUE), heights = c(2, 2), widths = c(1))
  plot.new()
  text(c(0.1, .5, .9), c(.9, .9, .9), labels = c("Area of investment", "Number of times invested", "Average return"), cex = 1.5, font = 1)
  text(c(0.1, .5, .9), c(.7, .7, .7), labels = c("A", "3", "20"), cex = 1.5, font = 1)
  text(c(0.1, .5, .9), c(.5, .5, .5), labels = c("B", "3", "20"), cex = 1.5, font = 1)
  text(c(0.1, .5, .9), c(.3, .3, .3), labels = c("C", "3", "20"), cex = 1.5, font = 1)
  
}

output$previous_dis <- renderPlot({
  previous.display()
}
)

GameData <- reactiveValues(game = c(),
                           trial = c(),          
                           time = c(),
                           selection = c(),
                           outcome = c(),
                           points.cum = c())
  
  # --------------------------------
  # Section D: Page Layouts
  # --------------------------------
  
  PageLayouts <- reactive({
    
    # P1) Welcome
    if (CurrentValues$page == "welcome") {
      
      return(
        list(
          h2("????????????"),
          p("?????????????????????"),
          textInput(inputId = "workerid", 
                    label = "??????", 
                    value = "", 
                    placeholder = "??????:??????"),
          #textInput(inputId = "password", 
          #          label = "Please enter a password", 
          #          value = "", 
          #          placeholder = "e.g. 123"),
          # This displays the action putton Next.
          actionButton(inputId = "gt_instructions", 
                       label = "?????????") 
        )
      )}
    
    # P2) Instructions
    if (CurrentValues$page == "instructions") {
      
      return(
        list(
          h2("投资游戏"),
          h3("以下是游戏界面示意图"),
          plotOutput('GameScreenShot'),
          h3("游戏规则"),
          p("每个回合选取一个创业项目进行投资，点击对应方块。方块中会反馈投资回报。"),
          p("你的目标是，在50个回合中尽可能的获得更多的投资回报"),
          p("点击下一步进入准备"),

          
          actionButton(inputId = "gt_startnextgame", 
                       label = "下一步") 
        )
      )}
    
    # P3) Start next game
    if (CurrentValues$page == "startnextgame") {
      
      return(
        list(
          h2("等待发令!"),
          p("等待发令，你有3分钟的时间进行50轮投资。发令响后开始"),
          actionButton(inputId = "gt_game", 
                       label = paste("Start Game")) 
        )
      )}
    
    # P4) Game
    if (CurrentValues$page == "game") {
      
      return(
        list(
          # Main Display
          fixedRow(
            column(12, h2("点击一个方块进行投资，投资回报在方块中显示")),
            column(12, plotOutput('GameDisplay', click = "plot_click")),
            column(12, plotOutput('ScoreHistory'))
            )
        )
      )
    }
    
    # P5) Game end
    if (CurrentValues$page == "gameend") {
      
      return(list(h3("保存，查看你的最终成绩"),
                  actionButton(inputId = "gt_goodbye", 
                               label = "查看成绩")))
    }
    
    # P6) All games end page
    if (CurrentValues$page == "allgameend") {
      
      return(list(h3("保存，查看你的最终成绩"),
                  actionButton(inputId = "gt_goodbye", 
                               label = "查看成绩")))
    }
    
    # P7) Survey
    if (CurrentValues$page == "postsurvey") {
      
      return(list(
        h3("Survey (1 of 1)"),
        
        radioButtons("sex",
                     label = "What is your sex?",
                     choices = list("Male" = 1, 
                                    "Female" = 2, 
                                    "Other" = 3),
                     selected = 99),
        
        numericInput("age", 
                     label = "What is your age?",
                     value = NA),
        
        radioButtons("interesting", 
                     label = "How interesting did you find the Boxes Game?",
                     choices = c("1 - Not at all" =  1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5 - Very Much" = 5), 
                     selected = 99),
        
        textAreaInput(inputId = "strategy",
                      label = "What was your strategy in the Boxes Game?",
                      placeholder = "I tried to...", 
                      resize = "both",
                      value = ""),
        
        radioButtons("dontuse",
                     label = "Is there any reason why we should NOT use your data for scientific research? For example, were you not paying attention or were you intoxicated?",
                     choices = c("No. My data should be valid for scientific research" =  "0",
                                 "Yes. There is a good reason why you should NOT use my data for scientific research" = 1), 
                     selected = 99),
        
        radioButtons("playedbefore",
                     label = "Have you played a game similar to the Boxes game in the past?",
                     choices = c("No. I have not played a game similar to the Boxes game in the past" = 0,
                                 "Yes. I have played a game very similar to the Boes game in the past" = 1,
                                 "I am not sure" = 2), 
                     selected = 99),
        
        textAreaInput("comments",
                      label = "If you have any additional comments, please enter them below",
                      resize = "both", 
                      value = ""),
        
        actionButton(inputId = "gt_goodbye",
                     label = "Save Data and End Study"))
      )
    }
    
    # P8) Goodbye
    if (CurrentValues$page == "goodbye") {
      
      return(list(
        h3("投资积累曲线"),
        plotOutput('EarningsPlot'),
        h2("你最终的成绩是:", span(CurrentValues$points.cum, style="color:red"), "请截图保存！"),
        tags$img(src = "thankyou.jpg", width = "300px")
      ))
    }
    
  })
  
  # --------------------------------
  # Section E: Game Display
  # --------------------------------
  
  # Section E1: Box and Ticket graphical parameters
  
  #   Boxes
  box.center.x <- seq(.1, .9, length.out = options.n)
  box.center.y <- rep(.5, options.n)
  box.height <- .75
  box.width <- 1 / (options.n + 1)
  
  box.x0.v <- box.center.x - box.width / 2
  box.y0.v <- box.center.y - box.height / 2
  box.x1.v <- box.center.x + box.width / 2
  box.y1.v <- box.center.y + box.height / 2
  
  #   Tickets
  ticket.n <- 8
  ticket.width <- box.width / 15
  ticket.height <- box.height / 15
  ticket.col <- gray(.9)
  
  ticket.x.v <- as.numeric(sapply(1:options.n, FUN = function(x) {
    
    rep(seq(from = box.center.x[x] - box.width / 3,
            to = box.center.x[x] + box.width / 3,
            length.out = ticket.n), each = ticket.n)
    
  }))
  
  ticket.y.v <- as.numeric(sapply(1:options.n, FUN = function(x) {
    
    rep(seq(from = box.center.y[x] - box.height / 3,
            to = box.center.y[x] + box.height / 3,
            length.out = ticket.n), times = ticket.n)
    
  }))
  
  ticket.x0.v <- ticket.x.v - ticket.width / 2
  ticket.x1.v <- ticket.x.v + ticket.width / 2
  ticket.y0.v <- ticket.y.v - ticket.height / 2
  ticket.y1.v <- ticket.y.v + ticket.height / 2
  
  # Section E2: Main game display
  
  score.display <-function(arm1, arm2, arm3){
    par(mar = c(1, 1, 1, 1))
    res1 <- strsplit(strwrap(toString(arm1), width=20), "\n")
    res2 <- strsplit(strwrap(toString(arm2), width=20), "\n")
    res3 <- strsplit(strwrap(toString(arm3), width=20), "\n")
    text(0.5, 0.95, label="Score History", cex=2)
    text(c(0.1, 0.5, 0.9), c(0.85, 0.85, 0.85), labels=c("A", "B", "C"), cex=2)
    maxed_row <- max(c(length(res1),length(res2),length(res3)))
    if (CurrentValues$trial>1)
    {
    for (i in 1:maxed_row)
    {
      y.location <- 0.8-0.05*i
      if (i>length(res1) | length(arm1)==0)
      {
        res1_print <- " "
      }
      else
      {
        res1_print <- res1[i]
      }
      
      if (i>length(res2) | length(arm2)==0)
      {
        res2_print <- " "
      }
      else
      {
        res2_print <- res2[i]
      }
      
      if (i>length(res3) | length(arm3)==0)
      {
        res3_print <- " "
      }
      else
      {
        res3_print <- res3[i]
      }
      
      text(c(0.1, 0.5, 0.9), c(y.location, y.location, y.location), labels=c(res1_print, res2_print, res3_print), cex=1.5)
    }

    }
    
    
    #for (i in 1:length(res1))
    #{
    #  scorelist <- res1[i]
    #  mtext(scorelist, side=3, cex=1, at=box.center.x)
    #}
    
    #mtext(strwrap(optionlist[1:options.n]), side = 3, cex = 1, at = box.center.x)
    
  }
  
  bandit.display <- function(outcome,                # What is the outcome on this trial?
                             selection,              # Which option is selected?
                             points,                 # How many points have been earned?
                             trials.left,            # How many trials remain?
                             pos.col = "black",     # Color of positive outcomes
                             neg.col = "indianred1", # Color of negative outcomes
                             neu.col = "gray") {     # Color of neutral outcomes
    
    # Determine current outcome and display
    if(is.numeric(outcome)) {
      
      if(outcome > 0) {
        text.col <- pos.col
        current.outcome.disp <- paste0("+", outcome)
      }
      
      if(outcome < 0) {
        text.col <- neg.col
        current.outcome.disp <- outcome
      }
      
      if(outcome == 0) {
        text.col <- neu.col
        current.outcome.disp <- outcome
      }
    }
    
    if(is.numeric(outcome) == FALSE) {
      
      text.col <- "black"
      current.outcome.disp <- outcome
      
    }
    
    # Plotting space
    par(mar = c(1, 1, 1, 1))
    layout(matrix(c(1, 2), nrow = 2, ncol = 1, byrow = TRUE), heights = c(3,3), widths = c(9))
    
    # Title row
    plot.new()
    text(c(.5, .85), c(.7, .7), labels = c("Points Earned", "Round No. (of 50)"), cex = 2, font = 3)
    text(c(.5, .85), c(.4, .4), labels = c(points, trials.n - trials.left+1), cex = 4)
    abline(h = .2, col = gray(.5), lwd = 2)
    
    # Option display
    
    plot(1, xlim = c(0, 1), ylim = c(0, 1), type = "n", 
         xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    
    # Options
    rect(xleft = box.x0.v, ybottom = box.y0.v,
         xright = box.x1.v, ytop = box.y1.v, 
         lwd = 2)
    
    
    # Tickets
    rect(xleft = ticket.x0.v,
         ybottom = ticket.y0.v,
         xright = ticket.x1.v,
         ytop = ticket.y1.v,
         col = ticket.col, lwd = .5, 
         border = gray(.3))
    
    # Option Labels
    optionlist <- list("A", "B", "C")
    mtext(optionlist[1:options.n], side = 3, cex = 3, at = box.center.x)
    
    # Outcome display
    if(is.finite(selection)) {
      
      # Outcome border
      rect(xleft = box.center.x[selection] - box.width * .4,
           ybottom = box.center.y[selection] - box.height * .4,
           xright = box.center.x[selection] + box.width * .4,
           ytop = box.center.y[selection] + box.height * .4,
           col = "white"
      )
      # Outcome Display
      text(x = box.center.x[selection], y = .5, 
           current.outcome.disp, cex = 7, col = text.col)
      
    }
    
    
  }
  
  
  # Game Screenshot (used in instructions)
  output$GameScreenShot <- renderPlot({
    
    bandit.display(outcome = "",
                   selection = NA,
                   points = 0,
                   trials.left = trials.n)
    
  })
  
  # Actual game display
  output$GameDisplay <- renderPlot({
    
    bandit.display(outcome = CurrentValues$outcome,
                   selection = CurrentValues$selection,
                   points = CurrentValues$points.cum,
                   trials.left = trials.n - CurrentValues$trial + 1)
  })
  
  output$ScoreHistory <- renderPlot({
    score.display(
      arm1 = CurrentValues$arm1,
      arm2 = CurrentValues$arm2,
      arm3 = CurrentValues$arm3
    )
  })
  
  # --------------------------------
  # Section F: Event (e.g.; button) actions
  # --------------------------------
  
  # Section F1: Page Navigation Buttons
  observeEvent(input$gt_instructions, {CurrentValues$page <- "instructions"})
  observeEvent(input$gt_gameend, {CurrentValues$page <- "gameend"})
  observeEvent(input$gt_game, {CurrentValues$page <- "game"})
  observeEvent(input$gt_postsurvey, {CurrentValues$page <- "postsurvey"})
  observeEvent(input$gt_goodbye, {CurrentValues$page <- "goodbye"})
  observeEvent(input$gt_startnextgame, {CurrentValues$page <- "startnextgame"})
  observeEvent(input$gameend, {
    
    if(CurrentValues$game != games.n) {
      
      CurrentValues$game <- CurrentValues$game + 1
      CurrentValues$trial <- 1
      CurrentValues$selection <- 0
      CurrentValues$points.cum <- 0
      CurrentValues$page <- "startnextgame"
      
    } else {
      
      CurrentValues$page <- "allgameend"}
    
  })
  
# Section F2: Option selection buttons
selection.update <- function(selection.i,
                             trial.i,
                             game.i) {
  
  if(trial.i <= trials.n) {
    
    outcome.i <- outcomes.ls[[game.i]][trial.i, selection.i]
    if(trial.i>trials.n-12){
      if(selection.i==3){
        outcome.i <- outcomes.ls[[game.i]][trial.i, selection.i]*5
      }
    }
    time.i <- proc.time()[3]
    
    # Update current values
    CurrentValues$selection <<- selection.i
    CurrentValues$outcome <<- outcome.i   
    CurrentValues$points.cum <<- CurrentValues$points.cum + outcome.i
    CurrentValues$time <<- time.i
    CurrentValues$game <<- game.i
    if (selection.i==1)
    {
      CurrentValues$arm1 <<- c(isolate(CurrentValues$arm1), isolate(outcome.i))
    }
    if (selection.i==2)
    {
      CurrentValues$arm2 <<- c(isolate(CurrentValues$arm2), isolate(outcome.i))
    }    
    if (selection.i==3)
    {
      CurrentValues$arm3 <<- c(isolate(CurrentValues$arm3), isolate(outcome.i))
    } 
    
    
    # Update GameData
    GameData$game <<- c(GameData$game, game.i)
    GameData$trial <<- c(GameData$trial, trial.i)
    GameData$time <<- c(GameData$time, time.i)
    GameData$selection <<- c(GameData$selection, selection.i)
    GameData$outcome <<- c(GameData$outcome, outcome.i)
    GameData$points.cum <<- c(GameData$points.cum, CurrentValues$points.cum)
    
  }
  
  CurrentValues$trial <<- CurrentValues$trial + 1
}

  
observeEvent(input$plot_click, {
  
 # Which option was selected?
  
  selection.log <- input$plot_click$x > box.x0.v & 
                   input$plot_click$x < box.x1.v & 
                   input$plot_click$y > 0 & 
                   input$plot_click$y < .5
  
  if(any(selection.log)) {
    
    selection.update(which(selection.log), CurrentValues$trial, CurrentValues$game)
  }
  
})  



# Section F3: Event tracking buttons

# Reset current values at start of a game
observeEvent(input$gt_startnextgame, {
  
  # Increase game number by 1
  CurrentValues$game <- CurrentValues$game + 1
  
  # Set CurrentValues to defaults for start of next game
  CurrentValues$trial <- 1
  CurrentValues$selection <- 0
  CurrentValues$points.cum <- 0
  
  
})

# Watch for last trial in a game
observeEvent(CurrentValues$trial, {
  
  if(CurrentValues$trial == (trials.n + 1)) {
    
    CurrentValues$page <- "gameend"
    
  }
  
})



# --------------------------------
# Section G: Save data
# --------------------------------
observeEvent(input$gt_goodbye, {(
  
  # Create progress message   
  withProgress(message = "Saving data...", value = 0, {
    
    incProgress(.25)
    
    response.time.v <- c(NA, round(GameData$time[2:(trials.n)] - GameData$time[1:(trials.n - 1)], 4))
    
    # Write GameData to a dataframe
    

    
    GameData.i <- data.frame("game" = GameData$game,
                             "trial" = GameData$trial,
                             "time" = GameData$time,
                             "selection" = GameData$selection,
                             "outcome" = GameData$outcome,
                             "points.cum" = GameData$points.cum)
    
    GameDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(GameData.i), "_g.csv")
    
    # Write Survey data
    if(length(input$workerid) == 0) {workerid.i <- NA} else {workerid.i <- input$workerid}
    if(length(input$comments) == 0) {comments.i <- NA} else {comments.i <- input$comments}
    if(length(input$age) == 0) {age.i <- NA} else {age.i <- input$age}
    if(length(input$sex) == 0) {sex.i <- NA} else {sex.i <- input$sex}
    if(length(input$dontuse) == 0) {dontuse.i <- NA} else {dontuse.i <- input$dontuse}
    if(length(input$interesting) == 0) {interesting.i <- NA} else {interesting.i <- input$interesting}
    if(length(input$playedbefore) == 0) {playedbefore.i <- NA} else {playedbefore.i <- input$playedbefore}

    #SurveyData.i <- data.frame("workerid" = workerid.i,
    #                           "age" = age.i,
    #                           "sex" = sex.i,
    #                          "comments" = comments.i,
    #                           "dontuse" = dontuse.i,
    #                           "interesting" = interesting.i,
    #                           "playedbefore" = playedbefore.i,
    #                           "option.order" = paste(locations.r, collapse = ";"),
    #                           "completion.code" = completion.code)
    

    #SurveyDatafileName <- paste0(input$workerid,
    #                             as.integer(Sys.time()),
    #                             digest::digest(SurveyData.i), "_s.csv")
    
    incProgress(.5)
    
    
    if(saveDataLocation == "dropbox") {
      
      GameDatafilePath <- file.path(tempdir(), GameDatafileName)
      write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
      #rdrop2::drop_upload(GameDatafilePath, path = outputDir, dtoken = droptoken)
      
      #SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
      #write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
      #rdrop2::drop_upload(SurveyDatafilePath, dest = outputDir, dtoken = droptoken)
      
    }
    
    # if(saveDataLocation == "email") {
    #   
    #   GameDatafilePath <- file.path(tempdir(), GameDatafileName)
    #   write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
    # 
    #   SurveyDatafilePath <- file.path(tempdir(), SurveyDatafileName)
    #   write.csv(SurveyData.i, SurveyDatafilePath, row.names = FALSE, quote = TRUE)
    # 
    #   attachmentObject <- mime_part(x = "GameDatafilePath", name = GameDatafileName)
    #   attachmentObject2 <- mime_part(x = "SurveyDatafilePath", name = SurveyDatafileName)
    #   bodyWithAttachment <- list(body, attachmentObject, attachmentObject2)
    #   
    #   sendmail(from = from, to = to, subject = subject, msg = bodyWithAttachment, control = mailControl)
    #   
    # }
    
    
    # Write survey data 
    
    
    incProgress(.75)
    
    # Some interesting plots (not necessary)
    
    output$GameData.tbl <- renderTable(head(GameData.i))            
    output$EarningsPlot <- renderPlot({
      
      my.cols <- yarrr::piratepal("xmen", trans = .3)
      
      plot(1, xlim = c(0, trials.n), ylim = c(min(GameData.i$points.cum), max(GameData.i$points.cum)), 
           xlab = "Draw", ylab = "Points earned", main = "Your Earnings over time in each Game", type = "n")
      
      grid()
      
      for(game.i in 1:games.n) {
        
        lines(x = subset(GameData.i, game == game.i)$trial,
              y = subset(GameData.i, game == game.i)$points.cum,
              type = "l", col = my.cols[game.i], lwd = 3)
        
      }
      
      legend("topleft", legend = paste("Game", 1:games.n), 
             col = my.cols, lty = 1, lwd = 2, bty = "n")
      
    }) 
    
    Sys.sleep(.25)
    incProgress(1)
    
    
    
  })
  
)})
  
}

# Create app!
shinyApp(ui = ui, server = server)