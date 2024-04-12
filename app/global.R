box::use(shiny[h4, tagList, br, markdown],
         utils[read.csv],
         waiter[spin_dots])

# Import quiz questions
questions <- read.csv("quiz_questions.csv")

# Analysis results 
analysis <- read.csv("analysis.csv")

# Sample emojis
emojis <- c("😃", "😄", "😍", "😊", "🤗", "😎", "😉")

# Count of total questions
total_no_of_questions <- nrow(questions)

# Result waiting screen messages declaration
result_waiting_msgs <- c("Analysing your answers...", 
                         "Finding a match...", 
                         "Match found!")

# Result waiting screen generate function
result_waiting_screen <- function(msg) {
  tagList(
    spin_dots(),
    br(),
    br(),
    br(),
    h4(msg)
  )
}

# Quiz description
description <- markdown(readLines("README.md"))