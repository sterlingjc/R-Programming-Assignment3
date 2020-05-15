

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  if( is.character(num) ){
    num <- trimws(toupper(num))
  }
  
  
  if(num == 'BEST'){
    
    num <-1
      
  }
  
  
  num_args <- c('BEST', 'WORST')
  
  state <- trimws(toupper(state))
  outcome <- trimws(toupper(outcome))
  
  outcomes <- c('HEART ATTACK', 'HEART FAILURE', 'PNEUMONIA')
  
  outcomes <- toupper(outcomes)
  
  outcomes <- trimws(outcomes)
  
  cols_of_interest <- c(2,7,11,17,23)
  
  outcome_df <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings='Not Available', stringsAsFactors=FALSE)[,cols_of_interest]
  
  names(outcome_df) <- c('name','state','HEART ATTACK', 'HEART FAILURE', 'PNEUMONIA')
  
  
  for (i in 3:5) {
    
    outcome_df[, i] <- as.numeric(outcome_df[, i])
    
  }
  
  states <- unique(outcome_df$state)
  
  
  ## Check that state and outcome are valid
  
  if(!(state %in% states)) {
    
    stop('invalid state')  
    
  }
  
  
  if(!(outcome %in% outcomes)) {
    
    stop('invalid outcome')
    
  }
  
  
  
  if( !(is.numeric(num))   | !(outcome %in% outcomes) ) {
    
    stop('invalid num')
    
  }
  

  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  outcome_df <- outcome_df[!is.na(outcome_df[, outcome]) , c(1,2, 2+which(outcomes==outcome)   ) ]
  
  outcome_df <- outcome_df[ outcome_df$state==state  ,  ]
  
  outcome_df <- outcome_df[ order( outcome_df[ , outcome], outcome_df$state ) , ]
  
  
  row.names(outcome_df) <- 1:nrow(outcome_df)
  
  if(num == 'WORST'){
    
    num <-nrow(outcome_df)
    
  }
  
  
}
