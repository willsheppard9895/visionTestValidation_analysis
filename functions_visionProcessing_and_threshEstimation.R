shortenVA <- function(input){
  # this function selects the columns necessary to calculate the Va threshold as well as the ID and reaction time
  # # the screen name is revalued to represent the logMAR value of the stimuli
  #Reaction time is converted to a numeric and renamed response time
  # this is due to this being the measure named reaction time by gorilla being the time from the presentation of the stimuli to the time when the participant pressing enter following submitting there answer. therefore, this cannot be deemed a true measure of reaction time.
  output <- input %>% 
    select(id, Screen.Name, Attempt, Correct, Reaction.Time)%>%
    mutate(logMAR = 1.1-as.numeric(substring(Screen.Name, 6))/10,
           responseTime = as.numeric(Reaction.Time)
    )
  return(output)
}

summShortVA <- function(input){
  # this function calculates the proportion of correct answers for each logMAR value by participant
  # the mean RT is also calculated 
  output <- input %>%
    group_by(id, logMAR)%>%
    summarise(totalAttempt = sum(Attempt),
              totalCorrect = sum(Correct),
              meanResponseTime = mean(responseTime, na.rm = TRUE))%>%
    mutate(percCorrect = totalCorrect/totalAttempt)
  return(output)
}

threshVA <- function(input, setting){
  # VA threshold is calculated in a similar way to letter wise scoring works for a standard chart 
  # - the percentage correct for each level is subtracted from the highest possible score.
  # values are then rounded to 2dp (this may change to closest product of 0.02)
  output <- input%>%
    group_by(id)%>%
    summarise(va = 1.1-(sum(percCorrect)/10),
              y = mean(meanResponseTime)) %>%
    mutate(x = round(va, 2)) %>%
    select(-va)
  
  output[[paste('va',setting, sep = '')]] <- output$x
  output[[paste('va',setting,'RT', sep = '')]] <- output$y
  
  output <- output %>%
    select(-x, -y)
  
  return(output)
}

threshVAround <- function(input, setting){
  # VA threshold is calculated in a similar way to letter wise scoring works for a standard chart 
  # - the percentage correct for each level is subtracted from the highest possible score.
  # values are then rounded to 2dp (this may change to closest product of 0.02)
  
  output <- input%>%
    group_by(id)%>%
    summarise(va = 1.1-(sum(percCorrect)/10),
              y = mean(meanResponseTime)) %>%
    mutate(twoDP = as.numeric(str_sub(abs(va), 4, 4)))%>%
    mutate(x = case_when(
      twoDP%%2 == 0 & va >= 0 ~ plyr::round_any(va, 0.02, f = floor),
      twoDP%%2 == 0 & va < 0~ plyr::round_any(va, 0.02, f = ceiling),
      twoDP%%2 == 1 & va >= 0 ~ plyr::round_any(va, 0.02, f = ceiling),
      twoDP%%2 == 1 & va < 0 ~ plyr::round_any(va, 0.02, f = floor)
    )) %>%
    select(-va, -twoDP)
  
  
  output[[paste('va',setting, sep = '')]] <- output$x
  output[[paste('va',setting,'RT', sep = '')]] <- output$y
  
  output <- output %>%
    select(-x, -y)
  
  return(output)
}

shortenCS <- function(input){
  # selects necassary columns, takes the screen name and converts this to a contrast level, converts RT to numeric and renames
  output <- input %>%
    select(id, Screen.Name, Attempt, Correct, Reaction.Time)%>%
    mutate(contrast = 101 - as.numeric(substring(Screen.Name, 6)),
           responseTime = as.numeric(Reaction.Time)
    )%>%
    select(-Reaction.Time, -Screen.Name)
  return(output)
}

summShortCS <- function(input){
  # this function calculates the proportion of correct answers for each contrast value by participant
  # the mean RT is also calculated 
  output <- input %>%
    group_by(id, contrast)%>%
    summarise(totalAttempt = sum(Attempt),
              totalCorrect = sum(Correct),
              meanResponseTime = mean(responseTime, na.rm = TRUE))%>%
    mutate(percCorrect = totalCorrect/totalAttempt)%>%
    ungroup()
  return(output)
}



threshCS <- function(input, setting){
  
  # filter all contrast scores where participants score 50% or lower (the result is not due to chance)
  # select the lowest value and calcuate their mean rt - this rt is just for levels with 50% correct or greater, do we want this?
  # calculate logCS 
  # rename columns and remove un-needed ones
  
  
  output <- input %>%
    filter(percCorrect > .5)%>%
    group_by(id)%>%
    summarise(contrastThresh = min(contrast),
              rt = mean(meanResponseTime))%>%
    ungroup()%>%
    group_by(id)%>%
    summarise(x = 2 + log10(1/contrastThresh),
              y = rt
    )
  
  
  output[[paste('cs',setting, sep = '')]] <- output$x
  output[[paste('cs',setting,'RT', sep = '')]] <- output$y
  
  output <- output %>%
    select(-x, -y)
  
  return(output)
}

precThreshCS <- function(input, percThresh = 0.5, colName){
  
  # This function estimates where the participant achieves that target percentage correct to estimate their threshold
  # this is capped at 1% on the bottom end as this is the lowest value that we tested so we cannot extrapolate beyond this range.
  # Essentially, the function draws a line between the lowest value that the participant scored above the threshold percentage and the percentage contract below this.
  # this is scaleed depending on the % correct at the threshodl and the subthreshold contrast level.
  
  percentageThreshold <- percThresh
  ppList <- unique(input$id)
  
  
  output <- data.frame(matrix(NA,    # Create empty data frame
                              nrow = 2*length(ppList),
                              ncol = 3))
  
  cols <- c("id",
            "thresh", "threshPerc")
  colnames(output) <- cols
  
  
  for (pp in ppList){
    ## select participant 
    ## can't just filter all values below threshold as we need to access the contrast below threshold
    x <- input %>%
      filter(id == pp)
    y <- input %>%
      filter(id == pp)%>%
      filter(percCorrect >percentageThreshold)
    
    thresh <- min(y$contrast)
    
    
    
    # selects the index of the lowest contrast - this should always be 1, but this should be less buggy
    
    threshInd <- which(x$contrast == thresh)
    
    #print(paste('pp: ', pp))
    #print(paste('thresh: ', thresh))
    #print(paste('threshInd: ', threshInd))

    threshPerc <- x$percCorrect[threshInd]

    
    if (thresh == 1){
      subThresh <- as.numeric(0.0) 
    }else{
      subThresh <- as.numeric(thresh - 1)
    }
    #print(paste('subThresh:', subThresh))
    
    
    #subThreshInd <- which(x$contrast == subThresh)
    
    
    if (subThresh == 0){
      subThreshPerc <- 0
    }else{
      subThreshPerc<- x$percCorrect[which(x$contrast == subThresh)] 
    }
    

    # when the minimum contrast is not 0 and also has a percCorrect > threshold, then this produces a subThreshPerc with no value.
    # this if statement checks the length of subThreshPerc, if it is greater than 0, subThreshPerc remains, if the length is 0, the perccorrect is set to 0 as the participant nver attempted this level
    if(length(subThreshPerc)> 0) {
      subThreshPerc <- subThreshPerc
    }else{
      subThreshPerc <- 0
    }
    
    #print('subThreshPerc:')
    #print(subThreshPerc)

    #print(paste(pp, ' - ', thresh))
    
    # this is where the problem is - when the threshold is the lowest score (but not 0) the threshold contrast - 1 neeeds to be allocated to sub thresh and percCorrect needs to equal 0 
    
    percList <- seq(from = subThreshPerc, to = threshPerc, by = 0.01)
    threshList <- seq(from = subThresh, to = thresh, length.out = length(percList))
    
    interDF <- data.frame(matrix(NA,    # Create empty data frame
                                 nrow = length(percList),
                                 ncol = 3))
    cols <- c('id', 'percCorrect', 'thresh')
    interDF[1] <- pp
    interDF[2] <- percList
    interDF[3] <- threshList
    
    colnames(interDF) <- cols
    
    rm(cols)
    
    interDF <- interDF %>%
      filter(percCorrect > percentageThreshold)
    
    outDF <- interDF[1,]
    
    
    
    #colnames(interDF) <- cols
    
    cols <- c('id', 'thresh', 'cs')
    colnames(output) <- cols
    rm(cols)
    output[which(ppList == pp), 1]<-outDF[1]
    output[which(ppList == pp), 2]<-outDF[3]
    
  }
  
  
  output <- output %>%
    group_by(id) %>%
    mutate(thresh = case_when(
      thresh < 1 ~ 1,
      TRUE ~ thresh
    ))%>%
    mutate(thresh = round(thresh, 2))%>%
    mutate(cs = 2 + log10(1/thresh))

  output <- output %>%
    rename(!!colName := cs) %>%
    select(-thresh)
  
  
  
  
  
  output <- output[complete.cases(output), ]
  
  return(output)
}