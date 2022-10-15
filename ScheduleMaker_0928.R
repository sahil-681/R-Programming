# Libraries 

library(grid)

# Functions 

#' Convert 12 hour times to minutes since the day began 
#' 
#' @description Input a string of length two that has start and stop times of a 
#' given meeting to convert them to start and stop times of minutes since the
#' day began. 
#' 
#' @details 
#' A 12 hour time is converted to 24 hour time. Since this is a meeting-specific
#' function, the code assumes times between "1" and "7" are in the 
#' afternoon/evening as it is unlikely a meeting will start between 1:00 am and
#' 7:00 am. 
#' 
#' Once times are converted to 24 hour time, hours are multiplied by 60 and
#' minutes are multiplied by 100 and then the two are added. See examples 
#'
#' @param time a string of length two with start time and stop
#' time in 12 hour times 
#' 
#' @return list of integers of length 2 with start and end time as minutes since 
#' the day began 
#' 
#' @example 
#' # Create a string vector where 12.00 is the start time and 1.00 is the end 
#' time of the meeting 
#' time <- c("12.00", "1.00")
#' convertTime(time)
#' 
#' # 720 is the start time and 780 is end time (both in minutes since day began)
#' # Note: 1.00 is assumed to be in the afternoon and so converts to 13.00 which 
#' # is 780 minutes since the day began
#' 
#' @export 

convertTime <- function(time){
  
  # Convert string to numeric 
  st <- as.numeric(time[1])
  et <- as.numeric(time[2])
  
  # Convert to 24 hour time
  if(st >= 1 & st <= 5) st <- st + 12.00
  if(et >= 1 & et <= 5) et <- et + 12.00
  
  # Return as minutes since day began
  return(list(as.integer(st) * 60 + st%%1 * 100,
              as.integer(et) * 60 + et%%1 * 100))
}

#' Make a schedule 
#' 
#' @description Function to plot the schedule with available and meeting times 
#' 
#' @details 
#' This function is designed for users who want to visualize their data on times 
#' available for meetings and selected times to meet. 
#' For example, a teacher wants to visualize available meeting times for 19 
#' students and selected meeting times per student.
#' The function produces a minimalistic plot is organized per person i.e. each
#' row is a person. The columns are times of the day. The function will draw a 
#' blank box around times indicated as being "available" and shaded grey boxes
#' for times selected as meeting times. 
#' 
#' NOTE: Selected meeting times need to be an input in the function, 
#' the function will NOT automatically assign meeting times. 
#' 
#' @param names vector of character with names of all students 
#' @param timesAvailable A list the length of number of students where each 
#' element is another list with the start and stop times the student is 
#' available to meet
#' @param timesMeet A list the length of number of students where each element 
#' is a list with the start and stop meeting times 
#' @param startHour (numeric) Hour at which the schedule should start plotting 
#' @param endHour (numeric) Hour at which the schedule should end plotting 
#' @param tickInt (numeric) Interval at which ticks should be displayed e.g. 
#' 60 would display ticks at every 60 minutes 
#' @param new (logical) should the schedule open a new plot window to plot? 
#' TRUE for yes (default). 
#' @param title character defining the title of the schedule 
#' 
#' @return A plot of schedule with given available times and meeting times 
#' 
#' @example 
#' # Create a string of two names 
#' names <- c("A", "B")
#' 
#' # Create a list of lists for times available 
#' 
#' timesAvailable <- list(list(convertTime(c(12.30, 13.15))),
#' list(convertTime((08.00, 10.00))))
#' names(timesAvailable) <- names
#' 
#' # Create a list of lists for meeting times 
#' 
#' timesMeet <- list(list(convertTime(c(12.30, 13.15)), list(convertTime(
#' c(08.00, 09.00)))))
#' 
#' names(timesMeet) <- names
#' 
#' # Define start hour, end hour, tick interval and title 
#' startHour <- 7
#' endHour <- 14
#' tickInt <- 60 
#' title <- "Class schedule for 662" 
#' 
#' # Run plot 
#' 
#' makeSchedule(names, timesAvailable, timesMeet, startHour, endHour, 
#' tickInt, title)
#' 
#' @export



makeSchedule <- function(names, timesAvailable, timesMeet, startHour, endHour,
                         tickInt = 60, new = T, title = ""){

  if (new) grid.newpage()
  
  # Create a little edge buffer
  pushViewport(viewport(width = 0.98, height = 0.95))
  
  grid.text(title, x=0, y = 1, just = "left", gp = gpar(fontface = 'bold'))
  
  maxlen <- unit(1, "strwidth",
                 data = names[which.max(nchar(names))])
  
  pushViewport(viewport(x = 0, y = 0,
                        width = maxlen + unit(1, "char"), 
                        height = 1,
                        just = c("left", "bottom"),
                        yscale = c(0.5, length(names) + 1.5)))
  
  grid.text(names, 
            x = unit(0.4, "char"), 
            y = unit(1:length(names), "native"),
            just = c("left", "center"))
  
  popViewport(1) # The name viewport
  
  k <- length(names)
  
  # Make the outer rectangle
  pushViewport(viewport(x = 1, y = 0,
                        width = unit(1, "npc") - (maxlen + unit(1, "char")), 
                        height = 1,
                        just = c("right", "bottom"),
                        yscale = c(0.5, length(names) + 1.5),
                        xscale = c(startHour, endHour)))
  
  grid.rect(x = 0, y = 0, height = .95, just = c("left", "bottom"))
  
  # Plot header with hours of the day 
  hours <- as.character(seq(startHour, endHour, by=1))
  
  for (i in 1:length(hours)) {
    if (as.integer(hours[i]) < 12) { 
      hours[i] <- paste0(hours[i], "AM")
    } else if (as.integer(hours[i]) == 12) {
      hours[i] <- paste0(hours[i], "PM")
    } else {
      t <- as.integer(hours[i]) - 12
      hours[i] <- paste0(t, "PM")
    }
  }
  
  pushViewport(viewport(x = 0, y = 0.95,
                        width = 1, 
                        height = 1,
                        just = c("left", "bottom"),
                        xscale = c(startHour * 60, endHour * 60),
                        yscale = c(0.5, length(hours) + 0.5)))
  
  for (i in 2:(length(hours) - 1)) {
    
    grid.text(hours[i],
              x = unit(60 * (startHour + (i - 1)), "native"),
              y = 1 / (2 * length(names)),
              just = 'center',
              rot = 0,
              gp = gpar(fontsize = 7.5))
    
    grid.lines(x = unit(c(60 * (startHour + (i-1)), 
                          60 * (startHour + (i-1))),"native"),
               y = c(1 / (4 * k), 0))
    
  }
  
  popViewport(1)
  
  # Add in ticks 
  cur = startHour * 60 + tickInt
  
  while (cur < endHour * 60) {
    
    grid.lines(x = unit(c(cur / 60, cur / 60), "native"),
               y = unit(c(1 - 1 / k, 0), "npc"),
               gp = gpar(fill = 'grey25', alpha = .5))
    
    cur = cur + tickInt
    
  }
  
  # Loop for individual available times and meeting times 
  
  for (i in 1:k) {
    pushViewport(viewport(x = 0,
                          y = (i - 1) / (k + 1),
                          width = 1,
                          height = (1 / (k + 1)),
                          just = c("left","bottom"), 
                          xscale = c(startHour * 60, endHour * 60)))
    
    for (j in 1:length(timesAvailable[[i]])) {
      if (length(timesAvailable[[i]]) == 0 ) break 
      st <- timesAvailable[[i]][[j]][[1]]
      et <- timesAvailable[[i]][[j]][[2]]
      grid.rect(x = unit(st,"native"),
                width = unit(et - st, "native"),
                just = "left",
                gp = gpar(alpha = 1))
    }
    
    if (length(timesMeet[[i]]) != 0 ) {
      
      st <- timesMeet[[i]][[1]][1]
      et <- timesMeet[[i]][[1]][2]
      
      # Annoying little workaround for the list of lists
      et <- as.numeric(et[[1]])
      st <- as.numeric(st[[1]])
      
      grid.rect(x = unit(st, "native"),
                width = unit(et - st, "native"),
                just = "left",
                gp = gpar(fill = 'grey50', lty = "dashed", alpha = .75))
    }
    popViewport(1)
    
  }  
  popViewport(1)
  
}


# "Configurations"
classname <- '662'    
day <- "Friday"
startHour <- 5
endHour <- 17

# Data
data <- read.csv("662_survey.csv", as.is = TRUE)

names(data)[c(10,12)] <- c("MeetThursday", "MeetFriday")
data <- data[ , substring(names(data), 1, 1) != "data"]
data <- data[order(data$name, data$submitted, decreasing = TRUE), ]
data <- data[!duplicated(data$name), ]

data <- data[ , c("name", "id", "sis_id", "section", "MeetThursday", 
                 "MeetFriday")]
names <- data[ , "name"]


# Initialize the first time this is run:
data$day <- "Thursday"
data$time <- "9:00-9:45"

# Change selected days/times based on plots - I suppose you could do this 
# in Excel, I prefer to do it here basically so I don't have to switch windows

# Some examples below 
ww <- which(colnames(data) %in% c("day", "time"))

data[data$name == "Abby Spears", ww] <- c("Thursday", "8:00-8:45") 
data[data$name == "Christian Bombara", ww] <- c("Thursday", "8:00-8:45") 
data[data$name == "Emily Goldfarb",  ww] <- c("Thursday", "10:30-11:15")
data[data$name == "Emily Goldfarb",  ww] <- c("Thursday", "10:30-11:15")
data[data$name == "Evian Liu",  ww] <- c("Thursday", "10:30-11:15")
data[data$name == "Gang Wen",  ww] <- c("Thursday", "10:30-11:15")
data[data$name == "Jessica Eshenbaugh", ww] <- c("Thursday", "11:30-12:15")
data[data$name == "Kelly Wang", ww] <- c("Thursday", "11:30-12:15")
data[data$name == "Louis Deschuttere", ww] <- c("Thursday", "1:00-1:45")
data[data$name == "Max Lovig", ww] <- c("Thursday", "1:00-1:45")
data[data$name == "Nökkvi Elliðason", ww] <- c("Friday", "8:00-8:45")
data[data$name == "Sahil Singh", ww] <- c("Friday", "8:00-8:45")
data[data$name == "Shiyu Dou", ww] <- c("Friday", "11:00-11:45")
data[data$name == "Shubhi Sharma", ww] <- c("Friday", "9:00-9:45")
data[data$name == "Shuyu Rao", ww] <- c("Friday", "10:00-10:45")
data[data$name == "Siddhartha Chatterjee", ww] <- c("Friday", "9:00-9:45")
data[data$name == "Tiernon Riesenmy", ww] <- c("Friday", "10:00-10:45")
data[data$name == "Zihe Zheng", ww] <- c("Friday", "9:00-9:45")

# Get and clean the time availability information for Thursday
tempTh <- gsub("-[^,]*,|-[^,]*$", ",", data$MeetThursday)
tempTh <- gsub(":", ".", tempTh)
tempTh <- lapply(strsplit(tempTh, ","), as.numeric)

# Ditto for Friday...
tempFr <- gsub("-[^,]*,|-[^,]*$", ",", data$MeetFriday)
tempFr <- gsub(":", ".", tempFr)
tempFr <- lapply(strsplit(tempFr, ","), as.numeric)


for (i in 1:nrow(data)) {
  possible <- ifelse(data$day[i] == "Friday", data$MeetThursday[i], 
                     data$MeetFriday[i])
  if (!grepl(paste0(data$time[i], ":00"), possible)) {
    data$name[i] <- paste(data$name[i], "[NONE]") 
  }
}


# Meeting times 
# Focus 'y' from the original 'x' and clean it up a bit:
y <- data[, c("name", paste0("Meet", day))]
y$name <- gsub(" [NONE]", "", y$name, fixed = TRUE)
names(y)[2] <- "available"
y$selected <- rep(NA, nrow(y))

# Get selected meeting time 
sub <- data[which(data$day == day), c("name", "time")]

if(nrow(sub) > 0) {
sub$name <- gsub(" [NONE]", "", sub$name, fixed = TRUE)
y$selected[match(sub$name, y$name)] <- sub$time
} 


# Build the empty list 'z', one element per student:
z <- vector(mode = "list", length = nrow(data))
names(z) <- y$name

timesMeet <- vector(mode = "list", length = nrow(data))
names(timesMeet) <- y$name

# Now process, student by student:
for (i in 1:length(z)) {
  
  temp <- unlist(strsplit(y$available[i], ","))
  temp <- strsplit(temp, split = "-")
  temp <- lapply(temp, function(x) gsub(":", ".", x))
  temp <- lapply(temp, convertTime)
  
  if(is.na(y[i, 'selected'])) {
    
    meet <- list()
  } else {
    meet <- strsplit(y[i, "selected"], "-")
    meet <- lapply(meet, function(x) gsub(":", ".", x))
    meet <- lapply(meet, convertTime)
    
  }
  
  z[[i]] <- temp
  timesMeet[[i]] <- meet
  
}

timesAvailable <- z

# Finally, plot
makeSchedule(names, timesAvailable, timesMeet, startHour, endHour, 
             title = paste("Schedule", classname, "-", day))

