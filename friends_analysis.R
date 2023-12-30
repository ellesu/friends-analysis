# DATA SOURCE: Friends Sitcom Dataset by Sujay Kapadnis
# LINK: https://www.kaggle.com/datasets/sujaykapadnis/friends

# open files
friends <- read.csv("~/Downloads/friends/friends.csv")
ratings <- read.csv("~/Downloads/friends/friends_info.csv")

# declare new arrays for each character
ross <- array(data = NA, dim = c(236))
rachel <- array(data = NA, dim = c(236))
joey <- array(data = NA, dim = c(236))
phoebe <- array(data = NA, dim = c(236))
monica <- array(data = NA, dim = c(236))
chandler <- array(data = NA, dim = c(236))
guest <- array(data = NA, dim = c(236))

# record total utterances to calculate percentage later
episode_total <- array(data = NA, dim = c(236))

# count number of utterances without scene directions
episode_count <- 0

# current index in all arrays
index <- 1

# current episode (starting on S01E01)
current_episode <- friends$episode[1]

# count the number of utterances per character
mon_count <- 0
ross_count <- 0
rach_count <- 0
joey_count <- 0
phoeb_count <- 0
chan_count <- 0
guest_count <- 0

for (i in 1:nrow(friends)) {
  if (!is.na(friends$speaker[i])) {
    if (friends$speaker[i] != "Scene Directions") {
      episode_count <- episode_count + 1
    }
    
    # check to see which character talked
    if (friends$speaker[i] == "Monica Geller") {
      mon_count <- mon_count + 1
    } else if (friends$speaker[i] == "Ross Geller") {
      ross_count <- ross_count + 1
    } else if (friends$speaker[i] == "Rachel Green") {
      rach_count <- rach_count + 1
    } else if (friends$speaker[i] == "Joey Tribbiani") {
      joey_count <- joey_count + 1
    } else if (friends$speaker[i] == "Phoebe Buffay") {
      phoeb_count <- phoeb_count + 1
    } else if (friends$speaker[i] == "Chandler Bing") {
      chan_count <- chan_count + 1
    } else if (friends$speaker[i] != "Scene Directions") {
      guest_count <- guest_count + 1
    }
    
    # run if the episode changes
    if (friends$episode[i] != current_episode) {
      
      # record counts
      monica[index] <- mon_count
      ross[index] <- ross_count
      rachel[index] <- rach_count
      joey[index] <- joey_count
      phoebe[index] <- phoeb_count
      chandler[index] <- chan_count
      guest[index] <- guest_count
      episode_total[index] <- episode_count
      
      # reset variables
      mon_count <- 0
      ross_count <- 0
      rach_count <- 0
      joey_count <- 0
      phoeb_count <- 0
      chan_count <- 0
      guest_count <- 0
      episode_count <- 0
      
      # update episode
      index <- index + 1
      current_episode <- friends$episode[i]
      
    }
  }
}

# Record counts for the last episode
monica[index] <- mon_count
ross[index] <- ross_count
rachel[index] <- rach_count
joey[index] <- joey_count
phoebe[index] <- phoeb_count
chandler[index] <- chan_count
guest[index] <- guest_count
episode_total[index] <- episode_count

# copy over ratings into an array
rating <- array(data = NA, dim = c(236))
for(i in 1:nrow(ratings)){
  rating[i] <- ratings$imdb_rating[i]
}

# change values to proportion of lines spoken by each character
for(i in 1:nrow(ratings)){
  monica[i] <- monica[i]/episode_total[i]
  ross[i] <- ross[i]/episode_total[i]
  rachel[i] <- rachel[i]/episode_total[i]
  joey[i] <- joey[i]/episode_total[i]
  phoebe[i] <- phoebe[i]/episode_total[i]
  chandler[i] <- chandler[i]/episode_total[i]
  guest[i] <- episode_total[i]
}

# combine arrays into one dataframe
character_data <- data.frame(
  Episode = seq_along(ross), 
  Ross = ross,
  Rachel = rachel,
  Joey = joey,
  Phoebe = phoebe,
  Monica = monica,
  Chandler = chandler,
  Guest = guest,
  Rating = ratings$imdb_rating
)

# regress each characters' proportions on the episode rating in a 
# multi-variable linear regression
summary(lm(Rating ~ (Ross + Rachel + Joey + Phoebe + Monica + Chandler + Guest), data = character_data))


# CHI SQUARED

# bin declarations
bin8 <- 0
bin825 <-0
bin85 <- 0
bin875 <- 0
bin9 <- 0
binMax <- 0

for(i in 1:nrow(rating)){
  if(rating[i] < 8){
    bin8 <- bin8 + 1
  }
  else if(rating[i] < 8.25){
    bin825 <- bin825 + 1
  }
  else if(rating[i] < 8.5){
    bin85 <- bin85 + 1
  }
  else if(rating[i] < 8.75){
    bin875 <- bin875 + 1
  }
  else if(rating[i] < 9){
    bin9 <- bin9 + 1
  }
  else{
    binMax <- binMax + 1
  }
}

observed <- c(bin8, bin825, bin85, bin875, bin9, binMax)

# reset bins
bin8 <- 0
bin825 <-0
bin85 <- 0
bin875 <- 0
bin9 <- 0
binMax <- 0


for(i in 1:nrow(ross)){
  expected_rating <- 7.344 + (1.75 * ross[i]) +0.316*rachel[i]+0.300*joey[i]+0.717*phoebe[i]+1.169*monica[i]+0.121*chandler[i]+0.002*guest[i]
  if(expected_rating < 8){
    bin8 <- bin8 + 1
  }
  else if(expected_rating < 8.25){
    bin825 <- bin825 + 1
  }
  else if(expected_rating < 8.5){
    bin85 <- bin85 + 1
  }
  else if(expected_rating < 8.75){
    bin875 <- bin875 + 1
  }
  else if(expected_rating < 9){
    bin9 <- bin9 + 1
  }
  else{
    binMax <- binMax + 1
  }
}

expected <- c(bin8, bin825, bin85, bin875, bin9, binMax)

chisq.test(x=observed, p=expected, rescale.p = TRUE)