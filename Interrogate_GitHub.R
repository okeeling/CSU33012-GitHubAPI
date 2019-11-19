#Source: Michael Galarnyk - "Accessing Data from Github API using R"

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Interrogate GitHub
oauth_endpoints("github")
    
# Access GitHub App 
githubApp <- oauth_app(appname = "Access_GitHub",
                   key = "8cfb843e259927eb7002",
                   secret = "46c553c55adb9942a6d030940fb6b5bf8e9029c5")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), githubApp)

# Use API
myToken <- config(token = github_token)
followingData <- GET("https://api.github.com/users/okeeling/repos", myToken)

# Take action on http error
stop_for_status(followingData)

# Extract content from a request
followingDataContent = content(followingData)

# Save users' data in a dataframe
dataFrame = jsonlite::fromJSON(jsonlite::toJSON(followingDataContent))

# Subset data.frame
dataFrame[dataFrame$full_name == "okeeling/datasharing", "created_at"] 

#Retrieve usernames and save in a vector
id = dataFrame$login
user_ids = c(id)

#Create empty vectors and data frame
allUsers = c()
allUsersDF = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repositories = integer(),
  dateCreated = integer()
)


#Loop through list of usernames to find users to add to the list
for (i in 1:length(user_ids)) {
  #Retrieve an individual users following list
  followingUrl = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  following = GET(followingUrl, myToken)
  followingContent = content(following)
  
  #Skip the user if they do not follow anybody
  if (length(followingContent) == 0) {
    next
  }
}
