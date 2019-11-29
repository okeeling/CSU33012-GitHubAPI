#Source: Michael Galarnyk - "Accessing Data from Github API using R"

#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
#install.packages("plotly")
library(plotly)
#install.packages("devtools")
library(devtools)

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

### Collecting & Displaying My Data
# Get my data
myData = fromJSON("https://api.github.com/users/okeeling")

# Display the number of followers
myData$followers

# Gives user names of all my followers
followers = fromJSON("https://api.github.com/users/okeeling/followers")
followers$login

# Display the number of users I am following
myData$following

# Gives user names of all the users I am following
following = fromJSON("https://api.github.com/users/okeeling/following")
following$login

# Display the number of repositories I have
myData$public_repos

# Gives the name and creation date for my repositories
repositories = fromJSON("https://api.github.com/users/okeeling/repos")
repositories$name
repositories$created_at

# For this assignment I used Fabien Potencier's GitHub account - fabpot
# He is one of the most popular developers on GitHub with almost 10k followers
# In addition, he is one of the most active develops on the site
# Began to interrogate Fabien Potencier's account to produce graphs, by first looking at his followers
myData = GET("https://api.github.com/users/fabpot/followers?per_page=100;", myToken)
stop_for_status(myData)
extract = content(myData)
dataFrame = jsonlite::fromJSON(jsonlite::toJSON(extract))
dataFrame$login

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
  
  #Add followings to a dataframe and retrieve usernames
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #Loop through 'following' users
  for (j in 1:length(followingLogin)) {
    
    #Check that the user is not already in the list of users
    if (is.element(followingLogin[j], allUsers) == FALSE) {
      
      #Add user to list of users
      allUsers[length(allUsers) + 1] = followingLogin[j]
      
      #Retrieve data on each user
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, myToken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #Retrieve each users following
      followingNumber = followingDF2$following
      
      #Retrieve each users followers
      followersNumber = followingDF2$followers
      
      #Retrieve each users number of repositories
      reposNumber = followingDF2$public_repos
     
      #Retrieve year which each user joined Github
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Add users data to a new row in dataframe
      allUsersDF[nrow(allUsersDF) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
    }
    next
  }
  
  #Stop when there are more than 200 users
  if(length(allUsers) > 200) {
    break
  }
  next
}

#Created link to plotly which creates online interactive graphs.
Sys.setenv("plotly_username"="okeeling")
Sys.setenv("plotly_api_key"="SP3bYV33xvTUsIr9CJu9")


# Plot #1 - Repositories vs Followers By Year
# The data is from 200 of Fabien Potencier's followers
# The y-axis displays the number of followers of each of Fabien Potencier's followers
# The y-axis displays the number of repositories of each of Fabien Potencier's followers
plot1 = plot_ly(data = allUsersDF, x = ~repos, y = ~followers, text = ~paste("Followers: ", followers, "<br>Repositories: ", repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1
# Sends graph to plotly
api_create(plot1, filename = "Repositories vs Followers")
# Plot 1 - Repositories vs Followers - Attached plot uploaded onto GitHub
