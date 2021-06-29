# Introduction

This project aims to use publicly available data for football players to attempt to predict the outcomes for the recent 2020-21 football seasons for the First and Second Football divisions in Spain, France, England, Germany, Italy, Belgium, Scotland and Netherlands. The 2014-15 to 2019-20 season match outcomes are used as a training set.

Predicting match outcomes is an especially tricky exercise despite having a lot of metrics regarding the statistics of every player in a team, other factors such as morale, injuries, pitch/weather conditions and the specific strategies employed by teams against each other. Despite the use of fixed effects it is important to realize that there is some aspect of reinforcement learning, team strategies are constantly adapting, at times several times within the same game. Another important aspect of this study is actually checking how much does strategy matter. Any predictive power that falls short, can be attributed to strategy and its lack of control in our model. 

While there are ways of controlling for strategy they require more detailed non-public data and more computationally intensive techniques which we will discuss further later.

# 1. Model & Data

### 1.11. Football Players Data
Football players data is gathered from FIFA, and includes information that includes a players speed, shooting, agility, physicality etc. for each individual season. This data is at the player level. The data is reduced to provide one row for each team that includes the average of that teams members ability metrics. This provides a row of metrics for each Team for one football season.

This data provides nearly 230 columns of metrics for each team during a single season. The chart below shows the distribution of the overall metric for each team.

### 1.12. Football Match Data
Football Match data is gathered from https://www.football-data.co.uk. Which provides data for the date of the match, the final score as well as which team was the home/away side.

One season for a single league contains approximately 300-400 league games depending on the number of teams (excluding external competitions).

### 1.13 Balancing the Training set

The match outcomes while balanced between Lose and Win outcomes, include a smaller proportion of Draws. This means that we have unbalanced training set resulting in few to none accurate Draw predictions. To counter this we sample upwards to create a Balanced training data set that includes an equal number Wins, Losses and Draws. 

While SMOTE (Synthetic Minority Oversampling TEchnique) is an alternative to sampling upwards to create a balanced training set, due to high dimensionality of our data it is very difficult to implement SMOTE efficiently.

### 1.14 The Model
