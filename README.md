# Introduction

This project aims to use publicly available data for football players to attempt to predict the outcomes for the recent 2020-21 football seasons for the top First and Second Football divisions in Spain, France, England, Germany, Italy, Belgium, Scotland and Netherlands. The 2014-15 to 2019-20 season match outcomes are used as a training set.

The project uses machine learning best practices and methods such as cross validation for parameter selection, balancing classes in addition to advanced machine learning techniques such as Lasso regressions, Random Forests and Principal component analysis.

Predicting match outcomes is an especially tricky exercise despite having a lot of metrics regarding the statistics of every player in a team, other factors such as morale, injuries, pitch/weather conditions and the specific strategies employed by teams against each other tend to be very important and difficult to measure. Despite the use of fixed effects it is important to realize that there is some aspect of reinforcement learning, team strategies are constantly adapting, at times several times within the same game. For a manager realizing that they lack in holding the opposing team back when they counter may choose to leave more defenders behind when they attack. Similarly the opposing manager may realize the change in strategy and adapt as well.

This project has two aims, one to check which technique/methods provides the greatest accuracy for predicting match outcomes, and second to test for the importance of strategy in determining the outcome of matches. While we can't test for the presence/importance of strategy directly, we do have metrics on every possible feature of each team, therefore we can test the importance of strategy by checking how far are predictions fall short in the absence of controls for strategy.

The second question is of importance to the dynamics of the game in recent years, with the inflow of foreign funding and the ability of teams to purchase the best players they please, we are seeing the rise of those clubs which possess the most money. However, is the rise due to their ability to buy the best players or the best staff to create the best tactics? Likely the answer is a mixture of both, but is strategy important enough that smaller teams that lack funding can pull off wins with a superior strategy?

While there are ways of controlling for strategy they require more detailed non-public data and more computationally intensive techniques which we will discuss in detail later on.

# 1. Model & Data

### 1.11. Football Players Data
Football players data is gathered from FIFA (https://sofifa.com/), and includes information that includes a players speed, shooting, agility, physicality etc. for each individual season. This data is at the player level. The data is reduced to provide one row for each team that includes the average of that teams ability metrics for the position that they play in. This results in 4 series of metrics for the 4 broad components of a team, the defense, midfield, attack and finally the goalkeeper. This provides a row of metrics for each Team for one football season that provides metrics on how each of their 4 elements are ranked.

After this processing the data contains nearly 230 columns of metrics for each team during a single season.

### 1.12. Football Match Data
Football Match data is gathered from https://www.football-data.co.uk. Which provides data for the date of the match, the final score as well as which team was the home/away side. Additionally a "crosswalk" is constructed to allow us to merge the two data sets seamlessly.

One season for a single league contains approximately 300-400 league games depending on the number of teams (excluding external competitions). 


### 1.13 Balancing the Training set

The match outcomes while balanced between Lose and Win outcomes, include a smaller proportion of Draws. This means that we have unbalanced training set resulting in few to none accurate Draw predictions. To counter this we sample upwards to create a Balanced training data set that includes an equal number Wins, Losses and Draws. 

While SMOTE (Synthetic Minority Oversampling TEchnique) is an alternative to sampling upwards to create a balanced training set, due to high dimensionality of our data it is very difficult to implement SMOTE efficiently.

# 2. The Results

We use techniques ranging from Multinomial Logit to Lasso regression to more complex and computationally intensive techniques such as Random Forests. 
Our results show that the Boosted Trees model achieves the greatest accuracy.

However, one consistent pattern illustrated in all implemented ML techniques shows that the models have more difficulty accurately predicting a draw as compared to a win or a loss. This might be explained by the fact that draws themselves are a product idiosyncrasies or team strategies to "park the bus" (only play defensive) in the face of a far stronger opponent. 

Several football strategies such as playing on the counter, meaning that strong teams can draw with teams that anticipate they have little probability of wining using usual strategies therefore heavily rely on defense to secure a draw. Therefore we are likely to see a clear win/loss in teams that are closely matched in terms of metrics however if there exists an imbalance in the strength of the teams we might see a draw with greater likelihood as compared to a match with closely matched teams.

The results show that without the deficiency of draw predictions our models do far better for loses and wins as compared to draw. For instance the Boosted trees predict wins by an accuracy of 75%!

# 3. Next Steps

While the results are promising there are a few possible next steps that might improve our predictions. These involve improving our data as well as implementing more complex and intensive models.

  1. Using dynamic data that is updated match to match rather than snapshots from the begining of the season may more accuratly capture a players performance. 
  2. Using data that lists the coaching staff for each team, this may allow greater control for strategy and improve predictive power.
  3. Using Non-linear kernel methods with sufficient interaction terms could also lead to greater accuracy and control for some of the strategic decision that lead to inaccuracy.
  4. Given the high correlation between the different variables it might also be worthwhile using Partial Least Squares in addition to PCA for dimensionality reduction, in that it may improve performance. We see that PCA improves prediction in some models.


