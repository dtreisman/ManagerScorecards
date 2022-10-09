# Manager Scorecards

Ignore...
[![manager-scorecard-bot](https://github.com/dtreisman/ManagerScorecards/actions/workflows/daily_run.yml/badge.svg)](https://github.com/dtreisman/ManagerScorecards/actions/workflows/daily_run.yml)

10-07-2022

## Introduction

This project aims to quantify the value of pitching change decisions in the MLB. Inspired by the popular Twitter account [Umpire Scorecards](https://twitter.com/UmpScorecards), this project compiles yesterday's MLB games, calculated the total expected runs lost on pitching change decisions during each game for each team and [tweets the results](https://twitter.com/MLBManagerScore). 

## Methodology

There are two models working together to created "expected runs lost". The first is an expected runs model using a random forest algorithm. This model is comprised of game-state variables such as outs, runners on base, inning, score differential, platoon advantage, and others. However, it also incorporates the current pitchers wOBA-against (rolling 100 PA), the batter's wOBA (rolling 100 PA), and the pitchers wOBA in the current game. The model was trained on the 2020 and 2021 seasons. (The model was originally trained on 2020, and tested on 2021 to get an RMSE of around .34 compared to a naive RMSE of .41). 

Predictions were made for expected runs for all plate appearances. Predictions were also made for all other available pitchers on the active roster assuming they were the current pitcher. By comparing the current expected runs to the hypothetical values, the "expected runs lost"" is how many expected runs were lost by having the current pitcher in the game as opposed to a different pitcher. 

The second model is the expected pitching change model. The model was trained using similar variables as the model above in order to predict a probability of a pitching change. In order to not over-penalize a manager for making a substitution, the expected runs values were adjusted by whether a pitching change was likely. A manager will get credited (penalized) for more expected runs lost if a pitching change was highly unexpected, and less penalty if the pitching change was likely given the situation.

The final statistic (xRuns Lost) can be defined as:
> (max(xRuns<sub>hypothetical</sub>) - xRuns<sub>current</sub>) * abs(is_currently_pitching(0 or 1) - prob_new_pitcher)

This methodology assumes that all relievers are available and no starters are available. For each situation, a pitcher is not considered available if they pitched earlier in the game. In the playoffs, values will not reflect starting pitchers that pitch in relief.

This is currently the soft-launch/beta-test of the product. More methodology details will be updated in the coming weeks.
