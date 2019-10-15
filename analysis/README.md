---
title: Work to be done
date: 25 July 2019
author: swo
---

# To do

## Major

- Analyses
    - Primary: Δρ/Δτ ~ ε (robust regression; just use Huber?)
        - How does -β_ε ε* / μ compare with predictions from dynamical models?
        - Use the jackknifed models both for the plot as well as for that ratio
    - Δρ/Δτ ~ adjacent?
        - Compare the difference in the median Δρ/Δτ in the adjacent vs. non-adjacent pairs, comapred with the median across all pairs, to get a sense of the decrease in Δρ/Δτ associated with adjacency
    - Covariates
        - Δρ ~ 0 + β_τ Δτ + β_ετ Δτ ε + β_x x + φ
        - To see how much the value from #1 decreases when including those cofounders
        - Look at β_τε ε* / β_τ
- Methods
    - Just do CIs with JK. If I really need p-values, then I can put them in using z-approximations
    - Without permutations, simpler: data, cross-data, then jackknifed models. Pull out ratios & CIs.
- Displays
    - Figure out how to put rlm onto the ggplot
        - Maybe run the N models, keep all the predictions, and then use the quantiles?
- Discussion
    - Resistance might also be more similar because you're drawing from the same pop's. E.g., a hospital in Kansas City might have equal numbers of people from the two states in it!
    - But we dealt with the above
    - In general, though, would be nice to know where people live and where they are "reporting" their infections, which becomes especially important at small length scales! (cf Mass APCD paper)

## Minor

- Pick "EU" or "Europe" in all places
- A limitation is that we do a pairwise analysis only. There might be some signal that arises only when considering the whole network. (But full Bayesian seems like overkill.)

## Discarded

- Δρ/Δτ ~ (ε >? median) : Don't do this, because the median is zero for Europe, which will lead to weird results

# 25 July 2019 notes

# Intro

This is the results of my meeting with Yonatan and Marc about what next steps should be for this project.

cf. the google doc with thoughts about how to mollify the reviewers

# New figure order

1. As it currently is. But change black and white in the bars!
2. Delta-use vs. delta-resistance for different epsilon. High epsilon leads to a flat line; low epsilon to a bigger slope. This might actually belong as a panel in the previous figure, as it's reporting the same type of data.
3. Delta-use vs. delta-resistance for different pairs of states. Throw in all the different data sets here, not just one, to make it really clear. In the caption, say, "Adjacent states have similar use-resistance relationships as non-adjacent states, suggesting that ... something about states being good laboratories".

**OR** show a graph of d-resistance/d-use against epsilon, showing a downward decline. Then we look in our data and find that adjacent vs. non-adjcent states don't have different d-resistance/d-use, and that plotting against commuting amount you get a flat line. But this brings up the point that we expect that there is some kind of plateau: as you get to lower epsilon, we expect that things will level out.

This means removing the aggregation thing.

# New analyses

- Sensitivity analyses
    - Changing log odds of resistance for difference
    - Changing delta-use for ratio in use
    - Include & exclude the confounders temp, income, etc; say that these are things that are spatially correlated and might be leading to similar in resistance that are *not* spillover; therefore we need to control for them (maybe; check if it matters)
    - Use commuting rates (not just adjacency) as a predictor
- The thing that will go into new figure 2

# New framing

## Connecting the pieces

- Are states good laboratories? Do we need smaller? Larger?
    - States are already big
    - But too small clearly won't work or be representative
- How to look at the two slopes (adjacent vs. not) and say if we're getting spillover signal: if two pops are perfectly connected, then slope of 0

## Observation vs. intervention

We interpret the use-resistance relationship in observational data as causal.
There are all sorts of reasons to think this isn't true; we're doing it. In
this way, we interpret the differences in resistance between pairs of states as
the difference that would come about *within* a state if use were changed by
that amount.
