---
title: Work to be done
date: 25 July 2019
author: swo
---

# 11 Sep notes

## Thoughts

- A limitation is that we do a pairwise analysis only. There might be some signal that arises only when considering the whole network.
- But, doing a full hierarchical Bayesian model seems overkill. There isn't enough good data here to think that we're *really* going to learn a lot new that way.

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
