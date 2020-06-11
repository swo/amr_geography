# To do

- Send to Rene Niehus, who knows about flight data?

## 31 May (and June 11)

1. 2-deme simulations suggest could be an important effect
2. Power calculation and ε says maybe you could see it
3. Looked in empirical data and found it!

- Fig 1: 2-deme simulations
- Fig 2: Cross-sectional data
- Fig 3: Wilcoxon results
- Fig 4: Interaction results


### Commuting & Mantel tests

1. Make a matrix of from->to counts
2. Re-scale each row (ask what *proportion* of workers from A go to B)
3. Symmetrize: X_ij -> 0.5 * (X_ij + X_ji)
4. Set the diagonals to 1 (i.e., each compartment interacts maximally with itself)

When running a Mantel test:

- Turn the ε and Δρ/Δτ matrices into ranks (i.e., so that a negative Δρ/Δτ turns into a non-negative dissimilarity)
- Reverse the ranks on Δρ/Δτ (i.e., so that large association goes with *small* interaction)
- Use the Spearman test

## 7 May

Analyses should be done. Now it's about stitching together the new manuscript.

Include a point that says that, given that adjacent US states have epsilon ~
10^{-2.5} and non-adjacent have 10^-4, we expect 0.4% reduction in dr/du vs.
ε=0 and 29%, respectively. So basically we should be getting a big decline. Use
that in the power calculations?

But not sure if I can read off ε as easily from the commuting data as I thought I could...

## Framing

### From email

Now, reading this, I'm tempted to zoom out even more and say:
Use/resistance is important, but it's sometimes hard to detect use/res relationships
Spillover could be a reason for this
If you look across many demes for a use-resistance relationship, it's also going to be weaker than you would expect because of spillover
Simulations suggest that the cross-sectional use-resistance relationship measured across states is X% weaker than it would be if those populations were totally isolated. (This is a new simulation, joining ~50 populations with the known commuting network, and comparing that "measured" use/res association against if you set ε=0.)
This might be part of the enduring puzzle of why observational use-resistance associations look so weak.
But it's really hard to think about power in this case, so let's imagine you did something simpler, with pairs of demes.
If you observationally compare pairs of states (or Census tracts, or demes of any kind) with high contrast in use, we expect dilution of the contrast in resistance.
But you won't find it in a reasonable study.
Also, we checked, and we didn't find it.
If you performed a trial randomizing demes to higher/lower use and compare resistance, we come to the same theoretical conclusion: spillover is likely important, but it's very difficult to measure, unless you sample to a crazy level.
Conclusions
Spillover is a likely cause for the apparently weak use/res associations (cf. our eLife thing)
Spillover will likely dilute the results of any geographical- or population-based stewardship trials
However, spillover is very difficult to detect when measuring prevalence of resistance alone.
Path forward to quantifying this likely has to do with sequencing and figuring out population structure, measuring how much particular strains have moved around, and use that as the proxy to estimate the effect of spillover. (But that's way harder!)
I like this framing because it addresses a long-standing puzzle. I can cite the comment on the Goossens plot, which says measuring use/res is hard because "human population densities might also help to spread resistance more widely,3 as might the number of children in day-care centres,4 and rates of vaccine uptake.5" Now we can actually put a number on that!

### From this document

- Use/resistance is important, but it's sometimes hard to detect use/res relationships (cite Turnidge)
- Spillover could be a reason for this
- If you look across many demes for a use-resistance relationship, it's also going to be weaker than you would expect because of spillover
    - Simulations suggest that the cross-sectional use-resistance relationship measured across states is X% weaker than it would be if those populations were totally isolated. (This is a new simulation, joining ~50 populations with the known commuting network, and comparing that "measured" use/res association against if you set ε=0.)
    - This might be part of the enduring puzzle of why observational use-resistance associations look so weak.
    - But it's really hard to think about power in this case, so let's imagine you did something simpler, with pairs of demes.
- If you observationally compare pairs of states (or Census tracts, or demes of any kind) with high contrast in use, we expect dilution of the contrast in resistance.
    - But you won't find it in a reasonable study.
    - Also, we checked, and we didn't find it.
- If you performed a trial randomizing demes to higher/lower use and compare resistance, we come to the same theoretical conclusion: spillover is likely important, but it's very difficult to measure, unless you sample to a crazy level.
- Conclusions
    - Spillover is a likely cause for the apparently weak use/res associations (cf. our eLife thing)
    - Spillover will likely dilute the results of any geographical- or population-based stewardship trials
    - However, spillover is very difficult to detect when measuring prevalence of resistance alone.
    - Path forward to quantifying this likely has to do with sequencing and figuring out population structure, measuring how much particular strains have moved around, and use that as the proxy to estimate the effect of spillover. (But that's way harder!)

I like this framing because **it addresses a long-standing puzzle**. I can cite
the comment on the Goossens plot, which says measuring use/res is hard because
"human population densities might also help to spread resistance more widely,3
as might the number of children in day-care centres,4 and rates of vaccine
uptake.5" Now we can actually put a number on that!

## Power calculations

- N (number of independent pairs)
- Δρ (difference in resistance between populations without spillover): I would guess that the cross-state association Δρ/Δτ also applies at the Census tract level, and use the Δτ measured in the OFID piece
- σ_ρ (the accuracy of measurements of resistance): This and N seem like the key adjustable parameters in an experimental design. There are fewer people in a Census tract than a state, so maybe it's easier to get a simple random sample? (Or maybe there's actually more noise in smaller groups?)
- ε_0 (the "strength" of spillover): Guess that the WHN & D-types models, although design for European countries, also work for Census tracts
ε (the number of cross-population contacts): This would require introducing a new data source. I could ask Stephen about estimates for this.

# Notes

- Not going to use Europe commuting data, as it's too sparse
