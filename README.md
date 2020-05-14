# To do

## 14 May

Use a Mantel test! This will compare the matrix of interactions (ε) against the
matrix of use/resistance associations (Δρ/Δτ). If we use Spearman metric, then
no need to worry about whether its log odds of Δρ/Δτ or whatever

## 7 May

Analyses should be done. Now it's about stitching together the new manuscript.

Include a point that says that, given that adjacent US states have epsilon ~
10^{-2.5} and non-adjacent have 10^-4, we expect 0.4% reduction in dr/du vs.
ε=0 and 29%, respectively. So basically we should be getting a big decline. Use
that in the power calculations?

But not sure if I can read off ε as easily from the commuting data as I thought I could...

## Methods

For flights, say that flights relate to contacts by some proportion α. Then, a
fraction β of internal contacts are represented by internal flights. Then c_ij
(contacts) = α * t_ij (flights) and c_ii = α / β * t_ii.

Show that US commuting and EU flights are pretty similarly distributed, if you
allow that *internal* flights represent a small fraction (0.1%) of all internal
contacts. Seems reasonable. Commuting seems like an easier thing to imagine.

(Compare US flight pattern? Point isn't exactly what the network is, it's what
effect it could have.)

## (new) framing

- Not a big problem in terms of X-sectional observations across large populations, like states. Could become problematic for smaller units, like cities or states.
- However, hard to say it's *not* a problem, even for states, because very challenging to observe this effect, with so many unknowns.
- Frame this as an opinion piece or something?

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
