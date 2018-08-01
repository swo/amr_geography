# To do

- What's up with ECDC vs. Kahlmeter?
- Check on the non-finite values in the combined plot
- Consider random aggregations of HRRs, comparing what happens with the slopes? MAUP is tough

# Introduction

Say we want to do an outpatient antibiotic use intervention. At what length scale do we need to do the intervention to see an effect? How long does the intervention (or the wait until the effect) need to be?

- Vellinga *JAC* 2010 speculates that smaller geographic scales produce more positive slopes
- Donnan *BMJ* 2004 has some good discussion about why ecological studies have problems, since there's variation in use among the people making up the population. Some careful thought about practice vs. individual-level results.
- Openshaw "MAUP": correlation coefficients differ with the areal unit (tending to increase with larger units) and are relative only to the unit used. They don't signal anything fundamental about the process. So for p-values it's a trade: larger $\rho$ with smaller $n$. "[B]ecause the correlations are modifiable they may not provide any useful guide to individual or more spatially disaggregated levels of correlations." E.g., Robinson's nativity and illiteracy; although AMR is different because of transmission.
    - Robinson's negro & illiteracy is the same as our race & AMR: more blacks, who use less abx, in the South, where there is more AMR

# Methods

## Data sources

Limited ourselves to three bug/drug combos because there's a lot of literature about them, and they include some of the most-cited literature:

- *S. pneumoniae* and beta-lactams
- *S. pneumoniae* and macrolides
- *E. coli* and quinolones

(Maaaaaybe E. coli and trimethoprim)

Also limited to studies that have directly accessible data (raw in tables or extractable from plots) or that reported the results of single-variable regressions (resistance against use).

When I have access to the raw data (either just reported in a table in the paper or extracted from the figures):

Data source          Setting                               Time      Bugs & drugs
-----------          -------                               ----      ------------
Arason BMJ 1996      5 Icelandic communities               1992-1993 *S. pneumoniae* and beta-lactams
Kalhmeter JAC 2003   14 European countries                 1997-2000 *E. coli* and quinolones
Bronzwaer EID 2002   11 European countries                 1997-1999 *S. pneumoniae* and beta-lactams
Garcia-Rey JCM 2002  13 Spanish provinces                  1998-1999 *S. pneumoniae* and beta-lactams, macrolides
Goossens Lancet 2005 19 European countries                 2000-2001 *S. pneumoniae* and beta-lactams
Pihljamaki CID 2001  10 Finnish central hospital districts 1995-1997 *S. pneumoniae* and macrolides
""                   18 Finnish central hospital districts ""        *S. pneumoniae* and beta-lactams
NHSN & IMS           50 US states and DC                   2011-2014 *E. coli* and quinolones
CDDEP                9 US Census divisions                 1999-2012 *S. pneumoniae* and macrolides, beta-lactams; *E. coli* and quinolones
European CDC         13-25 European countries              2001-2015 *E. coli* and quinolones
""                   15-24 European countries              2005-2015 *S. pneumoniae* and beta-lactams
""                   16-24 European countries              2005-2015 *S. pneumoniae* and macrolides

NHSN/IMS includes DC. IMS data includes 2015, but NHSN doesn't, so I didn't use
that year.

CDDEP has a lot of years. Maybe break those down a little in an intelligent
way, to make it comparable to other data sets? Lots of methodological points there.

To convert US to European values, CDDEP is a Rosetta stone? Then do a sensitivity analysis to show that it's fine.

When I don't have the raw data and have to rely on their regressions:

Data source         Setting                                           Time      Bugs & drugs
-----------         -------                                           ----      ------------
Priest BMJ 2001     262 practices grouped into 20 primary care groups 1996-1998 Urinary coliforms and beta-lactams
                    371 practices grouped into 32 primary care groups           Urinary coliforms and trimethoprim

Priest *et al*. has some data about pneumococcus, but they do a trichotomous
grouping and $\chi$-square test. Could do some crazy bootstrapping to get
those. Definitely worth discussing their findings, since they get similar
results at the two scales!

## Cross-country

## Across US states

- MarketScan and ResistanceOpen
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides
    - E. coli and quinolones

Redo the Medicare thing, but rather than doing correlations, do slopes!

## Within US states

- MDPH and CHIA
    - E. coli

# Methods

- Say something about the data sources and years for each data source
- In a sensitivity analysis, where possible, compare the weight and unweighted regressions. I expect they give similar results.
- Show that it makes sense to average over multiple years. Like, do some funny GEE thing showing that that's OK for the CDDEP and ECDC stuff

Drug                        PKY:DID
--------------------------- -------
Broad-spectrum penicillins  19
Narrow-spectrum penicillins 8
Quinolones                  10
Macrolides                  7

Table: Ratio of prescriptions per 1,000 people per year to DDD per 1,000 people per day. I got this from the CDDEP Rosetta Stone, averaging over 2000 and 2012. Should probably do something a little more principled.

# Discussion

- Size of x-axis (wider ranges with same slope give smaller p-values)
    - E.g., look at van de Sande-Bruinsma: penicillin use varies 28-fold, cephalosporins 100-fold!
    - Different from Priest's point about the S-curve, so that we might expect the slopes away from the midpoint to be less steep
- What does "strength" mean, as in Bell? Could mean that there is more confidence of a relationship (i.e., p-value for correlation different or slope from zero) or it could mean the slope itself. Here we focused on slope because we expect it could be similar across dynamic ranges, etc.
    - Can we say that Ec/FQ is stronger than the Sp/bL relationship?
    - What does it mean that relationships are "stronger" in Southern Europe? If it's just about p-value, then that has to do with the range of the x-axis
    - Strength could have to do with who takes that drug. Say only children take a drug or have a resistance. Then a large increase in children's use looks like a small increase in overall use but a large change in resistance, giving a big slope.
    - Conversely, if you measure use among high users (e.g., older adults) and resistance among everyone, you'll get the same amount of resistance for higher use, i.e., a weaker slope.
    - Regression dilution: if there are errors on the x-axis, then slope is biased toward smaller values
- Analogy with temporal. It's hard to know if cross-sectional relationships should also be causal, and if so, on what scales. How widely does the change have to be implemented? Analogously, how long do you have to wait to see a change?

# Not using

- Kalhmeter, Menday, Cars (1997 use) only reports correlation coefficients
- Bergman CID 2004 (Finland) has raw data, but only S. pyogenes and macrolides
- van de Sande-Bruinsma, because they only do correlation coefficients
- Ironmonger does E. coli (not quinolones) and has a funny seasonal thing
- UK UTI nexus: Magee, Priest, Ironmonger, Pouwels
- Garcia-Rey 2004, because they report use in "units". I can't use the same conversions as for the US and get reasonable results, so I'd have to use G-R 2002 as the Rosetta Stone, which means I'm basically using two periods of data from the same stream.

## Unclear

The two Bergman papers have regression $\rho ~ \tau + t$. I'm thinking to only
put in studies that have univariate regressions $\rho ~ \tau$. I'm not sure
what adding $t$ would do. In practice, it shouldn't do anything to the
cross-sectional relationship, since we could pick a year and then we'd get back
to $\rho ~ \tau$, but I'm nervous about any model mis-specification. (And I
don't want to look to see if it supports my conclusions before deciding whether
to include it!)

- Bergman AAC 2006 has S. pneumoniae and macrolides, ~ and beta-lactams. Linear models are reported, but with that year term. No raw data.
- Bergman AAC 2009 (Finland). Linear models, but with year term. No raw data.
    - E. coli and TMP/SMX
    - E. coli and nitrofurantoin
    - E. coli and quinolones
    - E. coli and beta-lactams
    - E. coli and quinolones
    - E. coli and nitrofurantoin
