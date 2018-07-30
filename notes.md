# Data sources

- S. pneumoniae and beta-lactams
- S. pneumoniae and macrolides
- E. coli and quinolones

Maaaaaybe E. coli and trimethoprim

When I have access to the raw data (either just reported in a table in the paper or extracted from the figures):

Data source          Setting                               Time      Bugs & drugs
-----------          -------                               ----      ------------
Arason BMJ 1996      5 Icelandic communities               1992-1993 *S. pneumoniae* and beta-lactams
Kalhmeter JAC 2003   14 European countries                 1997-2000 *E. coli* and quinolones
Bronzwaer EID 2002   11 European countries                 1997-1999 *S. pneumoniae* and beta-lactams
Garcia-Rey JCM 2002  13 Spanish provinces                  1998-1999 *S. pneumoniae* and beta-lactams, macrolides
Garcia-Rey JAC 2004  14 Spanish provinces                  2001-2002 *S. pneumoniae* and beta-lactams, macrolides
Goossens Lancet 2005 19 European countries                 2000-2001 *S. pneumoniae* and beta-lactams
Pihljamaki CID 2001  10 Finnish central hospital districts 1995-1997 *S. pneumoniae* and macrolides
                     18 Finnish central hospital districts           *S. pneumoniae* and beta-lactams

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

### Raw data

- ECDC
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides
    - E. coli and quinolones

## Across US regions

- CDDEP data. Would require aggregating the states' use into the lower Census regions

## Across US states

- NHSN and IMS (or CDDEP)
    - E. coli CAUTI and quinolones
- MarketScan and ResistanceOpen
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides
    - E. coli and quinolones

Redo the Medicare thing, but rather than doing correlations, do slopes!

## Within US states

- MDPH and CHIA
    - E. coli

# Discussion

- Size of x-axis (wider ranges with same slope give smaller p-values)

# Not using

- Kalhmeter, Menday, Cars (1997 use) only reports correlation coefficients
- Bergman CID 2004 (Finland) has raw data, but only S. pyogenes and macrolides
- van de Sande-Bruinsma, because they only do correlation coefficients
- Ironmonger does E. coli (not quinolones) and has a funny seasonal thing

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
