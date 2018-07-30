# Data sources

- S. pneumoniae and beta-lactams
- S. pneumoniae and macrolides
- E. coli and quinolones


Data source         Setting                  Time      Bugs & drugs
-----------         -------                  ----      ------------
Arason BMJ 1996     5 Icelandic communities  1992-1993 *S. pneumoniae* and beta-lactams
Kalhmeter JAC 2003  14 European countries    1997-2000 *E. coli* and quinolones
Bronzwaer EID 2002  11 European countries    1997-1999 *S. pneumoniae* and beta-lactams
Garcia-Rey JCM 2002 13 Spanish provinces     1998-1999 *S. pneumoniae* and beta-lactams, macrolides

## Cross-country

### Raw data

- ECDC
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides
    - E. coli and quinolones

### Extract

- Goossens 2005
    - S. pneumoniae and beta-lactams

## Within European countries

### Raw data

- Garcia-Rey JCM 2002 (1998-9 use and resistance; 13 sites in Spain)
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides
- Garcia-Rey JAC 2004 (2002 data)
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides

### Extract from figures

- Pihlajamaki (22 sites in Finland)
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides
- Priest; McGee; Ironmonger; Pouwels?: UTIs, so mostly E. coli

### Unclear

- Bergman AAC 2006 has S. pneumoniae and macrolides, ~ and beta-lactams. Linear models are reported, but with that year term. No raw data.
- Bergman AAC 2009 (Finland). Linear models, but with year term. No raw data.
    - E. coli and TMP/SMX
    - E. coli and nitrofurantoin
    - E. coli and quinolones
    - E. coli and beta-lactams
    - E. coli and quinolones
    - E. coli and nitrofurantoin
- Priest BMJ 2001 has linear models but no raw data. But does show that regressions at different scales give similar results!
    - S. pneumoniae and beta-lactams
    - S. pneumoniae and macrolides
    - Also E. coli and TMP/SMX?

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
