Resistance data is from Table 1. I just kept NAL=nalidixic acid and CIP=cipro.

Use data is from the stacked barchart Figure 1. I extracted the top and bottom points of each quinolones cross-hatched box and subtracted them. Countries with no visible box I gave 0 DID.

```
inner_join(u, r, by='country') %>%
  group_by(year, drug) %>%
  do(tidy(lm(pct_res ~ did, data=.), conf.int=TRUE)) %>%
  ungroup() %>%
  filter(term=='did') %>%
  ggplot(aes(interaction(year, drug), estimate, ymin=conf.low, ymax=conf.high)) +
  geom_point() +
  geom_errorbar()
```
