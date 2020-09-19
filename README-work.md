If you take the `between.R` script and put in a lot more alpha's, then you
can ask how changing eps0 alters the slope and variance in the
cross-population line: more interaction means weaker slope and more variance.
So you could say something like, if you attribute 100% of the observed
variance in our data to spillover, it means that the causal relationship is
4-10x larger than the observed one!

```{r}
results %>%
  mutate(
    model = map(unit_data, ~ lm(res ~ use, data = .)),
    slope = map_dbl(model, ~ coef(.)["use"]),
    r2 = map_dbl(model, ~ summary(.)$r.squared)
  ) %>%
  select(alpha, slope, r2) %>%
  ggplot(aes(r2, slope)) +
  geom_point()
```

More cautiously, we say we don't know what fraction is due to noise, etc., so
that is the absolute upper limit.
