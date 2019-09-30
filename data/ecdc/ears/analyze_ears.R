#!/usr/bin/env Rscript

safe_binconf = function(x, n) {
  stopifnot(length(x) == length(n))

  good_rows = !is.na(x) & x >= 0 & !is.na(n) & n > 0
  good_x = x[good_rows]
  good_n = n[good_rows]
  m = Hmisc::binconf(good_x, good_n, return.df=TRUE)

  nas = rep(NA, length(x))
  estimate = nas
  ci_lo = nas
  ci_hi = nas
  estimate[good_rows] = m$PointEst
  ci_lo[good_rows] = m$Lower
  ci_hi[good_rows] = m$Upper

  data_frame(estimate, ci_lo, ci_hi)
}

x = read_tsv('raw/ears.tsv') %>%
  spread(metric, value) %>%
  # if we don't have no. nonsusceptible, use no. resistant instead
  mutate(n_ns=pmax(n_ns, n_res, na.rm=TRUE)) %>%
  bind_cols(safe_binconf(.$n_ns, .$n_isolates))

p = x %>%
  ggplot(aes(x=country, y=estimate, ymin=ci_lo, ymax=ci_hi, color=factor(year))) +
  geom_point(shape=1, size=0.5) +
  geom_errorbar(width=0.2) +
  facet_grid(bug ~ drug) +
  coord_flip() +
  theme_classic() +
  theme(text=element_text(size=3))

ggsave('ears-plot.pdf', plot=p, useDingbats=FALSE, width=9.5, height=7.0, units='in')
