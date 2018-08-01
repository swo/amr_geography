#!/usr/bin/env Rscript

# Aggregate states into larger units at random to see if there's some systematic effects
# - Clustering truly at random gives *smaller* slopes for larger aggregates (i.e., fewer aggregates)
# - Clustering zones (adjacent) gives similar or slightly larger slopes for larger aggregates

res = read_tsv('data/abg_state.tsv') %>%
  filter(bug == 'E. coli', drug_group == 'quinolone') %>%
  mutate(n_res = as.integer(round(f_ns * n_isolates))) %>%
  select(state, n_res, n_isolates)

use = read_tsv('data/marketscan_abxdist.tsv') %>%
  filter(drug_group == 'quinolone') %>%
  group_by(state, year) %>%
  summarize(n_rx = sum(n_rx * n_member),
            n_member = sum(n_member)) %>%
  summarize_at(vars(n_rx, n_member), function(x) as.integer(round(mean(x)))) %>%
  ungroup() %>%
  filter(state %in% res$state)

ur = inner_join(use, res, by='state')

# census regions
cr = read_tsv('census-regions.tsv') %>%
  select(state, region, division=division_name)

cat('state-level (44 aggregates)')
state_results = ur %>%
  mutate(rky = n_rx / n_member * 1000, pct_res = 100 * n_res / n_isolates) %>%
  lm(pct_res ~ rky, data=.)

state_results %>%
  summary

cat('region-level (4 zones)')
region_results = ur %>%
  left_join(cr, by='state') %>%
  group_by(region) %>%
  summarize_at(vars(starts_with('n_')), sum) %>%
  mutate(rky = n_rx / n_member * 1000, pct_res = 100 * n_res / n_isolates) %>%
  lm(pct_res ~ rky, data=.)

region_results %>%
  summary()

cat('division-level (9 zones)')
division_results = ur %>%
  left_join(cr, by='state') %>%
  group_by(division) %>%
  summarize_at(vars(starts_with('n_')), sum) %>%
  mutate(rky = n_rx / n_member * 1000, pct_res = 100 * n_res / n_isolates) %>%
  lm(pct_res ~ rky, data=.)

division_results %>%
  summary()

# random sampling (not adjacent)

sample_zone = function(n_units, n_zones) {
  stopifnot(n_units >= n_zones)

  # each zone must appear at least once
  fixed_idx = 1:n_zones

  # remaining units assigned to zones at random
  random_idx = sample(n_zones, n_units - n_zones, replace=TRUE)

  # mix up the fixed and random indices
  idx = sample(c(fixed_idx, random_idx))

  idx
}

f = function(n_zones) {
  zone = sample_zone(nrow(ur), n_zones)

  ur %>%
    mutate(zone = zone) %>%
    group_by(zone) %>%
    summarize_at(vars(starts_with('n_')), sum) %>%
    mutate(rky = n_rx / n_member * 1000, pct_res = 100 * n_res / n_isolates) %>%
    lm(pct_res ~ rky, data=.) %>%
    coef %>%
    { .['rky'] }
}

g = function(n_zones, n_trials) {
  x = replicate(n_trials, f(n_zones))
  data_frame(n_zones=n_zones, mean=mean(x), std=sd(x))
}

cat('random results')
list_n_zones = c(4, 9, 15, 20, 25, 30, 35, 40, 44)
#list_n_zones = c(4, 9, 44)
n_trials = 100
sample_results = lapply(list_n_zones, function(x) g(x, n_trials)) %>%
  bind_rows() %>%
  mutate(std = if_else(n_zones == 44, 0, std))

write_tsv(sample_results, 'sample_results.tsv')

sample_results

# random zones
usa = map_data('state') %>%
  as_tibble() %>%
  select(state=region, long, lat) %>%
  mutate(state = tools::toTitleCase(state)) %>%
  group_by(state) %>%
  summarize_at(vars(long, lat), mean)

h = function(k) {
  # select k states to be the centers of the zones
  centers = sample_n(usa, k) %>%
    mutate(zone = LETTERS[1:k]) %>%
    select(zone, long, lat)

  # then cluster the states into those zones
  zones = crossing(usa, centers) %>%
    mutate(dist = sqrt((long - long1) ** 2 + (lat - lat1) ** 2)) %>%
    group_by(state) %>%
    filter(dist == min(dist)) %>%
    ungroup() %>%
    select(zone, state)

  # group the states and do the analysis
  ur %>%
    left_join(zones, by='state') %>%
    group_by(zone) %>%
    summarize_at(vars(starts_with('n_')), sum) %>%
    mutate(rky = n_rx / n_member * 1000, pct_res = 100 * n_res / n_isolates) %>%
    lm(pct_res ~ rky, data=.) %>%
    coef %>%
    { .['rky'] }
}

j = function(n_zones, n_trials) {
  x = replicate(n_trials, h(n_zones))
  data_frame(n_zones = n_zones, mean = mean(x), std = sd(x))
}

zone_results = lapply(list_n_zones, function(x) j(x, n_trials)) %>%
  bind_rows() %>%
  mutate(std = if_else(n_zones == 44, 0, std))

write_tsv(zone_results, 'zone_results.tsv')

zone_results

results = bind_rows(
  sample_results %>% mutate(type='sample'),
  zone_results %>% mutate(type='zone'),
  #data_frame(type='region', n_zones=4, mean=coef(region_results)['rky'], std=0),
  #data_frame(type='division', n_zones=9, mean=coef(division_results)['rky'], std=0)
)

results %>%
  mutate(hci = std / 2,
         ymin = mean - hci,
         ymax = mean + hci) %>%
  ggplot(aes(n_zones, mean, color=type)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=ymin, ymax=ymax)) +
  geom_point(data=data_frame(type='states', n_zones=44, mean=coef(state_results)['rky']), shape=1, size=3, color='black') +
  geom_point(data=data_frame(type='region', n_zones=4, mean=coef(region_results)['rky']), shape=1, size=3, color='black') +
  geom_point(data=data_frame(type='division', n_zones=9, mean=coef(division_results)['rky']), shape=1, size=3, color='black') +
  xlab('number of aggregations') +
  ylab('mean regression slope (SD)')
