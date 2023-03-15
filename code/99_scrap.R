

# ranking mechanism -------------------------------------------------------

# idea: prioritize exposures (-0.3, -0.2, -0.1, 0.1, 0.2, 0.3) where
#   - magnitude represents preference
#   - sign indicates favor (+) or disfavor (-)

importance_tbl <- tibble::tribble(
  ~factor,                 ~importance,
  "Earnings Yield",                0.1,
  "Growth",                        0.2,
  # "Leverage",                     -0.1,
  "Leverage",                        0,
  # "Market Sensitivity",            0.1,
  "Market Sensitivity",              0,
  # "Medium-Term Momentum",         -0.1,
  "Medium-Term Momentum",            0,
  "Profitability",                 0.3
)

score_tbl <- tec_styles_exp_tbl |>
  semi_join(acct_tbl, by = c("security" = "name")) |>
  select(security, factor, value) |>
  left_join(importance_tbl) |>
  group_by(security) |>
  summarize(
    score = sum(value * importance)
  ) |>
  arrange(desc(score))

tec_styles_exp_tbl |>
  semi_join(acct_tbl, by = c("security" = "name")) |>
  select(security, factor, value) |>
  pivot_wider(names_from = factor, values_from = value) |>
  left_join(score_tbl, by = "security") |>
  arrange(desc(score))



