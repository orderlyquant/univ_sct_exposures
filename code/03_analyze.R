source("code/00_dependencies.R")

# board <- pins::board_folder("~/R/000_pins/cam-stock")
board <- pins::board_folder("/Volumes/Users/BFarr/000_pins/cam-stock/")

pins::pin_list(board)

wts_tbl <- pins::pin_read(board, "weights")
exp_tbl <- pins::pin_read(board, "security-exposures")

acct_name  <- "Attribution ACTM for Large Cap"
bench_name <- "Russell 1000 Growth"
sector_name <- "Information Technology"

# styles <- frsAttr::axioma_factor_levels_tbl |>
#   filter(factor_type == "Style")

styles <- c(
  "Earnings Yield", "Growth",
  "Leverage", "Profitability",
  "Market Sensitivity", "Medium-Term Momentum"
)

prep_sector_data <- function(w_tbl, e_tbl, acct_name, bench_name, sct, styles) {
  
  acct_tbl  <- w_tbl |> filter(acct == acct_name)  |> filter(sector == sct)
  bench_tbl <- w_tbl |> filter(acct == bench_name) |> filter(sector == sct)
  
  sector_names <- union(
    acct_tbl  |> filter(sector == sct) |> pull(name),
    bench_tbl |> filter(sector == sct) |> pull(name)
  ) |> sort()
  
  styles_exp_tbl <- e_tbl |>
    filter(factor %in% styles) |> 
    mutate(factor = factor(factor, styles))
  
  sct_styles_exp_tbl <- styles_exp_tbl |>
    filter(security %in% sector_names)
  
  return(
    list(
      acct_hld = acct_tbl,
      bench_hld = bench_tbl,
      sector_names = sector_names,
      sector_exposures = sct_styles_exp_tbl
    )
  )
  
}

jj <- prep_sector_data(
  w_tbl = wts_tbl, e_tbl = exp_tbl,
  acct_name = acct_name, bench_name = bench_name,
  sct = sector_name,
  styles = styles
)




# graphical representation ------------------------------------------------

density_adj <- 1.5



all_styles_exp_tbl |>
  ggplot(
    aes(x = value)
  ) +
  geom_vline(xintercept = 0, color = "gray40", linewidth = 0.7) +
  geom_density(color = NA, fill = "gray85", adjust = density_adj) +
  geom_density(
    data = tec_styles_exp_tbl |>
      semi_join(
        bench_tbl, by = c("security" = "name")
      ),
    aes(x = value),
    color = "dodgerblue",
    adjust = density_adj,
    linewidth = 1
  ) +
  geom_density(
    data = tec_styles_exp_tbl |>
      semi_join(
        acct_tbl, by = c("security" = "name")
      ),
    aes(x = value),
    color = "orange",
    adjust = density_adj,
    linewidth = 1
  ) +
  facet_wrap(~factor, dir = "v", nrow = 2) +
  labs(
    x = NULL, y = NULL
  ) +
  theme_minimal_hgrid() +
  theme(axis.text.y = element_blank())




# numeric representation --------------------------------------------------

summary_tbl <- bind_rows(
  all_styles_exp_tbl |>
    group_by(factor) |>
    summarize(
      avg = mean(value),
      med = median(value)
    ) |>
    mutate(type = "all"),
  tec_styles_exp_tbl |> semi_join(
    acct_tbl, by = c("security" = "name")
  ) |>
    group_by(factor) |>
    summarize(
      avg = mean(value),
      med = median(value)
    ) |>
    mutate(type = "acct_tec"),
  tec_styles_exp_tbl |>
    semi_join(
      bench_tbl, by = c("security" = "name")
    ) |>
    group_by(factor) |>
    summarize(
      avg = mean(value),
      med = median(value)
    ) |>
    mutate(type = "bench_tec")
)

summary_tbl |>
  select(-med) |>
  pivot_wider(names_from = type, values_from = avg)

summary_tbl |>
  select(-avg) |>
  pivot_wider(names_from = type, values_from = med)


# security details --------------------------------------------------------

tec_styles_exp_tbl |>
  semi_join(acct_tbl, by = c("security" = "name")) |>
  select(security, factor, value) |>
  pivot_wider(names_from = factor, values_from = value)



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



