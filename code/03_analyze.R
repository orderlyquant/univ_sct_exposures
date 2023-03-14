source("code/00_dependencies.R")
source("code/01_functions.R")

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


tec_data <- prep_sector_data(
  w_tbl = wts_tbl, e_tbl = exp_tbl,
  acct_name = acct_name, bench_name = bench_name,
  sct = sector_name,
  styles = styles
)

cds_data <- prep_sector_data(
  w_tbl = wts_tbl, e_tbl = exp_tbl,
  acct_name = acct_name, bench_name = bench_name,
  sct = "Consumer Discretionary",
  styles = styles
)


# graphical representation ------------------------------------------------


gen_key_exposures_plot(tec_data)
gen_key_exposures_plot(cds_data)





# numeric representation --------------------------------------------------

gen_summary_tbl(tec_data)


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



