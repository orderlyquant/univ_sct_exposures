source("code/00_dependencies.R")
source("code/01_functions.R")

# board <- pins::board_folder("~/R/000_pins/cam-stock")
board <- pins::board_folder("/Volumes/Users/BFarr/000_pins/cam-stock/")

pins::pin_list(board)

wts_tbl <- pins::pin_read(board, "weights")
exp_tbl <- pins::pin_read(board, "security-exposures")

acct_name  <- "Attribution ACTM for Large Cap"
bench_name <- "Russell 1000 Growth"
# bench_name <- "iShares Core S&P 500 ETF"

# styles <- frsAttr::axioma_factor_levels_tbl |>
#   filter(factor_type == "Style")

styles <- c(
  "Earnings Yield", "Growth",
  "Leverage", "Profitability",
  "Market Sensitivity", "Medium-Term Momentum"
)

exp_data <- prep_exposure_data(
  w_tbl = wts_tbl, e_tbl = exp_tbl,
  acct_name = acct_name, bench_name = bench_name,
  styles = styles
)



# graphical representation ------------------------------------------------


gen_key_exposures_plot(exp_data, "Information Technology")
# gen_key_exposures_plot(exp_data, "Consumer Discretionary")


# numeric representation --------------------------------------------------

gen_summary_tbl(exp_data, "Information Technology")
# gen_summary_tbl(exp_data, "Consumer Discretionary")


# security details --------------------------------------------------------

gen_sector_security_tbl(
  e_data = exp_data,
  e_tbl = exp_tbl,
  sct = "Information Technology"
)

