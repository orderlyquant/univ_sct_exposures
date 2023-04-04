source("code/00_dependencies.R")
source("code/01_functions.R")

board <- pins::board_folder("~/R/000_pins/cam-stock")
# board <- pins::board_folder("/Volumes/Users/BFarr/000_pins/cam-stock/")

pins::pin_list(board)

wts_tbl <- pins::pin_read(board, "weights")
exp_tbl <- pins::pin_read(board, "security-exposures")
char_tbl <- pins::pin_read(board, "characteristics")

acct_name  <- "Attribution ACTM for Multicap"
bench_name <- "Russell 3000 Growth"
sector_name <- "Information Technology"
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

gen_key_exposures_plot(exp_data, sector_name)

# numeric representation --------------------------------------------------

gen_summary_tbl(exp_data, sector_name)

# security details --------------------------------------------------------

sector_sec_tbl <- gen_sector_security_tbl(
  e_data = exp_data,
  e_tbl = exp_tbl,
  sct = sector_name
)

full_sec_tbl <- bind_rows(
  sector_sec_tbl |> 
    mutate(
      focus = if_else(!is.na(acct_wt), 1, 0)
    #) |> 
    # mutate(
    #   focus = if_else(
    #     str_detect(name, "Trade Desk|Arista"), 1, focus
    #   )
    ),
  
  meta_tbl <- gen_sector_security_tbl(
    e_data = exp_data,
    e_tbl = exp_tbl,
    sct = "Communication Services"
  ) |> 
    filter(
      str_detect(name, "Meta Platform")
    ) |> 
    mutate(focus = 1)
) |> 
  arrange(desc(acct_wt), desc(focus))

full_sec_tbl |> 
  left_join(
    char_tbl |> 
      select(name, free_cash_flow_yield),
    by = "name"
  ) |> 
  rename(FCFYield = free_cash_flow_yield) |> 
  relocate(focus, .after = name)
