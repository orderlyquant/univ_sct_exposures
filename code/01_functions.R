gen_port_set <- function(w_tbl, e_tbl, port) {
  w_tbl |>
    filter(acct == port) |>
    select(ticker, name, sector, weight) |>
    left_join(
      e_tbl |>
        rename(name = security) |>
        rename(exposure = value) |>
        select(name, factor, exposure),
      by = "name",
      multiple = "all"
    )
}

prep_exposure_data <- function(w_tbl, e_tbl, acct_name, bench_name, styles) {
  
  acct_data  <- gen_port_set(w_tbl, e_tbl, acct_name) |> 
    filter(factor %in% styles) |> 
    mutate(factor = factor(factor, styles))
  bench_data <- gen_port_set(w_tbl, e_tbl, bench_name) |> 
    filter(factor %in% styles) |> 
    mutate(factor = factor(factor, styles))
  
  return(
    list(
      acct = acct_name,
      acct_data = acct_data,
      bench = bench_name,
      bench_data = bench_data
    )
  )
  
}

gen_key_exposures_plot <- function(e_data, sct, density_adj = 1.5) {
  
  type_codes <- c(
    glue("{lu_port_code(e_data$bench)}: {lu_sector_code(sct)}"),
    glue("{lu_port_code(e_data$acct)}: {lu_sector_code(sct)}")
  )
  
  sector_data <- bind_rows(
    e_data$acct_data |>
      filter(sector == sct) |> 
      mutate(type = type_codes[2]) |>
      select(type, name, factor, exposure),
    e_data$bench_data |>
      filter(sector == sct) |> 
      mutate(type = type_codes[1]) |>
      select(type, name, factor, exposure)
  ) |> 
    mutate(
      type = factor(type, type_codes)
    )
  
  e_data$bench_data |> 
    ggplot(
      aes(x = exposure)
    ) +
    # geom_vline(xintercept = 0, color = "gray40", linewidth = 0.7) +
    geom_density(color = NA, fill = "gray60", adjust = density_adj) +
    geom_density(
      data = sector_data,
      aes(x = exposure, fill = type),
      adjust = density_adj,
      color = NA,
      alpha = 0.7
    ) +
    facet_wrap(~factor, dir = "v", nrow = 2) +
    # scale_x_continuous(limits = c(-3, 3)) +
    scale_x_continuous(breaks = seq(-3, 3, 1)) +
    coord_cartesian(xlim = c(-3.25, 3.25)) +
    labs(
      title = sct,
      subtitle = "Key Factor Exposures",
      x = NULL, y = NULL, color = NULL, fill = NULL
    ) +
    theme_minimal_grid() +
    theme(
      axis.text.y = element_blank(),
      legend.position = "bottom",
      panel.spacing = unit(2, "lines")
    ) +
    # scale_fill_oq()
    scale_fill_manual(
      values = c("dodgerblue", "orange")
    )
  
}

gen_summary_tbl <- function(e_data, sct) {
  
  type_codes <- c(
    glue("{lu_port_code(e_data$acct)}"),
    glue("{lu_port_code(e_data$bench)}"),
    glue("{lu_port_code(e_data$acct)}: {lu_sector_code(sct)}"),
    glue("{lu_port_code(e_data$bench)}: {lu_sector_code(sct)}")
  )
  
  
  bind_rows(
    e_data$acct_data |>
      group_by(factor) |>
      summarize(
        # avg = mean(exposure),
        # med = median(exposure),
        wtd_exp = sum(weight * exposure) / sum(weight)
      ) |> 
      mutate(
        type = type_codes[1]
      ),
    
    e_data$bench_data |>
      group_by(factor) |>
      summarize(
        # avg = mean(exposure),
        # med = median(exposure),
        wtd_exp = sum(weight * exposure) / sum(weight)
      ) |> 
      mutate(
        type = type_codes[2]
      ),
    
    e_data$acct_data |> 
      filter(sector == sct) |> 
      group_by(factor) |> 
      summarize(
        # avg = mean(exposure),
        # med = median(exposure),
        wtd_exp = sum(weight * exposure) / sum(weight)
      ) |> 
      mutate(
        type = type_codes[3]
      ),
    
    e_data$bench_data |>
      filter(sector == sct) |>
      group_by(factor) |>
      summarize(
        # avg = mean(exposure),
        # med = median(exposure),
        wtd_exp = sum(weight * exposure) / sum(weight)
      ) |> 
      mutate(
        type = type_codes[4]
      )
  ) |> 
    mutate(type = factor(type, type_codes)) |> 
    pivot_wider(
      names_from = type,
      values_from = wtd_exp
    )
  
}

gen_sector_security_tbl <- function(e_data, e_tbl, sct) {
  
  all_sector_security_tbl <- bind_rows(
    e_data$acct_data |> 
      filter(sector == sct) |> 
      group_by(name) |> 
      summarize(
        weight = first(weight),
        type = "acct_wt"
      ),
    e_data$bench_data |> 
      filter(sector == sct) |> 
      group_by(name) |> 
      summarize(
        weight = first(weight),
        type = "bench_wt"
      )
  ) |> 
    pivot_wider(
      names_from = type,
      values_from = weight
    )
  
  all_security_exposure_tbl <- e_tbl |>
    rename(name = security) |> 
    filter(name %in% all_sector_security_tbl$name) |> 
    filter(factor %in% styles) |> 
    select(name, factor, value) |> 
    pivot_wider(
      names_from = factor,
      values_from = value
    )
  
  all_sector_security_tbl |>
    left_join(
      all_security_exposure_tbl, by = "name"
    ) |> 
    arrange(desc(acct_wt))
  
}


lu_port_code <- function(port_name) {
  
  lu_tbl <- tibble::tribble(
    ~full_name, ~abbv_name,
    "Attribution ACTM for Large Cap",      "LCG",
    "Attribution ACTM for Multicap",      "MULTI",
    "Attribution ACTM for SMID Cap",     "SMID",
    "Russell 1000 Growth",   "R1000G",
    "Russell 3000 Growth",   "R3000G",
    "Russell 2500 Growth",   "R2500G",
    "iShares Core S&P 500 ETF",  "S&P 500"
  )
  
  lu_tbl |>
    filter(full_name == port_name) |> 
    pull(abbv_name)
  
}

lu_sector_code <- function(sector_name) {
  
  lu_tbl <- tibble::tribble(
    ~full_name, ~abbv_name,
    "Communication Services",      "COM",
    "Consumer Discretionary",      "CDS",
    "Consumer Staples",      "CST",
    "Energy",      "ENE",
    "Financials",      "FIN",
    "Health Care",      "HEA",
    "Industrials",      "IND",
    "Information Technology",      "TEC",
    "Materials",      "MAT",
    "Real Estate",      "REA",
    "Utilities",      "UTL"
  )
  
  
  lu_tbl |>
    filter(full_name == sector_name) |> 
    pull(abbv_name)
  
}
