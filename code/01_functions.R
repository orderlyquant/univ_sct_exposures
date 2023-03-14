prep_sector_data <- function(w_tbl, e_tbl, acct_name, bench_name, sct, styles) {
  
  # gen_port_set(wts_tbl, exp_tbl, acct_name)
  # gen_port_set(wts_tbl, exp_tbl, bench_name)
  
  acct_tbl  <- w_tbl |> filter(acct == acct_name)
  bench_tbl <- w_tbl |> filter(acct == bench_name)
  
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
      acct = acct_name,
      bench = bench_name,
      sector = sct,
      acct_hld = acct_tbl,
      bench_hld = bench_tbl,
      sector_names = sector_names,
      sector_exposures = sct_styles_exp_tbl,
      univ_exposures = styles_exp_tbl
    )
  )
  
}

gen_key_exposures_plot <- function(exp_data, density_adj = 1.5) {
  
  univ_exp <- exp_data$univ_exposures
  
  sector_exp <- bind_rows(
    exp_data$sector_exposures |>
      semi_join(exp_data$acct_hld, by = c("security" = "name")) |> 
      mutate(type = exp_data$acct) |> 
      select(security, factor, value, type),
    exp_data$sector_exposures |>
      semi_join(exp_data$bench_hld, by = c("security" = "name")) |> 
      mutate(type = exp_data$bench) |> 
      select(security, factor, value, type)
  )
  
  exp_data$univ_exposures |>
    ggplot(
      aes(x = value)
    ) +
    geom_vline(xintercept = 0, color = "gray40", linewidth = 0.7) +
    geom_density(color = NA, fill = "gray85", alpha = 0.7, adjust = density_adj) +
    geom_density(
      data = sector_exp,
      aes(x = value, fill = type),
      adjust = density_adj,
      color = NA,
      alpha = 0.5
    ) +
    facet_wrap(~factor, dir = "v", nrow = 2) +
    labs(
      title = exp_data$sector,
      # subtitle = glue("{exp_data$acct} (+ {exp_data$bench})"),
      subtitle = "Key Factor Exposures",
      x = NULL, y = NULL, color = NULL, fill = NULL
    ) +
    theme_minimal_hgrid() +
    theme(
      axis.text.y = element_blank(),
      legend.position = "bottom"
    ) +
    scale_fill_oq()
  
}

gen_summary_tbl <- function(exp_data) {
  
  summary_tbl <- bind_rows(
    exp_data$univ_exposures |> 
      group_by(factor) |>
      summarize(
        avg = mean(value),
        med = median(value)
      ) |>
      mutate(type = "Univ"),
    exp_data$sector_exposures |> semi_join(
      exp_data$acct_hld, by = c("security" = "name")
    ) |>
      group_by(factor) |>
      summarize(
        avg = mean(value),
        med = median(value)
      ) |>
      mutate(type = exp_data$acct),
    exp_data$sector_exposures |>
      semi_join(
        exp_data$bench_hld, by = c("security" = "name")
      ) |>
      group_by(factor) |>
      summarize(
        avg = mean(value),
        med = median(value)
      ) |>
      mutate(type = exp_data$bench)
  )
  
  summary_tbl |>
    select(-med) |>
    pivot_wider(names_from = type, values_from = avg)
  
  # summary_tbl |>
  #   select(-avg) |>
  #   pivot_wider(names_from = type, values_from = med)
  
}

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
