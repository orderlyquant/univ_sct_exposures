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
  
  # univ_exp <- e_data$univ_exposures
  
  sector_data <- bind_rows(
    e_data$acct_data |>
      filter(sector == sct) |> 
      mutate(type = e_data$acct) |>
      select(type, name, factor, exposure),
    e_data$bench_data |>
      filter(sector == sct) |> 
      mutate(type = e_data$bench) |>
      select(type, name, factor, exposure)
  ) |> 
    mutate(
      type = factor(type, c(e_data$bench, e_data$acct))
    )
  
  e_data$bench_data |> 
    ggplot(
      aes(x = exposure)
    ) +
    # geom_vline(xintercept = 0, color = "gray40", linewidth = 0.7) +
    geom_density(color = NA, fill = "gray80", adjust = density_adj) +
    geom_density(
      data = sector_data,
      aes(x = exposure, fill = type),
      adjust = density_adj,
      color = NA,
      alpha = 0.75
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
      legend.position = "bottom"
    ) +
    scale_fill_oq()
  
}

gen_summary_tbl <- function(e_data, sct) {
  
  summary_tbl <- bind_rows(
    e_data$bench_data |> 
      group_by(factor) |>
      summarize(
        avg = mean(exposure),
        med = median(exposure)
      ) |>
      mutate(type = "Univ"),
    e_data$bench_data |> 
      filter(sector == sct) |>
      group_by(factor) |>
      summarize(
        avg = mean(exposure),
        med = median(exposure)
      ) |>
      mutate(type = e_data$bench),
    e_data$acct_data |> 
      filter(sector == sct) |>
      group_by(factor) |>
      summarize(
        avg = mean(exposure),
        med = median(exposure)
      ) |>
      mutate(type = e_data$acct)
  ) |> 
    mutate(
      type = factor(
        type,
        c("Univ", e_data$bench, e_data$acct)
      )
    )
  
  summary_tbl |>
    select(-med) |>
    pivot_wider(names_from = type, values_from = avg)
  
  # summary_tbl |>
  #   select(-avg) |>
  #   pivot_wider(names_from = type, values_from = med)
  
}
