summarize_scores <- function(data, score_col, group_col) {
  data |>
    group_by({{ group_col }}) |>
    mutate(
      score_min  = min({{ score_col }}, na.rm = TRUE),
      score_max  = max({{ score_col }}, na.rm = TRUE),
      score_mean = mean({{ score_col }}, na.rm = TRUE)
    ) |>
    ungroup() |>
    mutate(
      {{ group_col }} := fct_reorder({{ group_col }}, {{ score_col }}, .fun = mean, .na_rm = TRUE)
    )
}

plot_scores <- function(data,
                        score_col,
                        group_col,  # e.g. countryname or region
                        color_col = NULL,
                        palette = "Paired") {
  
  data |>
    summarize_scores({{score_col}}, {{ group_col }}) |>
    ggplot() +
    geom_segment(
      aes(
        x = score_min,
        xend = score_max,
        y = {{ group_col }},
        yend = {{ group_col }}
      ),
      color = "grey70"
    ) +
    geom_point(
      aes(
        x = score_mean,
        y = {{ group_col }}
      ),
      shape = 1,
      size = 6,
      color = "grey50"
    ) +
    geom_point(
      aes(
        {{score_col}},
        {{ group_col }},
        color = {{ color_col }}
      ),
      size = 4
    ) +
    scale_color_brewer(
      palette = palette
    ) +
    guides(
      color = guide_legend(nrow = 2)
    ) +
    theme(
      legend.position = "bottom"
    )
}
