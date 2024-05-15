#' Distance From Threshold
#' 
#' Calculate the distance between the optimal threshold and the estimates. 
#' 
#' @details
#' The distance from the threshold is a method to provide a weight for estimates
#' based on the distance that each estimate is from the optimal threshold calculated.
#' The distance is calculated by subtracting the optimal threshold from each estimate.
#' The possible range for the distance ranges from -1 to 1, so the distance is then 
#' compared to the adjusted distance, which is the distance added to the threshold 
#' cutpoint. The threshold cutpoint is set to .5; which is often the default. This 
#' adjusted threshold then acts as the new probability estimates to generate the new 
#' truth values. 
#' 
#' @param df_tbl The estimated tibble with the estimates and the generated truth columns.
#' @param thresh The threshold decided by the optimal function.
#' @param thresh_cutpoint The threshold cutpoint decided to classify 0 or 1 values.
#' 
#' @return A numerical vector representing the new estimates.
#' 
#' @examples
#' dist_thresh()

dist_thresh <- function(df_tbl, thresh, thresh_cutpoint = .5){
  df_tbl <- {{df_tbl}} |>
    dplyr::mutate(threshold = thresh,
                  dist = estimate - threshold,
                  dist = round(dist, 2))

possible_dist <- tibble::tibble(
    dist = seq(-1, 1, .01),
    adj_dist = purrr::map_dbl(
    dist,
    ~.thresh_cutpoint + .x
    )
  ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::everything(),
        ~round(.x, 2)
      )
    ) |>
    dplyr::filter(
      adj_dist >= 0
      )

est_truth <- dplyr::left_join(df_tbl, possible_dist, by = "dist") |>
            dplyr::mutate(adj_dist = dplyr::case_when(
              adj_dist > 1 ~ 1,
              TRUE ~ adj_dist
            )) |>
            dplyr::pull(adj_dist)

return(est_truth)
}

