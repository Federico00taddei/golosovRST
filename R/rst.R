#' Validate input for Golosov RST functions
#' @keywords internal
.validate_rst_input <- function(data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  required <- c("s1", "s2", "s3")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  for (nm in required) {
    if (!is.numeric(data[[nm]])) {
      stop("Column `", nm, "` must be numeric.", call. = FALSE)
    }
    if (anyNA(data[[nm]])) {
      stop("Column `", nm, "` contains NA values.", call. = FALSE)
    }
    if (any(data[[nm]] < 0 | data[[nm]] > 1)) {
      stop("Column `", nm, "` must be in [0, 1].", call. = FALSE)
    }
  }

  tot <- data$s1 + data$s2 + data$s3
  if (any(tot > 1 + sqrt(.Machine$double.eps))) {
    stop("Each row must satisfy s1 + s2 + s3 <= 1.", call. = FALSE)
  }

  sr <- 1 - tot
  den <- data$s1 + sr
  if (any(abs(den) < sqrt(.Machine$double.eps))) {
    stop("Denominator (s1 + sr) is zero for at least one row.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Compute Golosov relative size coordinates
#'
#' @param data A data.frame containing numeric columns `s1`, `s2`, `s3`, representing the seat shares of the three largest parties in parliament after the election.
#' @return A data.frame with original columns plus `sr`, `x`, and `y`.
#' @references
#' Golosov, G. V. (2011). Party system classification: A methodological inquiry. \emph{Party Politics}, 17(5), 539--560.
#'
#' Golosov, G. V. (2014). Towards a classification of the world's democratic party systems, step 2: Placing the units into categories. \emph{Party Politics}, 20(1), 3--15.
#' @export
rst_coords <- function(data) {
  .validate_rst_input(data)

  sr <- 1 - data$s1 - data$s2 - data$s3
  den <- data$s1 + sr

  data.frame(
    data,
    sr = sr,
    x = (data$s2 + sr) / den,
    y = (data$s3 + sr) / den,
    check.names = FALSE
  )
}

#' Classify observations in Golosov subtypes using RST polygons
#'
#' @param data A data.frame containing numeric columns `s1`, `s2`, `s3`, representing the seat shares of the three largest parties in parliament after the election.
#' @param offset Small numeric offset used to avoid polygon-edge ambiguity.
#' @return A data.frame with coordinates and `golosov_subtype`.
#' @references
#' Golosov, G. V. (2011). Party system classification: A methodological inquiry. \emph{Party Politics}, 17(5), 539--560.
#'
#' Golosov, G. V. (2014). Towards a classification of the world's democratic party systems, step 2: Placing the units into categories. \emph{Party Politics}, 20(1), 3--15.
#' @export
rst_classify <- function(data, offset = 5e-16) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for `rst_plot()`. Please install it.",
         call. = FALSE)
  }

  coords <- rst_coords(data)
  points_sf <- sf::st_as_sf(coords, coords = c("x", "y"), crs = NA)

  subzones <- sf::st_sf(
    golosov_subtype = c(
      "Predominant Party Polyvalent",
      "Predominant Party Bivalent",
      "Two-Party Monovalent",
      "Two-Party Polyvalent",
      "Multiparty Bivalent",
      "Multiparty Monovalent"
    ),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        0.666 - offset, 0.333 - offset,
        0.5, 0.5 - offset,
        0.1, 0.1,
        0.2, 0.1 + offset,
        0.5 - offset, 0.25 + offset,
        0.625, 0.25 + offset,
        0.666 - offset, 0.333 - offset
      ), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(
        0, 0,
        0.1, 0.1 - offset,
        0.2, 0.1 - offset,
        0.5, 0.25 - offset,
        0.625 - offset, 0.25 - offset,
        0.5 - offset, 0 - offset,
        0, 0
      ), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(
        0.5 + offset, 0 - offset,
        0.625 + offset, 0.25 - offset,
        0.75 - offset, 0.25 - offset,
        0.9 - offset, 0.1 - offset,
        1 + offset, 0.1 - offset,
        1 + offset, 0 - offset,
        0.5 + offset, 0 - offset
      ), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(
        1 + offset, 0.5 - offset,
        0.666 + offset, 0.333 - offset,
        0.625 + offset, 0.25 + offset,
        0.75 + offset, 0.25 + offset,
        0.9 + offset, 0.1 + offset,
        1 + offset, 0.1 + offset,
        1 + offset, 0.5 - offset
      ), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(
        1 + offset, 1,
        1 + offset, 0.5 + offset,
        0.75 + offset, 0.375,
        0.75 + offset, 0.5,
        0.9 + offset, 0.8,
        0.9 + offset, 0.9,
        1 + offset, 1
      ), ncol = 2, byrow = TRUE))),
      sf::st_polygon(list(matrix(c(
        0.666, 0.333 + offset,
        0.75 - offset, 0.375,
        0.75 - offset, 0.5,
        0.9 - offset, 0.8,
        0.9 - offset, 0.9 - offset,
        0.5 + offset, 0.5,
        0.666, 0.333 + offset
      ), ncol = 2, byrow = TRUE)))
    ),
    crs = NA
  )

  joined <- sf::st_join(points_sf, subzones, join = sf::st_within, left = TRUE)
  xy <- sf::st_coordinates(joined)

  data.frame(
    sf::st_drop_geometry(joined),
    X = xy[, 1],
    Y = xy[, 2],
    check.names = FALSE
  )
}

#' Plot Golosov Relative Size Triangle
#'
#' @param data A data.frame containing numeric columns `s1`, `s2`, `s3`, representing the seat shares of the three largest parties in parliament after the election.
#' @param label_col Optional character scalar: name of a column used as point labels.
#' @return A `ggplot2` object.
#' @importFrom rlang .data
#' @references
#' Golosov, G. V. (2011). Party system classification: A methodological inquiry. \emph{Party Politics}, 17(5), 539--560.
#'
#' Golosov, G. V. (2014). Towards a classification of the world's democratic party systems, step 2: Placing the units into categories. \emph{Party Politics}, 20(1), 3--15.
#' @examples
#' mydata <- data.frame(
#'   country = c("uk 2015", "ita 2013", "spa 2015"),
#'   s1 = c(0.508, 0.4714, 0.3514),
#'   s2 = c(0.357, 0.1730, 0.2571),
#'   s3 = c(0.086, 0.1556, 0.1971)
#' )
#' plot_rst(mydata)
#' plot_rst(mydata, label_col = "country")
#' @export
plot_rst <- function(data, label_col = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for `rst_plot()`. Please install it.",
         call. = FALSE)
  }

  final_data <- rst_classify(data)

  if (!is.null(label_col) && !label_col %in% names(final_data)) {
    stop("`label_col` not found in `data`.", call. = FALSE)
  }

  triangle <- data.frame(
    x = c(0, 1, 1),
    y = c(0, 1, 0),
    group = "frame"
  )

  dividing_lines1 <- data.frame(
    x = c(0.5, 0.5, 1),
    y = c(0, 0.5, 0.5),
    xend = c(0.666, 0.666, 0.666),
    yend = c(0.333, 0.333, 0.333)
  )

  dividing_lines2 <- data.frame(
    x = c(0.1, 0.2, 0.5, 0.75, 0.9, 0.9, 0.9, 0.75),
    y = c(0.1, 0.1, 0.25, 0.25, 0.1, 0.9, 0.8, 0.5),
    xend = c(0.2, 0.5, 0.75, 0.9, 1, 0.9, 0.75, 0.75),
    yend = c(0.1, 0.25, 0.25, 0.1, 0.1, 0.8, 0.5, 0.375)
  )

  labels <- data.frame(
    x = c(0.4, 0.5, 0.4, 0.75, 0.8, 0.9, 0.9, 0.85, 0.7),
    y = c(0.2, 0.3, 0.1, 0.1, 0.2, 0.3, 0.6, 0.7, 0.6),
    label = c(
      "Predominant Party", "Polyvalent", "Bivalent", "Monovalent",
      "Two-Party", "Polyvalent", "Bivalent", "Multiparty", "Monovalent"
    )
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = triangle, ggplot2::aes(x = x, y = y, group = group),
      fill = NA, color = "black", linewidth = 1
    ) +
    ggplot2::geom_segment(
      data = dividing_lines1,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      color = "black", linewidth = 1
    ) +
    ggplot2::geom_segment(
      data = dividing_lines2,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      color = "black", linewidth = 0.5
    ) +
    ggplot2::geom_text(
      data = labels, ggplot2::aes(x = x, y = y, label = label),
      size = 2.5, fontface = "bold"
    ) +
    ggplot2::geom_point(
      data = final_data, ggplot2::aes(x = X, y = Y),
      color = "red", size = 2
    ) +
    ggplot2::coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
    ggplot2::labs(
      x = "(s2 + sr)/(s1 + sr)",
      y = "(s3 + sr)/(s1 + sr)",
      title = "Relative Size Triangle"
    ) +
    ggplot2::theme_minimal()

  if (!is.null(label_col)) {
    p <- p + ggplot2::geom_text(
      data = final_data,
      ggplot2::aes(x = X, y = Y, label = .data[[label_col]]),
      hjust = -0.1, vjust = 0, size = 3
    )
  }

  p
}
