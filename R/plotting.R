
getName <- function(vec) {
  if (is.null(names(vec))){
    return(vec)
  }
  ret <- names(vec)
  ret[ret == ''] <- vec[names(vec) == '']
  return(ret)
}

#' Make PCA scatter plot.
#'
#' @param pc A PCA matrix.
#' @param dat.metadata A dataframe mapping replicate names to metadata terms to use in plots.
#' @param color.col Column to use for color aesthetic.
#' @param plot.title Plot title. By default there is no title.
#' @param show.ylab Show y axis label?
#' @param show.xlab Show x axis label?
#' @param show.legend Show legend for color aesthetic?
#' @param interactive Use ggiraph::geom_point_interactive?
#' @param legend.maxLabelPerCol An integer indicating the maximum labels per column in the legend.
#' @param ... Additional arguments passed to plot theme()
#'
#' @return A ggplot object.
#'
#' @import ggplot2
#' @export
PCAScatterPlot <- function(pc, dat.metadata, color.col, plot.title=NULL,
                           show.ylab=T, show.xlab=T, show.legend=F,
                           interactive=F, legend.maxLabelPerCol=12, ...)
{
  dat <- dplyr::left_join(pc[['pc']], dat.metadata, by='replicate')

  p <- ggplot(dat, aes(x=PC1, y=PC2, color=get(color.col),
                       tooltip=replicate, data_id=replicate))

  if (interactive) {
    p <- p + ggiraph::geom_point_interactive()
  } else {
    p <- p + geom_point()
  }

  p <- p + theme_bw() +
    theme(panel.grid=element_blank(),
          plot.title=element_text(hjust=0.5), ...)

  if (show.ylab) {
      p <- p + ylab(pc[['y.lab']])
  } else {
    p <- p + theme(..., axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
      ylab(NULL)
  }
  if (show.xlab) {
    p <- p + xlab(pc[['x.lab']])
  } else {
    p <- p + theme(..., axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      xlab(NULL)
  }

  # add viridis color scale if color.col is numeric
  color_scale = ifelse(is.numeric(dat.metadata[[color.col]]),
                       viridis::scale_color_viridis,
                       scale_color_discrete)
  p <- p + color_scale(name=getName(color.col))

  if (!show.legend) {
    p <- p + guides(color='none')
  } else {
    if (!is.numeric(dat.metadata[[color.col]])) {
      legend.ncol = ceiling(length(unique(dat.metadata[[color.col]])) / legend.maxLabelPerCol)
      p <- p + guides(color=guide_legend(ncol=legend.ncol))
    }
  }
  if ( !is.null(plot.title) ) { p <- p + ggtitle(plot.title) }

  p
}

#' Arange multiple PCA matrices into a plot grid
#'
#' @param pcs A list of PCA matrices.
#' @param row.cols Variables to use for rows.
#' @param color.cols Variables to use for columns.
#' @param dat.metadata A dataframe mapping replicate names to metadata terms to use in plots.
#' @param interactive Use ggiraph::geom_point_interactive?
#' @param legend.maxLabelPerCol An integer indicating the maximum labels per column in the legend.
#' @param nrow Number of rows in plot layout.
#'        If there are more than 1 color.cols nrow must be equal to length(color.cols)
#' @param return.list Return a list of ggplot objects instead of a patchwork object.
#' @param ... Additional arguments passed to plot theme()
#'
#' @return A patchwork plot object
#'
#' @import patchwork
#' @export
arrangePlots <- function(pcs, row.cols, color.cols, dat.metadata,
                         interactive=F, legend.maxLabelPerCol=12,
                         nrow=length(color.cols), return.list=FALSE, ...)
{
  if (nrow != length(color.cols) & length(color.cols) != 1) {
    stop('nrow must equal length(color.cols) when more than 1 color.col')
  }
  n_plot_rows <- nrow

  p_list <- vector('list', length=length(color.cols) * length(row.cols))
  plot_i <- 1
  for (color.i in 1:length(color.cols)) {
    for (row.i in 1:length(row.cols)) {
      show_legend <- row.cols[row.i] == row.cols[length(row.cols)]
      show_xlab <- color.cols[color.i] == color.cols[length(color.cols)]
      plot_title <- if (color.cols[color.i] == color.cols[1]) getName(row.cols[row.i]) else { NULL }

      p_list[[plot_i]] <- rDIAUtils::PCAScatterPlot(
        pcs[[row.cols[row.i]]],
        dat.metadata,
        color.cols[color.i],
        plot.title = plot_title,
        show.xlab = show_xlab,
        show.legend = show_legend,
        interactive = interactive,
        legend.maxLabelPerCol = legend.maxLabelPerCol,
      )
      plot_i <- plot_i + 1
    }
  }

  if (return.list) {
    return(p_list)
  }

  p <- patchwork::wrap_plots(p_list)
  if (n_plot_rows > length(color.cols)) {
    return(p + patchwork::plot_layout(nrow=n_plot_rows, byrow=T, guides="collect"))
  }
  p + patchwork::plot_layout(nrow=length(color.cols), byrow=T)
}

