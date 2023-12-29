
getName <- function(vec) {
    if(is.null(names(vec))){
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
#' 
#' @return A ggplot object.
#' 
#' @import ggplot2
#' @export
PCAScatterPlot <- function(pc, dat.metadata, color.col, plot.title=NULL,
                           show.ylab=T, show.xlab=T, show.legend=F)
{
    dat <- dplyr::left_join(pc[['pc']], dat.metadata, by='replicate')

    p <- ggplot(dat, aes(x=PC1, y=PC2, color=get(color.col))) +
        geom_point() +
        theme_bw() +
        theme(panel.grid=element_blank(),
              plot.title=element_text(hjust=0.5))

    if(show.ylab) {
        p <- p + ylab(pc[['y.lab']])
    } else {
        p <- p + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
            ylab(NULL)
    }
    if(show.xlab) {
        p <- p + xlab(pc[['x.lab']])
    } else {
        p <- p + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
            xlab(NULL)
    }

    # add viridis color scale if color.col is numeric
    color_scale = ifelse(is.numeric(dat.metadata[[color.col]]),
                         viridis::scale_color_viridis,
                         scale_color_discrete)
    p <- p + color_scale(name=getName(color.col))

    if(!show.legend) { p <- p + guides(color='none') }
    if(!is.null(plot.title)) { p <- p + ggtitle(plot.title) }
    
    p
}

#' Arange multiple PCA matrices into a plot grid
#' 
#' @param pcs A list of PCA matrices.
#' @param row.cols Variables to use for rows.
#' @param color.cols Variables to use for columns.
#' @param dat.metadata A dataframe mapping replicate names to metadata terms to use in plots.
#' 
#' @return A patchwork plot object
#' 
#' @import patchwork
#' @export
arrangePlots <- function(pcs, row.cols, color.cols, dat.metadata)
{
    p <- NULL # initlize empty plot group
    for(color.i in 1:length(color.cols))
    {
        for(row.i in 1:length(row.cols))
        {
            show.legend <- row.cols[row.i] == row.cols[length(row.cols)]
            show.xlab <- color.cols[color.i] == color.cols[length(color.cols)]
            plot.title <- if(color.cols[color.i] == color.cols[1]) getName(row.cols[row.i]) else { NULL }

            if(is.null(p)) {
                p <- PCAScatterPlot(pcs[[row.cols[row.i]]], dat.metadata,
                                    color.cols[color.i], plot.title=plot.title,
                                    show.xlab=show.xlab, show.legend=show.legend)
            } else {
                p <- p + PCAScatterPlot(pcs[[row.cols[row.i]]], dat.metadata,
                                        color.cols[color.i], plot.title=plot.title,
                                        show.xlab=show.xlab, show.legend=show.legend)
            }
        }
    }
    p + patchwork::plot_layout(nrow=length(color.cols), byrow=T) 
}

