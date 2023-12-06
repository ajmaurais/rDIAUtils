arrangePlots <-
function(pcs, row.cols, color.cols)
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
                p <- PCAScatterPlot(pcs[[row.cols[row.i]]], color.cols[color.i], plot.title=plot.title,
                                    show.xlab=show.xlab, show.legend=show.legend)
            } else {
                p <- p + PCAScatterPlot(pcs[[row.cols[row.i]]], color.cols[color.i], plot.title=plot.title,
                                        show.xlab=show.xlab, show.legend=show.legend)
            }
        }
    }
    p + patchwork::plot_layout(nrow=length(color.cols), byrow=T) 
}
