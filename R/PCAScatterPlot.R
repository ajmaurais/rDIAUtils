PCAScatterPlot <-
function(pc, color.col, plot.title=NULL,
                           show.ylab=T, show.xlab=T, show.legend=F)
{
    dat <- dplyr::left_join(pc[['pc']], dat.meta, by='replicate')

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
    color_scale = ifelse(is.numeric(dat.meta[[color.col]]), scale_color_viridis, scale_color_discrete)
    p <- p + color_scale(name=getName(color.col))

    if(!show.legend) { p <- p + guides(color='none') }
    if(!is.null(plot.title)) { p <- p + ggtitle(plot.title) }
    
    p
}
