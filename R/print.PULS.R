print.PULS <- function (x, abbrev=0,  spaces = 2, digits = options('digits')$digits, 
    ...) 
{

## Modified from print.rpart.

minlength <-0
    if (!inherits(x, "rpart")) 
        stop("Not legitimate rpart object")
    if (!is.null(x$frame$splits)) 
        x <- rpconvert(x)

    frame <- x$frame
    ylevel <- attr(x, "ylevels")
    node <- as.numeric(row.names(frame))
    depth <- tree.depth(node)
    indent <- paste(rep(" ", spaces * 32L), collapse = "")
    if (length(node) > 1L) {
        indent <- substring(indent, 1L, spaces * seq(depth))
        indent <- paste(c("", indent[depth]), format(node), ")", 
            sep = "")
    }
    else indent <- paste(format(node), ")", sep = "")
    tfun <- (x$functions)$print
    if (!is.null(tfun)) {
        if (is.null(frame$yval2)) 
            yval <- tfun(frame$yval, ylevel, digits)
        else yval <- tfun(frame$yval2, ylevel, digits)
    }
    else yval <- format(signif(frame$yval, digits = digits))
	print(yval)
    term <- rep(" ", length(depth))
    term[frame$var == "<leaf>"] <- "*"
    z <- labels(x, digits = digits, abbrev= abbrev, minlength = minlength, ...)
    n <- frame$n
    z <- paste(indent, z, n, format(signif(frame$dev, digits = digits)), round((1-as.numeric(yval)/1),digits=2), term)
    omit <- x$na.action
    if (length(omit)) 
        cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
    else cat("n=", n[1L], "\n\n")
    cat("Node, N, Within Cluster Deviance, Proportion Deviance Explained\n")
    cat("      * denotes terminal node\n\n")
    cat(z, sep = "\n")
    return(invisible(x))
}

