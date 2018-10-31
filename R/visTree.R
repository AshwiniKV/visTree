#' Visualization of subgroups for decision trees
#'
#' This visualization characterizes subgroups defined by a decision tree structure and identifies the range of covariate values associated with outcome values in each subgroup.
#'
#' @param cond.tree Decision tree generated as a party object.
#' @param rng Restrict plotting to a particular set of nodes. Default value is set as NULL.
#' @param interval logical. Continuous outcome (interval = FALSE) and Categorical outcome (interval = TRUE).
#' @param color.type Color palettes (rainbow_hcl = 1; heat_hcl = 2; terrain_hcl = 3; sequential_hcl = 4 ; diverge_hcl = 5)
#' @param alpha Transparency for horizontal colored bars in each subplot. Values between 0 to 1.
#' @param add.h.axis logical. Add axis for the outcome distribution (add.h.axis = TRUE), remove axis for the outcome (add.h.axis = FALSE).
#' @param add.p.axis logical. Add axis for the percentiles (add.p.axis = TRUE) computed over covariate values, remove axis for the percentiles (add.p.axis = FALSE).
#' @param text.main Change the size of the main titles
#' @param text.axis Change the size of the text of axis labels
#' @param text.title Change the size of the text in the title
#' @param text.bar Change the size of the text in the horizontal bar 
#' @param text.round Round the threshold displayed on the horizontal bar
#' @param text.label Change the size of the axis annotation
#' @param text.percentile Change the size of the percentile title
#' @param density.line logical. Draw a density line. (density.line = TRUE).
#' @keywords visualization pathway decision tree
#' @author Ashwini Venkatasubramaniam and Julian Wolfson
#' @export
#' @importFrom utils capture.output tail
#' @examples
#' data(blsdata)
#' newblsdata<-blsdata[,c(7,21, 22,23, 24, 25, 26)]
#' ptree1<-partykit::ctree(kcal24h0~., data = newblsdata)
#' visTree(ptree1, text.axis = 1.3, text.label = 1.2, text.bar = 1.2, alpha = 0.5)
#' 
#' ptree2<-partykit::ctree(kcal24h0~skcal+rrvfood+resteating+age, data = blsdata)
#' visTree(ptree2, text.axis = 1.3, text.label = 1.2, text.bar = 1.2, alpha = 0.5)
#' 
#' blsdataedit<-blsdata[,-7]
#' blsdataedit$bin<-0 
#' blsdataedit$bin<-cut(blsdata$kcal24h0, unique(quantile(blsdata$kcal24h0)), 
#' include.lowest = TRUE, dig.lab = 4)
#' names(blsdataedit)[26]<-"kcal24h0"
#' ptree3<-partykit::ctree(kcal24h0~hunger+rrvfood+resteating+liking, data = blsdataedit)
#' visTree(ptree3, interval = TRUE,  color.type = 1, alpha = 0.6, 
#' text.percentile = 1.2, text.bar = 1.8)
#' 
#' ptree4<-rpart::rpart(kcal24h0~wanting+liking+rrvfood, data = newblsdata, 
#' control = rpart::rpart.control(cp = 0.029))
#' visTree(ptree4, text.bar = 1.8, text.label = 1.4, text.round = 1, 
#' density.line = TRUE, text.percentile = 1.3)

visTree <- function(cond.tree, rng = NULL, interval = FALSE, color.type = 1, alpha = 0.5, add.h.axis = TRUE, add.p.axis = TRUE, text.round = 1, text.main = 1.5, text.bar = 1.5, text.title = 1.5, text.label = 1.5, text.axis = 1.5, text.percentile = 0.7, density.line = TRUE) {
  ## Wrapper function to produce plots from a conditional inference tree
  ## 'range' parameter can restrict plotting to a particular set of nodes

  if (inherits(cond.tree, "constparty")) {
    splittree <- path_node(cond.tree)
  } else {
    splittree <- path_node(as.party(cond.tree))
  }
  structure <- strsplit(splittree, split = ";")
  if (inherits(cond.tree, "constparty")) {
    terminal.id <- nodeids(cond.tree, terminal = TRUE)
  } else {
    terminal.id <- nodeids(as.party(cond.tree), terminal = TRUE)
  }
  if (length(structure[[1]]) == length(terminal.id)) {
    structure[[1]] <- sapply(1:length(structure[[1]]), function(i) {
      paste0(structure[[1]][i], ",", terminal.id[i], " ")
    })
  }

  if (inherits(cond.tree, "constparty")) {
    input.info <- data_party(cond.tree)
    #  X <- input.info[,2:(length(input.info)-3)]
    X <- input.info[, 2:(length(input.info) - 3)]
    Y <- fitted(cond.tree)[[3]]
  } else {
    input.info <- data_party(as.party(cond.tree))
    #  X <- input.info[,2:(length(input.info)-3)]
    X <- input.info[, 2:(length(input.info) - 2)]
    Y <- fitted(as.party(cond.tree))[[2]]
  }

  if (is.factor(Y)) {
    n.terminals <- length(structure[[1]])
    # prob.mat <- matrix(data=unlist(lapply(structure[[1]],function(S) {
    # unlist(lapply(strsplit(S,","),function(split.S) {
    # seg <- unlist(split.S[length(split.S)])
    # as.numeric(trim(strsplit(seg,"=")[[1]][2]))
    # }))
    # })), nrow=n.terminals)
    y.list <- sapply(1:length(structure[[1]]), function(j) {
      seg <- strsplit(structure[[1]], ",")
      if (interval == TRUE) {
        paste0(seg[[j]][c((length(seg[[j]]) - 2):(length(seg[[j]]) - 1))], collapse = ",")
      } else {
        paste0(seg[[j]][length(seg[[j]]) - 2], collapse = ",")
      }
    })

    x.list <- sapply(1:length(structure[[1]]), function(j) {
      seg <- strsplit(structure[[1]], ",")
      x.l <- sapply(1:length(seg), function(i) {
        if (interval == TRUE) {
          x.length <- length(seg[[i]]) - 3
        } else {
          x.length <- length(seg[[i]]) - 2
        }
      })
      if (interval == TRUE) {
        paste0(seg[[j]][1:(length(seg[[j]]) - 3)], collapse = ",")
      } else {
        paste0(seg[[j]][1:(length(seg[[j]]) - 2)], collapse = ",")
      }
    })

    term.node <- sapply(1:length(structure[[1]]), function(j) {
      seg <- strsplit(structure[[1]], ",")
      if (interval == TRUE) {
        paste0(tail(seg[[j]], 1), collapse = ",")
      } else {
        paste0(tail(seg[[j]], 1), collapse = ",")
      }
    })

    structure <- lapply(1:length(y.list), function(i) {
      paste0(x.list[[i]], ", ", y.list[[i]], ", ", term.node[[i]])
    })
  }
  if (length(unlist(structure)) == 1) {
    stop("Tree has only a single node; nothing to visualize.")
  }
  # terminal.id<-nodeids(cond.tree, terminal = TRUE)
  n.terminals <- ifelse(is.null(rng), length(unlist(structure)), length(rng))
  if (is.null(rng)) {
    index <- 1:n.terminals
  } else {
    index <- min(rng):min(max(rng), length(unlist(structure)))
  } ## Should probably do some range checking
  if (length(index) > 10) stop("Number of subgroups exceeds ten")

  par(mfrow = c(4, ceiling(length(index) / 2)), mar = c(2, 1, 3, 1))
  number <- length(index) * 2 + (length(index) * 2) %% 4
  layout(matrix(1:number, 4, ceiling(length(index) / 2)))

  invisible(
    sapply(unlist(structure)[index], function(S) {
      plot_minmax(minmax_mat(S, colnames(X), Y, interval), X, Y, S, color.type, alpha, add.p.axis, add.h.axis, cond.tree, text.main, text.bar, text.round, text.percentile, density.line, text.title, text.axis, text.label)
    })
  )
}
