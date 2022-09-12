\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (graph, init_node) 
{
    node_count <- nlevels(factor(graph[, 1]))
    list_spath <- array(rep(Inf, node_count))
    visited <- array(rep(0), node_count)
    assert_args <- is.data.frame(graph) && all(names(graph) == 
        c("v1", "v2", "w")) && any(init_node == graph[1]) && 
        is.numeric(init_node) && length(init_node) == 1
    stopifnot(assert_args)
    for (i in c(1:(node_count - 1))) {
        temp_gph <- graph[which(graph[, 1] == init_node), ]
        if (list_spath[init_node] == Inf) 
            list_spath[init_node] <- 0
        for (j in temp_gph[, 2]) {
            if (list_spath[j] == Inf) {
                list_spath[j] <- temp_gph[which(temp_gph[, 2] == 
                  j), 3] + list_spath[init_node]
            }
            else list_spath[j] <- min(temp_gph[which(temp_gph[, 
                2] == j), 3] + list_spath[init_node], list_spath[j])
        }
        visited[i] <- init_node
        init_node <- max(which(list_spath == min(list_spath[-visited])))
    }
    return(list_spath)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
