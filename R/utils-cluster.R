# check whether the cluster is running.
isClusterRunning = function(cl) {

  tryCatch(any(unlist(clusterEvalQ(cl, TRUE))),
    error = function(err) { FALSE })

}#ISCLUSTERRUNNING

# check the status of the snow/parallel cluster.
check.cluster = function(cluster) {

  if (is.null(cluster))
    return(TRUE)

  if (!(any(class(cluster) %in% supported.clusters)))
    stop("cluster is not a valid cluster object.")

  if (!("package:snow" %in% search()))
    if (!require(parallel) && !require(snow))
      stop("Can't find required packages: snow or parallel.")
  if (!isClusterRunning(cluster))
    stop("the cluster is stopped.")

}#CHECK.CLUSTER

# get the number of slaves.
nSlaves = function(cluster) {

  length(cluster)

}#NSLAVES

slaves.setup = function(cluster) {

  # set the test counter in all the cluster nodes.
  clusterEvalQ(cluster, library(bnlearn))
  clusterEvalQ(cluster, reset.test.counter())

}#SLAVE.SETUP
