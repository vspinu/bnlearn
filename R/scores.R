
# compute the score of a bayesian network.
network.score = function(network, data, score, extra.args, debug = FALSE) {

  sum(per.node.score(network = network, data = data, score = score,
    targets = names(network$nodes), extra.args = extra.args,
    debug = debug))

}#NETWORK.SCORE

# compute single nodes' contributions to the network score.
per.node.score = function(network, data, score, targets, extra.args,
    debug = FALSE) {

  if (score == "k2") {

   res = vapply(targets, dirichlet.node, x = network, data = data,
           debug = debug, FUN.VALUE = template.numeric)

  }#THEN
  else if (score == "bde") {

   res = vapply(targets, dirichlet.node, x = network,
           imaginary.sample.size = extra.args$iss,
           prior = extra.args$prior, beta = extra.args$beta,
           experimental = NULL, sparse = FALSE, data = data,
           debug = debug, FUN.VALUE = template.numeric)

  }#THEN
  else if (score == "mbde") {

   res = vapply(targets, dirichlet.node, x = network,
           imaginary.sample.size = extra.args$iss,
           experimental = extra.args$exp, sparse = FALSE,
           data = data, debug = debug, FUN.VALUE = template.numeric)

  }#THEN
  else if (score == "bdes") {

   res = vapply(targets, dirichlet.node, x = network,
           imaginary.sample.size = extra.args$iss,
           experimental = NULL, sparse = TRUE, data = data,
           debug = debug, FUN.VALUE = template.numeric)

  }#THEN
  else if (score == "loglik") {

    res = vapply(targets, loglik.node, x = network, data = data,
            debug = debug, FUN.VALUE = template.numeric)

  }#THEN
  else if (score %in% c("aic", "bic")) {

    res = vapply(targets, aic.node, x = network, data = data,
            k = extra.args$k, debug = debug, FUN.VALUE = template.numeric)

  }#THEN
  else if (score == "bge") {

    res = vapply(targets, bge.node, x = network, phi = extra.args$phi,
            prior = extra.args$prior, beta = extra.args$beta,
            data = data, iss = extra.args$iss, debug = debug,
            FUN.VALUE = template.numeric)

  }#THEN
  else if (score == "loglik-g") {

    res = vapply(targets, gloglik.node, x = network, data = data,
            debug = debug, FUN.VALUE = template.numeric)

  }#THEN
  else if (score %in% c("aic-g", "bic-g")) {

    res = vapply(targets, aic.gauss.node, x = network, data = data,
            k = extra.args$k, debug = debug, FUN.VALUE = template.numeric)

  }#THEN

  # set the names on the resulting array for easy reference.
  names(res) = targets

  return(res)

}#PER.NODE.SCORE

# compute the loglikelihood of single node of a continuous bayesian network.
gloglik.node = function(target, x, data, debug = FALSE) {

  .Call("gloglik_node",
        target = target,
        x = x,
        data = data,
        debug = debug)

}#GLOGLIK.NODE

# compute the AIC of single node of a discrete bayesian network.
aic.gauss.node = function(target, x, data, k = 1, debug = FALSE) {

  lik = gloglik.node(x = x, target = target, data = data, debug = debug)
  pen = k * nparams.gaussian.node(node = target, x = x)

  if (debug) {

    cat("  > penalization is", pen, ".\n")
    cat("  > penalized loglikelihood is", lik - pen, ".\n")

  }#THEN

  return(lik - pen)

}#AIC.GAUSS.NODE

# compute the loglikelihood of single node of a discrete bayesian network.
loglik.node = function(target, x, data, debug = FALSE) {

  .Call("loglik_node",
        target = target,
        x = x,
        data = data,
        debug = debug)

}#LOGLIK.NODE

# compute the AIC of single node of a discrete bayesian network.
aic.node = function(target, x, data, k = 1, debug = FALSE) {

  lik = loglik.node(x = x, target = target, data = data, debug = debug)
  pen = k * nparams.discrete.node(node = target, x = x, data = data, real = TRUE)

  if (debug) {

    cat("  > penalization is", pen, ".\n")
    cat("  > penalized loglikelihood is", lik - pen, ".\n")

  }#THEN

  return(lik - pen)

}#AIC.NODE

# complete a prior over arcs as per Castelo and Siebes.
cs.completed.prior = function(beta, nodes) {

  beta = .Call("castelo_completion",
               prior = beta,
               nodes = nodes)

  class(beta) = c("prior", "prior.cs", "data.frame")
  attr(beta, "nodes") = nodes

  return(beta)

}#CS.COMPLETED.PRIOR

# compute the dirichlet posterior density of a node.
dirichlet.node = function(target, x, data, imaginary.sample.size = NULL,
    prior = "uniform", beta = NULL, experimental = NULL, sparse = FALSE,
    debug = FALSE) {

  .Call("dirichlet_node",
        target = target,
        x = x,
        data = data,
        iss = imaginary.sample.size,
        prior = prior,
        beta = beta,
        experimental = experimental,
        sparse = sparse,
        debug = debug)

}#DIRICHLET.NODE

# compute the posterior density of a gaussian node.
bge.node = function(target, x, data, iss, phi = "heckerman", prior = NULL,
    beta = NULL, debug = FALSE) {

  .Call("wishart_node",
        target = target,
        x = x,
        data = data,
        iss = iss,
        phi = phi,
        prior = prior,
        beta = beta,
        debug = debug)

}#BGE.NODE

