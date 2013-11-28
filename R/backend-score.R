
# compare two network scores in an efficient way.
score.delta = function(arc, network, data, score, score.delta,
    reference.score, op, extra, decomposable = TRUE, debug = FALSE) {

  if (decomposable) {

    score.delta.decomposable(arc = arc, network = network, data = data,
      score = score, score.delta = score.delta, reference.score = reference.score,
      op = op, extra = extra, debug = debug)

  }#THEN
  else {

    score.delta.monolithic(arc = arc, network = network, data = data,
      score = score, score.delta = score.delta, reference.score = reference.score,
      op = op, extra = extra, debug = debug)

  }#ELSE

}#SCORE.DELTA

# compare two decomposable network scores in an efficient way.
score.delta.decomposable = function(arc, network, data, score, score.delta,
    reference.score, op, extra, debug = FALSE) {

  # do a minimal update to the network structure.
  fake = .Call("score_delta_helper",
          net = network,
          arc = arc,
          operator = op)

  if (op == "reverse") {

    # compute the updated score contributions of the nodes involved.
    new.score = per.node.score(network = fake, score = score,
                        targets = arc, extra.args = extra, data = data)

    # update the test counter.
    increment.test.counter(2)

    # compare the network scores, minus numeric tolerance for better score
    # equivalence detection.
    old = sum(reference.score[arc])
    new = sum(new.score)

    if (isTRUE(all.equal(new, old)))
      retval = 0
    else
      retval = new - old

  }#THEN
  else {

    # compute the updated score contributions of arc[2].
    new.score = per.node.score(network = fake, score = score,
                  targets = arc[2], extra.args = extra, data = data)

    # update the test counter.
    increment.test.counter(1)

    # compare the network scores.
    old = reference.score[arc[2]]
    new = new.score
    retval = new - old

  }#ELSE

  # catch: the difference between two -Inf scores must be -Inf and not NaN,
  # so that we can safely test it and reject the change; and if the old score
  # is -Inf just look at the new score.
  if (!is.finite(old))
    retval = ifelse(is.finite(new), new, -Inf)

  if (debug)
    cat("    > delta between scores for nodes", arc, "is", retval, ".\n")

  return(list(bool = (retval > score.delta + sqrt(.Machine$double.eps)),
    delta = retval, updates = new.score))

}#SCORE.DELTA.DECOMPOSABLE

# compare two decomposable network scores in an efficient way.
score.delta.monolithic = function(arc, network, data, score, score.delta,
    reference.score, op, extra, debug = FALSE) {

  # the default solution for non-dcomposable scores os to compute the whole
  # thing again, for all the nodes.
  candidate = arc.operations(network, from = arc[1], to = arc[2], op = op,
                check.cycles = FALSE, update = TRUE, debug = FALSE)
  new.score = per.node.score(network = candidate, score = score, 
                targets = names(candidate$nodes), extra.args = extra,
                data = data)

  # update the test counter.
  increment.test.counter(length(new.score))

  old = sum(reference.score)
  new = sum(new.score)
  retval = new - old

  # catch: the difference between two -Inf scores must be -Inf and not NaN,
  # so that we can safely test it and reject the change; and if the old score
  # is -Inf just look at the new score.
  if (!is.finite(old))
    retval = ifelse(is.finite(new), new, -Inf)

  if (debug)
    cat("    > delta between scores for nodes", arc, "is", retval, ".\n")

  return(list(bool = (retval > score.delta + sqrt(.Machine$double.eps)),
    delta = retval, updates = new.score))

}#SCORE.DELTA.MONOLITHIC

# create a data frame or an adjacency matrix containing the arcs to be added.
arcs.to.be.added = function(amat, nodes, blacklist = NULL, whitelist = NULL,
    nparents = NULL, maxp = Inf, arcs = TRUE) {

  .Call("hc_to_be_added",
        arcs = amat,
        blacklist = blacklist,
        whitelist = whitelist,
        nparents = nparents,
        maxp = maxp,
        nodes = nodes,
        convert = arcs)

}#ARCS.TO.BE.ADDED

# create a data frame containing the arcs to be dropped:
# arcs                   arcs already in the graph.
# !is.listed(whitelist)  exclude whitelisted arcs.
arcs.to.be.dropped = function(arcs, whitelist) {

  if (!is.null(whitelist))
    return(arcs[!which.listed(arcs, whitelist), , drop = FALSE])
  else
    return(arcs)

}#ARCS.TO.BE.DROPPED

# create a data frame containing the arcs to be reversed:
arcs.to.be.reversed = function(arcs, blacklist, nparents, maxp = Inf) {

  if (!is.null(blacklist))
    arcs = arcs[!which.listed(arcs[, c(2, 1), drop = FALSE], blacklist), , drop = FALSE]

  if (!missing(nparents))
    arcs = arcs[nparents[arcs[, 1]] < maxp, ]

  return(arcs)

}#ARCS.TO.BE.REVERSED

