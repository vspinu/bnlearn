
# load suggested packages and initialize global variables.
.onLoad = function(lib, pkg) {

  setHook(packageEvent("graph", "attach"), action = "append",
    function(...) {

      # re-register S4 methods.
      setMethod("nodes", "bn", where = topenv(parent.frame()),
        function(object) .nodes(object))
      setMethod("nodes", "bn.fit", where = topenv(parent.frame()),
        function(object) .nodes(object))
      setMethod("nodes", "bn.naive", where = topenv(parent.frame()),
        function(object) .nodes(object))
      setMethod("nodes", "bn.tan", where = topenv(parent.frame()),
        function(object) .nodes(object))

      setMethod("degree", "bn", where = topenv(parent.frame()),
        function(object, Nodes) .degree(object, Nodes))
      setMethod("degree", "bn.fit", where = topenv(parent.frame()),
        function(object, Nodes) .degree(object, Nodes))
      setMethod("degree", "bn.naive", where = topenv(parent.frame()),
        function(object, Nodes) .degree(object, Nodes))
      setMethod("degree", "bn.tan", where = topenv(parent.frame()),
        function(object, Nodes) .degree(object, Nodes))

  })

  # make bnlearn's classes known to S4.
  setClass("bn")
  setClass("bn.fit")
  setClass("bn.naive")
  setClass("bn.tan")

  # if no generic is present for nodes(), create it.
  #if (!("graph" %in% loadedNamespaces()))
    setGeneric("nodes", function(object, ...) standardGeneric("nodes"))
  # add the methods.
  setMethod("nodes", "bn", function(object) .nodes(object))
  setMethod("nodes", "bn.fit", function(object) .nodes(object))
  setMethod("nodes", "bn.naive", function(object) .nodes(object))
  setMethod("nodes", "bn.tan", function(object) .nodes(object))

  # if no generic is present for degree(), create it.
  #if (!("graph" %in% loadedNamespaces()))
    setGeneric("degree", function(object, Nodes, ...) standardGeneric("degree"))
  # add the methods.
  setMethod("degree", "bn", function(object, Nodes) .degree(object, Nodes))
  setMethod("degree", "bn.fit", function(object, Nodes) .degree(object, Nodes))
  setMethod("degree", "bn.naive", function(object, Nodes) .degree(object, Nodes))
  setMethod("degree", "bn.tan", function(object, Nodes) .degree(object, Nodes))

  # load the shared library.
  library.dynam("bnlearn", package = pkg, lib.loc = lib)

}#.ONLOAD

