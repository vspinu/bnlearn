
#include "common.h"

double dlik(SEXP x) {

int i = 0, k = 0;
int *n = NULL, *xx = INTEGER(x), llx = NLEVELS(x), num = LENGTH(x);
double res = 0;

  /* initialize the contingency table. */
  n = alloc1dcont(llx);

  /* compute the joint frequency of x and y. */
  for (k = 0; k < num; k++) {

    n[xx[k] - 1]++;

  }/*FOR*/

  /* compute the entropy from the joint and marginal frequencies. */
  for (i = 0; i < llx; i++) {

    if (n[i] != 0)
      res += (double)n[i] * log((double)n[i] / num);

  }/*FOR*/

  return res;

}/*DLIK*/

double cdlik(SEXP x, SEXP y) {

int i = 0, j = 0, k = 0;
int **n = NULL, *nj = NULL;
int llx = NLEVELS(x), lly = NLEVELS(y), num = LENGTH(x);
int *xx = INTEGER(x), *yy = INTEGER(y);
double res = 0;

  /* initialize the contingency table and the marginal frequencies. */
  n = alloc2dcont(llx, lly);
  nj = alloc1dcont(lly);

  /* compute the joint frequency of x and y. */
  for (k = 0; k < num; k++) {

    n[xx[k] - 1][yy[k] - 1]++;

  }/*FOR*/

  /* compute the marginals. */
  for (i = 0; i < llx; i++)
    for (j = 0; j < lly; j++) {

      nj[j] += n[i][j];

    }/*FOR*/

  /* compute the conditional entropy from the joint and marginal
       frequencies. */
  for (i = 0; i < llx; i++)
    for (j = 0; j < lly; j++) {

      if (n[i][j] != 0)
        res += (double)n[i][j] * log((double)n[i][j] / (double)nj[j]);

    }/*FOR*/

  return res;

}/*CDLIK*/

SEXP loglik_node(SEXP target, SEXP x, SEXP data, SEXP debug) {

int *debuglevel = LOGICAL(debug);
double *l = NULL;
char *t = (char *)CHAR(STRING_ELT(target, 0));
SEXP nodes, node_t, parents, data_t, parent_vars, config, loglik;

  if (*debuglevel > 0) {

    Rprintf("----------------------------------------------------------------\n");
    Rprintf("* processing node %s.\n", t);

  }/*THEN*/

  /* get the node cached information. */
  nodes = getListElement(x, "nodes");
  node_t = getListElement(nodes, t);
  /* get the parents of the node. */
  parents = getListElement(node_t, "parents");
  /* extract the node's column from the data frame. */
  data_t = c_dataframe_column(data, target, TRUE, FALSE);
  /* allocate the return value. */
  PROTECT(loglik = allocVector(REALSXP, 1));
  l = REAL(loglik);

  if (LENGTH(parents) == 0) {

    *l = dlik(data_t);

  }/*THEN*/
  else { 

    /* generate the configurations of the parents. */
    PROTECT(parent_vars = c_dataframe_column(data, parents, FALSE, FALSE));
    PROTECT(config = c_cfg2(parent_vars, TRUE, TRUE));
    /* compute the log-likelihood. */
    *l = cdlik(data_t, config);

    UNPROTECT(2);

  }/*ELSE*/

  if (*debuglevel > 0)
    Rprintf("  > loglikelihood is %lf.\n", *l);

  UNPROTECT(1);

  return loglik;

}/*LOGLIK_NODE*/
