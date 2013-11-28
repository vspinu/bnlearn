
#include "common.h"
#include <Rmath.h>
#include <R_ext/Applic.h>

double glik(SEXP x) {

int i = 0, num = LENGTH(x);
double *xx = REAL(x), xm = 0, res = 0;
long double sd = 0;

  /* compute the mean, */
  for (i = 0; i < num; i++)
    xm += xx[i];
  xm /= num;

  /* compute the standard deviation. */
  for (i = 0; i < num; i++)
    sd += (xx[i] - xm) * (xx[i] - xm);
  sd = sqrt(sd / (num - 1));

  /* compute the log-likelihood. */
  for (i = 0; i < num; i++)
    res += dnorm(xx[i], xm, (double)sd, TRUE);

  return res;

}/*GLIK*/

double cglik(SEXP x, SEXP data, SEXP parents) {

double *xx = REAL(x), *qr = NULL, *qraux = NULL, *work = NULL;
double res = 0, tol = MACHINE_TOL, *fitted = NULL;
long double sd = 0;
int *pivot = NULL, rank = 0, one = 1;
int i = 0, ncol = LENGTH(parents) + 1, nrow = LENGTH(x);
SEXP qr_x, x_copy;

  PROTECT(qr_x = qr_matrix(data, parents));
  qr = REAL(qr_x);

  PROTECT(x_copy = duplicate(x));

  /* allocate the working space. */
  qraux = alloc1dreal(ncol);
  work = alloc1dreal(2 * ncol);
  pivot = alloc1dcont(ncol);
  for (i = 0; i < ncol; i++)
    pivot[i] = i + 1;

  /* perform the QR decomposition. */
  F77_CALL(dqrdc2)(qr, &nrow, &nrow, &ncol, &tol, &rank, qraux, pivot, work);

  fitted = alloc1dreal(nrow);

  /* compute the fitted values. */
  F77_CALL(dqrxb)(qr, &nrow, &rank, qraux, REAL(x_copy), &one, fitted);

  /* compute the standard deviation of the residuals. */
  for (i = 0; i < nrow; i++)
    sd += (xx[i] - fitted[i]) * (xx[i] - fitted[i]);
  sd = sqrt(sd / (nrow - 1));

  /* compute the log-likelihood. */
  for (i = 0; i < nrow; i++)
    res += dnorm(xx[i], fitted[i], (double)sd, TRUE);

  UNPROTECT(2);

  return res;

}/*CGLIK*/

SEXP gloglik_node(SEXP target, SEXP x, SEXP data, SEXP debug) {

double *l = NULL;
char *t = (char *)CHAR(STRING_ELT(target, 0));
SEXP nodes, node_t, parents, data_t, loglik;

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

    *l = glik(data_t);

  }/*THEN*/
  else {

    /* compute the log-likelihood. */
    *l = cglik(data_t, data, parents);

  }/*ELSE*/

  UNPROTECT(1);

  return loglik;

}/*GLOGLIK_NODE*/
