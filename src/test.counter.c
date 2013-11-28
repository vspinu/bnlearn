#include "common.h"

/* initialize the global test counter. */
double test_counter = 0;

/* increment the global test counter. */
SEXP increment_test_counter(SEXP n) {

  test_counter += NUM(n);

  return R_NilValue;

}/*INCREMENT_TEST_COUNTER*/

/* reset the global test counter. */
SEXP reset_test_counter() {

  test_counter = 0;

  return R_NilValue;

}/*RESET_TEST_COUNTER*/

/* return the global test counter, for R to see. */
SEXP get_test_counter() {

SEXP result;

  PROTECT(result = allocVector(REALSXP, 1));
  NUM(result) = test_counter;
  UNPROTECT(1);

  return result;

}/*GET_TEST_COUNTER*/

