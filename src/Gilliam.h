
#ifndef GILLIAM_H_
#define GILLIAM_H_

#include <Rcpp.h>
RcppExport SEXP NPopEst(SEXP _b00, SEXP _b0l, SEXP _g00, SEXP _g0l, SEXP _csi, SEXP _Nobserved, SEXP _Times) ;
RcppExport SEXP csi_sample1(SEXP list_in1, SEXP _nsim1);
RcppExport SEXP Surv2(SEXP Sij, SEXP _b0, SEXP _b1, SEXP time1, SEXP Dij, SEXP Lij);
RcppExport SEXP LengthLikeR(SEXP Diji, SEXP Liji, SEXP _sigma1);
RcppExport SEXP mortalityR( SEXP ab1, SEXP ad1, SEXP S1);
RcppExport SEXP Surv3prep(SEXP _Inlist);
RcppExport SEXP multmat(SEXP n);
RcppExport SEXP indmat(SEXP n);
RcppExport SEXP dchisqinvlogd1R(SEXP _X, SEXP _v1);
RcppExport SEXP dchisqinvlogdVVR(SEXP _X, SEXP _v1);
RcppExport SEXP dchisqinvlogdVSR(SEXP _X, SEXP _v1);
RcppExport SEXP DetectR(SEXP Diji, SEXP Liji, SEXP _g00, SEXP _gl0);
RcppExport SEXP birthR( SEXP ab1, SEXP csi2);
RcppExport SEXP captureR(SEXP x1, SEXP ab1, SEXP ad1, SEXP Dij);
#endif /* GILLIAM_H_ */
