// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Estep
Rcpp::List Estep(arma::mat X, arma::mat A, arma::mat C, arma::mat Q, arma::mat R, arma::colvec F_0, arma::mat P_0);
RcppExport SEXP _dfms_Estep(SEXP XSEXP, SEXP ASEXP, SEXP CSEXP, SEXP QSEXP, SEXP RSEXP, SEXP F_0SEXP, SEXP P_0SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type C(CSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Q(QSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type R(RSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type F_0(F_0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type P_0(P_0SEXP);
    rcpp_result_gen = Rcpp::wrap(Estep(X, A, C, Q, R, F_0, P_0));
    return rcpp_result_gen;
END_RCPP
}
// SKF
Rcpp::List SKF(arma::mat X, arma::mat A, arma::mat C, arma::mat Q, arma::mat R, arma::colvec F_0, arma::mat P_0, bool retLL);
RcppExport SEXP _dfms_SKF(SEXP XSEXP, SEXP ASEXP, SEXP CSEXP, SEXP QSEXP, SEXP RSEXP, SEXP F_0SEXP, SEXP P_0SEXP, SEXP retLLSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type C(CSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Q(QSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type R(RSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type F_0(F_0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type P_0(P_0SEXP);
    Rcpp::traits::input_parameter< bool >::type retLL(retLLSEXP);
    rcpp_result_gen = Rcpp::wrap(SKF(X, A, C, Q, R, F_0, P_0, retLL));
    return rcpp_result_gen;
END_RCPP
}
// FIS
Rcpp::List FIS(arma::mat A, arma::mat ZTf, arma::mat ZTp, Rcpp::NumericVector VTf_v, Rcpp::NumericVector VTp_v, SEXP F_0SEXP, SEXP P_0SEXP);
RcppExport SEXP _dfms_FIS(SEXP ASEXP, SEXP ZTfSEXP, SEXP ZTpSEXP, SEXP VTf_vSEXP, SEXP VTp_vSEXP, SEXP F_0SEXPSEXP, SEXP P_0SEXPSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ZTf(ZTfSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type ZTp(ZTpSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type VTf_v(VTf_vSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type VTp_v(VTp_vSEXP);
    Rcpp::traits::input_parameter< SEXP >::type F_0SEXP(F_0SEXPSEXP);
    Rcpp::traits::input_parameter< SEXP >::type P_0SEXP(P_0SEXPSEXP);
    rcpp_result_gen = Rcpp::wrap(FIS(A, ZTf, ZTp, VTf_v, VTp_v, F_0SEXP, P_0SEXP));
    return rcpp_result_gen;
END_RCPP
}
// SKFS
Rcpp::List SKFS(arma::mat X, arma::mat A, arma::mat C, arma::mat Q, arma::mat R, arma::colvec F_0, arma::mat P_0, bool retLL);
RcppExport SEXP _dfms_SKFS(SEXP XSEXP, SEXP ASEXP, SEXP CSEXP, SEXP QSEXP, SEXP RSEXP, SEXP F_0SEXP, SEXP P_0SEXP, SEXP retLLSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type A(ASEXP);
    Rcpp::traits::input_parameter< arma::mat >::type C(CSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Q(QSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type R(RSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type F_0(F_0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type P_0(P_0SEXP);
    Rcpp::traits::input_parameter< bool >::type retLL(retLLSEXP);
    rcpp_result_gen = Rcpp::wrap(SKFS(X, A, C, Q, R, F_0, P_0, retLL));
    return rcpp_result_gen;
END_RCPP
}
// ainv
SEXP ainv(SEXP x);
RcppExport SEXP _dfms_ainv(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(ainv(x));
    return rcpp_result_gen;
END_RCPP
}
// apinv
SEXP apinv(SEXP x);
RcppExport SEXP _dfms_apinv(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(apinv(x));
    return rcpp_result_gen;
END_RCPP
}
