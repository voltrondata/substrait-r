// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// test-encode.cpp
raws r_encode_substrait_Type_Boolean(int type_variation_reference, int nullablity);
extern "C" SEXP _substrait_r_encode_substrait_Type_Boolean(SEXP type_variation_reference, SEXP nullablity) {
  BEGIN_CPP11
    return cpp11::as_sexp(r_encode_substrait_Type_Boolean(cpp11::as_cpp<cpp11::decay_t<int>>(type_variation_reference), cpp11::as_cpp<cpp11::decay_t<int>>(nullablity)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_substrait_r_encode_substrait_Type_Boolean", (DL_FUNC) &_substrait_r_encode_substrait_Type_Boolean, 2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_substrait(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}