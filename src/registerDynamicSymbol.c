// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "topotoolbox.h"
#include "topotoolboxr.h"

static const R_CMethodDef cMethods[] = {
   {"wrap_gradient8",(DL_FUNC) &wrap_gradient8,5},
   {NULL,NULL,0,NULL},
};

void R_init_topotoolbox(DllInfo *info) {

  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}


