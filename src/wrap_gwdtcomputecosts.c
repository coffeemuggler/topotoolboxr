#include <stddef.h>
#include <stdint.h>
#include <R.h>
#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_gwdtcomputecosts(float *costsR, int *connsR, int *flatsR, float *demR, float *filledR, int *dimsR){

  ptrdiff_t *conns = {connsR};
  ptrdiff_t dims[2] = {dimsR[0], dimsR[1]};
  
  gwdt_computecosts(costsR, conns, flatsR, demR, filledR, dims);
}
