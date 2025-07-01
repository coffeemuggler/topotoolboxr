#include <stddef.h>
#include <stdint.h>
#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_gradient8(float *outputR, float *demR, float *cellsizeR,int *use_mpR, int *dimsR){

   ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};

   gradient8(outputR, demR, *cellsizeR, *use_mpR, dims);
}
