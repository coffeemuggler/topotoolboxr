#include <stddef.h>
#include <stdint.h>
#include <R.h>
#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_identifyflats(int *outputR, float *demR, int *dimsR){

   ptrdiff_t dims[2] = {dimsR[0], dimsR[1]};

   identifyflats(outputR, demR, dims);
}
