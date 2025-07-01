#include <stddef.h>
#include <stdint.h>
#include <R.h>
#include "topotoolbox.h"
#include "topotoolboxr.h"

void wrap_fillsinks(float *output, float *dem, int *bcR, int *dimsR){
   
   ptrdiff_t dims [2]= {dimsR[0], dimsR[1]};

   // Allocate memory for bc
   uint8_t *bc = R_Calloc(dims[0] * dims[1], uint8_t);

   // Fill bc from the int-valued bcR array
   for (ptrdiff_t j = 0; j < dims[1]; j++) {
     for (ptrdiff_t i = 0; i < dims[0]; i++) {
       bc[j * dims[0] + i] = bcR[j * dims[0] + i];
     }
   }
   
   fillsinks(output, dem, bc, dims);

   // Free the allocated memory
   R_Free(bc);
}
