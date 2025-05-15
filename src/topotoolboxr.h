#ifndef TOPOTOOLBOXR_H
#define TOPOTOOLBOXR_H

void wrap_gradient8(float *outputR,float *demR, float *cellsizeR,int *use_mpR, int* dimsR);

void wrap_fillsink(float *output, float *dem, int *bcR, int *dimsR);

#endif // TOPOTOOLBOXR_H
