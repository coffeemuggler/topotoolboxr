#ifndef TOPOTOOLBOXR_H
#define TOPOTOOLBOXR_H

void wrap_gradient8(float *outputR,float *demR, float *cellsizeR,int *use_mpR, int* dimsR);

void wrap_fillsinks(float *output, float *dem, int *bcR, int *dimsR);

void wrap_identifyflats(int *output, float *dem, int *dimsR);

void wrap_gwdtcomputecosts(float *costs, int *conns, int *flats, float *dem, float *filled, int *dimsR);

#endif // TOPOTOOLBOXR_H
