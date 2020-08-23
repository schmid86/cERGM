// Custom Terms used for the US Supreme Court Citation Network
// See: https://github.com/desmarais-lab/Supreme_Court_Citation_Network

#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[] = {
    {NULL, NULL, 0}
};

void R_init_ergm_ego(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
