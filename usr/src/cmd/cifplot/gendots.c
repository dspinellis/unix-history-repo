#include "masks.h"

main() {
    int i;
    for(i=0;i<31;i++) printf("\t0x%x,\n",fromMask[i]&toMask[i+1]);
    }
