#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "../ascii/ascebc.h"
#include "../ctlr/ebc_disp.h"


void
main()
{
    int i;

    /* For each display code, find the ascii code that matches */

    printf("unsigned char disp_asc[%d] = {", sizeof disp_ebc);
    for (i = 0; i < sizeof disp_ebc; i++) {
	if ((i%8) == 0) {
	    printf("\n");
	}
	printf("\t");
	printf("0x%2x,", ebcasc[0][disp_ebc[i]]);
    }
    printf("\n};\n");
}
