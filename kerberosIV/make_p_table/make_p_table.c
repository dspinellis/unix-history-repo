/*
 * $Source: /usr/src/kerberosIV/make_p_table/RCS/make_p_table.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1985, 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please
 * see the file <mit-copyright.h>.
 *
 */

#include <mit-copyright.h>
#include <stdio.h>
#include "des_internal.h"
#include "tables.h"

extern unsigned long swap_byte_bits();
extern unsigned long rev_swap_bit_pos_0();
static unsigned char P_temp[32];
static unsigned long P_prime[4][256];

void gen(stream)
    FILE *stream;
{
    register i,j,k,m;
    /* P permutes 32 bit input R1 into 32 bit output R2 */

#ifdef BIG
    /* flip p into p_temp */
    for (i = 0; i<32; i++)
	P_temp[P[rev_swap_bit_pos_0(i)]] = rev_swap_bit_pos_0(i);

    /*
     * now for each byte of input, figure out all possible combinations
     */
    for (i = 0; i <4 ; i ++) {	/* each input byte */
	for (j = 0; j<256; j++) { /* each possible byte value */
	    /* flip bit order */
	    k = j;
	    /* swap_byte_bits(j); */
	    for (m = 0; m < 8; m++) { /* each bit */
		if (k & (1 << m)) {
		    /* set output values */
		    P_prime[i][j] |= 1 << P_temp[(i*8)+m];
		}
	    }
	}
    }

    fprintf(stream,
	    "\n\tstatic unsigned long const P_prime[4][256] = {\n\t");
    for (i = 0; i < 4; i++) {
	fprintf(stream,"\n");
	for (j = 0; j < 64; j++) {
	    fprintf(stream,"\n");
	    for (k = 0; k < 4; k++) {
		fprintf(stream,"0x%08X",P_prime[i][j*4+k]);
		if ((i == 3) && (j == 63) && (k == 3))
		    fprintf(stream,"\n};");
		else
		    fprintf(stream,", ");
	    }
	}
    }

#endif
    fprintf(stream,"\n");
}
