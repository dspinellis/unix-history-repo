/*
 * $Source: /mit/kerberos/src/lib/des/RCS/make_p.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1985, 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please
 * see the file <mit-copyright.h>.
 *
 * This routine generates the P permutation code for the DES.
 */

#include <mit-copyright.h>
#include <stdio.h>
#include "des_internal.h"
#include "tables.h"

void gen(stream)
    FILE *stream;
{
    /* P permutes 32 bit input R1 into 32 bit output R2 */	

    /* clear the output */
    fprintf(stream,"    L2 = 0;\n");
#ifndef	BIG
    fprintf(stream,"    R2 = 0;\n");
    fprintf(stream,
	    "/* P operations */\n/* from right to right */\n");
    /* first list mapping from left to left */
    for (i = 0; i <=31; i++)
	if (P[i] < 32)
	    fprintf(stream,
		    "    if (R1 & (1<<%d)) R2 |= 1<<%d;\n",P[i],i);
#else /* BIG */
    /* flip p into p_temp */
    fprintf(stream,"    P_temp = R1;\n");
    fprintf(stream,"    P_temp_p = (unsigned char *) &P_temp;\n");
 
#ifdef	LSBFIRST
    fprintf(stream,"    R2 = P_prime[0][*P_temp_p++];\n");
    fprintf(stream,"    R2 |= P_prime[1][*P_temp_p++];\n");
    fprintf(stream,"    R2 |= P_prime[2][*P_temp_p++];\n");
    fprintf(stream,"    R2 |= P_prime[3][*P_temp_p];\n");
#else /* MSBFIRST */
    fprintf(stream,"    R2 = P_prime[3][*P_temp_p++];\n");
    fprintf(stream,"    R2 |= P_prime[2][*P_temp_p++];\n");
    fprintf(stream,"    R2 |= P_prime[1][*P_temp_p++];\n");
    fprintf(stream,"    R2 |= P_prime[0][*P_temp_p];\n");
#endif /* MSBFIRST */
#endif /* BIG */
}
