/*
 * $Source: /mit/kerberos/src/lib/des/RCS/make_odd.c,v $
 * $Author: steiner $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see
 * the file <mit-copyright.h>.
 *
 * This routine generates an odd-parity table for use in key generation.
 */

#include <mit-copyright.h>
#include <stdio.h>

void gen(stream)
    FILE *stream;
{
    /*
     * map a byte into its equivalent with odd parity, where odd
     * parity is in the least significant bit
     */
    register i, j, k, odd;

    fprintf(stream,
            "static unsigned char const odd_parity[256] = {\n");

    for (i = 0; i < 256; i++) {
        odd = 0;
        /* shift out the lsb parity bit */
        k = i >> 1;
        /* then count the other bits */
        for (j = 0; j < 7; j++) {
            odd ^= (k&1);
            k = k >> 1;
        }
        k = i&~1;
        if (!odd)
            k |= 1;
        fprintf(stream, "%3d", k);
        if (i < 255)
            fprintf(stream, ", ");
        if (i%8 == 0)
            fprintf(stream, "\n");
    }
    fprintf(stream, "};\n");
}
