/*
 * $Source: /mit/kerberos/src/lib/des/RCS/make_s_table.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1985, 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please
 * see the file <mit-copyright.h>.
 */

#include <mit-copyright.h>
#include <stdio.h>
#include "des_internal.h"
#include "tables.h"

extern unsigned long swap_bit_pos_0();
extern unsigned long swap_six_bits_to_ansi();
extern unsigned long swap_four_bits_to_ansi();
char temp[8][64];
int des_debug;

void gen(stream)
    FILE *stream;
{
    register unsigned long i,j,k,l,m,n;

    /* rearrange the S table entries, and adjust for host bit order */

    fprintf(stream, "static unsigned char const S_adj[8][64] = {");
    fprintf(stream, "    /* adjusted */\n");

    for (i = 0; i<=7 ; i++) {
        for (j = 0; j <= 63; j++) {
            /*
             * figure out which one to put in the new S[i][j]
             *
             * start by assuming the value of the input bits is "j" in
             * host order, then figure out what it means in standard
             * form.
             */
            k = swap_six_bits_to_ansi(j);
            /* figure out the index for k */
            l = (((k >> 5) & 01) << 5)
                + ((k & 01) <<4) + ((k >> 1) & 0xf);
            m = S[i][l];
            /* restore in host order */
            n = swap_four_bits_to_ansi(m);
            if (des_debug)
                fprintf(stderr,
                "i = %d, j = %d, k = %d, l = %d, m = %d, n = %d\n",
                        i,j,k,l,m,n);
            temp[i][j] = n;
        }
    }

    for (i = 0; i<=7; i++) {
        fprintf(stream,"\n");
        k =0;
        for (j = 0; j<= 3; j++) {
            fprintf(stream,"\n");
            for (m = 0; m <= 15; m++) {
                fprintf(stream,"%2d",temp[i][k]);
                if ((k++ != 63) || (i !=7)) {
                    fprintf(stream,", ");
                }
            }
        }
    }

    fprintf(stream,"\n};\n");
}
