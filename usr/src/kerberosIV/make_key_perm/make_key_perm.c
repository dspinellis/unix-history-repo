/*
 * $Source: /mit/kerberos/src/lib/des/RCS/make_key_perm.c,v $
 * $Author: jtkohl $
 * $Locker:  $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * This routine calculates an effective Key schedule set of
 * permutations for des.  Beginning with the pre-defined key schedule
 * algorithm, it reduces it to a set of 16 permutations upon the
 * initial key.  Only needs to execute once to produce a header file.
 * Note that we subtract one from the values ouput to fix up for C
 * subscripts starting at 0.
 */

#include <mit-copyright.h>
#include <stdio.h>
#include <errno.h>
#include "des_internal.h"

#ifndef lint
static char rcsid[]=
    "$Header: make_key_perm.c,v 4.9 88/11/15 11:29:40 jtkohl Exp $";
#endif /* lint */

char *progname;
extern char *errmsg();
extern int errno;
extern long swap_bit_pos_1();
extern long swap_bit_pos_0();
int sflag;
int vflag;
int dflag;
int pid;
int child_status;

int key_position[64+1];
int C[28+1];
int D[28+1];
int C_temp, D_temp;

/*
 *  CONVENTIONS for numbering the bits
 *  bit 0 ==> lsb
 *  L starts at bit 0
 *  R starts at bit 64
 *
 *  BEWARE-- some stuff starts at 0, some at 1;  perhaps some bugs still?
 */

/*
 * Sequence of shifts used for the key schedule.
 */
int const shift[16+1] = { 0,
    1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1,
};

int const pc_1[64+1] = { 0,

    57,49,41,33,25,17, 9,
     1,58,50,42,34,26,18,
    10, 2,59,51,43,35,27,
    19,11, 3,60,52,44,36,

    63,55,47,39,31,23,15,
     7,62,54,46,38,30,22,
    14, 6,61,53,45,37,29,
    21,13, 5,28,20,12, 4,
};


/*
 * Permuted-choice 2, to pick out the bits from
 * the CD array that generate the key schedule.
 */
int const pc_2[48+1] = { 0,

    14,17,11,24, 1, 5,
     3,28,15, 6,21,10,
    23,19,12, 4,26, 8,
    16, 7,27,20,13, 2,

    41,52,31,37,47,55,
    30,40,51,45,33,48,
    44,49,39,56,34,53,
    46,42,50,36,29,32,
};

int ks_perm[16+1][48+1];

int des_debug;

gen(stream)
    FILE *stream;
{
    /*  Local Declarations */
    register i, j, iter;

    /*
     * initialize the key_position array s.t. key_position[i] = i;
     * that is, each element is equal to its starting position.
     *
     * Also adjust for the bit order within bytes.
     */

    for (i=0; i<65; i++)
        key_position[i]= swap_bit_pos_1(i);

    fprintf(stream,"static int const key_perm[16][48] = {\n");

    /*
     * apply pc_1 to initial key_position to create C[0] and D[0]
     * Start at pc_1[1], not pc_1[0]
     */
    for (i=1; i<=28; i++) {
        C[i] = key_position[pc_1[i]];
        D[i] = key_position[pc_1[i+28]];
    }

    /*
     * major loop over the 16 iterations
     * start at iter = 1, not zero.
     */
    for (iter = 1; iter <= 16; iter++) {
        if (des_debug) {
            /*  for debugging */
            printf(
                    "/* DEBUG-- start iteration = %d  shifts = %d",
                    iter, shift[iter]);
            printf("\nC array");
            for (i = 1; i <=4 ; i++) {
                printf("\n");
                for (j = 1; j<=7; j++)
                    printf("%d, ",C[(i-1)*7+j]);
            }
            printf("\n\nD array");
            for (i = 1; i <=4 ; i++) {
                printf("\n");
                for (j = 1; j<=7; j++)
                    printf("%d, ",D[(i-1)*7+j]);
            }
            printf("\n */");
            fflush(stdout);
        }

        /* apply the appropriate left shifts */
        for (i = 1; i <= shift[iter]; i++) {
            C_temp = C[1];
            D_temp = D[1];
            for (j =1; j<=27; j++) {
                C[j] = C[j+1];
                D[j] = D[j+1];
            }
            C[j] = C_temp;
            D[j] = D_temp;
        }


        if (des_debug) {
            /* for debugging */
            printf("/* DEBUG:\n");
            printf(" * after shifts, iteration = %d  shifts = %d",
                    iter, shift[iter]);
            printf("\nC array");
            for (i = 1; i <=4 ; i++) {
                printf("\n");
                for (j = 1; j<=7; j++)
                    printf("%d, ",C[(i-1)*7+j]);
            }
            printf("\n\nD array");
            for (i = 1; i <=4 ; i++) {
                printf("\n");
                for (j = 1; j<=7; j++)
                    printf("%d, ",D[(i-1)*7+j]);
            }
            printf("\n */");
            fflush(stdout);
        }

        /*
         * apply pc_2
         * Start at pc_2[1], not pc_2[0]
         *
         * Start stuffing ks_perm[1][1], not ks_perm[0][0]
         *
         * Adjust ks_perm for bit order if needed.
         */
        for (i = 1; i <= 48; i++) {
            if (pc_2[i] <= 28)
                ks_perm[iter][(i)] = C[pc_2[i]];
            else
                ks_perm[iter][(i)] = D[pc_2[i]-28];
        }

        /* now output the resulting key permutation */
        fprintf(stream, "    /* ks permutation iteration = %2d */",
                iter);
        for (i = 1; i <= 6; i++) {
            fprintf(stream, "\n    ");
            for (j = 1; j <= 8; j++) {
                /*
                 * IMPORTANT -- subtract one from value to adjust to a
                 * zero-based subscript for key
                 */
                fprintf(stream, "%d", ks_perm[iter][(i-1)*8+j]-1);
                /* omit last comma */
                if ((j != 8) || (i != 6) || (iter != 16)) {
                    fprintf(stream,", ");
                }
            }
        }
    }
    fprintf(stream,"\n};\n");
}
