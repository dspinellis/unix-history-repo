/*
 * $Source: /mit/kerberos/src/lib/des/RCS/misc.c,v $
 * $Author: jtkohl $
 *
 * Copyright 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information,
 * please seethe file <mit-copyright.h>.
 *
 * This file contains most of the routines needed by the various
 * make_foo programs, to account for bit- and byte-ordering on
 * different machine types.  It also contains other routines useful in
 * generating the intermediate source files.
 */

#include <mit-copyright.h>
#include <stdio.h>
#include "des_internal.h"

/*
 * The DES algorithm is defined in terms of MSBFIRST, so sometimes,
 * e.g.  VAXes, we need to fix it up.  ANSI order means the DES
 * MSBFIRST order.
 */

#if 0 /* These don't seem to get used anywhere.... */
void swap_bits(array)
    char *array;
{
#ifdef MSBFIRST
    /* just return */
    return;
#else /* LSBFIRST */
    register old,new,i,j;

    /* for an eight byte block-- */
    /* flips the bit order within each byte from 0 lsb to 0 msb */
    for (i = 0; i<=7; i++) {
        old = *array;
        new = 0;
        for (j = 0; j<=7; j++) {
            new |= old & 01;    /* copy a bit */
            if (j < 7) {
                /* rotate in opposite directions */
                old = old >> 1;
                new = new << 1;
            }
        }
        *array++ = new;
    }
#endif /* MSBFIRST */
}

unsigned long long_swap_bits(x)
    unsigned long x;
{
#ifdef MSBFIRST
    return x;
#else
    char *array = (char *) &x;
    register old,new,i,j;

    /* flips the bit order within each byte from 0 lsb to 0 msb */
    for (i = 0; i <= (sizeof(long)-1); i++) {
        old = *array;
        new = 0;
        for (j = 0; j<=7; j++) {
            if (old & 01)
                new = new | 01;
            if (j < 7) {
                old = old >> 1;
                new = new << 1;
            }
        }
        *array++ = new;
    }
    return x;
#endif /* LSBFIRST */
}
#endif /* 0 */

unsigned long swap_six_bits_to_ansi(old)
    unsigned long old;
{
    register unsigned long new, j;

    /* flips the bit order within each byte from 0 lsb to 0 msb */
    new = 0;
    for (j = 0; j<=5; j++) {
        new |= old & 01;        /* copy a bit */
        if (j < 5) {
            /* rotate in opposite directions */
            old = old >> 1;
            new = new << 1;
        }
    }
    return new;
}

unsigned long swap_four_bits_to_ansi(old)
    unsigned long old;
{
    register unsigned long new,j;

    /* flips the bit order within each byte from 0 lsb to 0 msb */
    new = 0;
    for (j = 0; j<=3; j++) {
        new |= (old & 01);      /* copy a bit */
        if (j < 3) {
            old = old >> 1;
            new = new << 1;
        }
    }
    return new;
}

unsigned long swap_bit_pos_1(x)
    unsigned long x;
{
    /*
     * This corrects for the bit ordering of the algorithm, e.g.
     * bit 0 ==> msb, bit 7 lsb.
     *
     * given the number of a bit position, >=1, flips the bit order
     * each byte. e.g. bit 3 --> bit 6, bit 13 --> bit 12
     */
    register y,z;

    /* always do it, only used by des_make_key_perm.c so far */
    y = (x-1)/8;
    z = (x-1)%8;

    x = (8-z) + (y*8);

    return x;
}

unsigned long swap_bit_pos_0(x)
    unsigned long x;
{
    /*  zero based version */

    /*
     * This corrects for the bit ordering of the algorithm, e.g.
     * bit 0 ==> msb, bit 7 lsb.
     */

#ifdef MSBFIRST
    return x;
#else /* LSBFIRST */
    register y,z;

    /*
     * given the number of a bit position, >=0, flips the bit order
     * each byte. e.g. bit 3 --> bit 6, bit 13 --> bit 12
     */
    y = x/8;
    z = x%8;

    x = (7-z) + (y*8);

    return x;
#endif /* LSBFIRST */
}

unsigned long swap_bit_pos_0_to_ansi(x)
    unsigned long x;
{
    /* zero based version */

    /*
     * This corrects for the bit ordering of the algorithm, e.g.
     * bit 0 ==> msb, bit 7 lsb.
     */

    register y,z;
    /*
     * given the number of a bit position, >=0, flips the bit order each
     * byte. e.g. bit 3 --> bit 6, bit 13 --> bit 12
     */
    y = x/8;
    z = x%8;

    x = (7-z) + (y*8);

    return x;
}

unsigned long rev_swap_bit_pos_0(x)
    unsigned long x;
{
    /* zero based version */

    /*
     * This corrects for the bit ordering of the algorithm, e.g.
     *  bit 0 ==> msb, bit 7 lsb.
     *
     * Role of LSB and MSB flipped from the swap_bit_pos_0()
     */

#ifdef LSBFIRST
    return x;
#else /* MSBFIRST */

    register y,z;

    /*
     * given the number of a bit position, >=0, flips the bit order each
     * byte. e.g. bit 3 --> bit 6, bit 13 --> bit 12
     */
    y = x/8;
    z = x%8;

    x = (7-z) + (y*8);

    return x;
#endif /* MSBFIRST */
}

unsigned long swap_byte_bits(x)
    unsigned long x;
{
#ifdef MSBFIRST
    return x;
#else /* LSBFIRST */

    char *array = (char *) &x;
    register unsigned long old,new,j;

    /* flips the bit order within each byte from 0 lsb to 0 msb */
    old = *array;
    new = 0;
    for (j = 0; j<=7; j++) {
        new |= (old & 01);      /* copy a bit */
        if (j < 7) {
            old = old >> 1;
            new = new << 1;
        }
    }
    return new;
#endif /* LSBFIRST */
}

swap_long_bytes_bit_number(x)
    unsigned long x;
{
    /*
     * given a bit number (0-31) from a vax, swap the byte part of the
     * bit number to change the byte ordering to mSBFIRST type
     */
#ifdef LSBFIRST
    return x;
#else /* MSBFIRST */
    unsigned long y,z;

    y = x/8;                    /* initial byte component */
    z = x%8;                    /* bit within byte */

    x = (3-y)*8 +z;
    return x;
#endif /* MSBFIRST */
}

void test_set(stream, src, testbit, dest, setbit)
    FILE *stream;
    const char *src;
    int testbit;
    const char *dest;
    int setbit;
{
#ifdef DES_SHIFT_SHIFT
    if (testbit == setbit)
        fprintf(stream, "    %s |=  %s & (1<<%2d);\n",
                dest, src, testbit);
    else
        fprintf(stream, "    %s |= (%s & (1<<%2d)) %s %2d;\n",
                dest, src, testbit,
                (testbit < setbit) ? "<<" : ">>",
                abs(testbit - setbit));
#else
    fprintf(stream,
            "    if (%s & (1<<%2d))  %s |= 1<<%2d;\n",
            src, testbit, dest, setbit);
#endif
}

extern void gen PROTOTYPE((FILE * stream));
int des_debug;
char const *whoami;

main(argc, argv)
    int argc;
    char *argv[];
{
    char *filename;
    char *arg;
    FILE * stream;

    whoami = argv[0];
    filename = (char *)NULL;

    while (argc--, *++argv) {
        arg = *argv;
        if (*arg == '-') {
            if (!strcmp(arg, "-d") && !strcmp(arg, "-debug"))
                des_debug++;
            else {
                fprintf(stderr, "%s: unknown control argument %s\n",
                        whoami, arg);
                goto usage;
            }
        }
        else if (filename) {
            fprintf(stderr,
                    "%s: multiple file names provided: %s, %s\n",
                    whoami, filename, arg);
            goto usage;
        }
        else
            filename = arg;
    }

    if (!filename) {
        fprintf(stderr, "%s: no file name provided\n", whoami);
        goto usage;
    }

    stream = fopen(filename, "w");
    if (!stream) {
        perror(filename);
    usage:
        fprintf(stderr, "usage: %s [-debug] filename\n", whoami);
        exit(1);
    }

    fputs(
      "/* This file is automatically generated.  Do not edit it. */\n",
          stream);

    /* This routine will generate the contents of the file. */
    gen(stream);
    if (fclose(stream) == EOF) {
        perror(filename);
        exit(1);
    }
    exit(0);
}
