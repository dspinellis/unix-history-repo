/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)romvec.h	7.2 (Berkeley) %G%
 */

/* romvec.h Oct-22-1991 */


#define RVPtr	((struct romvec *) 0x41000000)

#define ROM_memsize	(*((int *) *RVPtr->vec03))
#define	ROM_getchar	(*RVPtr->vec06)
#define	ROM_putchar	(*RVPtr->vec07)
#define	ROM_abort	(*RVPtr->vec25)
#define ROM_plane	(*((int *) *RVPtr->vec46))

struct romvec {
    int     (*vec00)();    /* 00 [00] - Cold Boot Entry */
    int     (*vec01)();    /* 01 [04] */
    int     (*vec02)();    /* 02 [08] */
    int     (*vec03)();    /* 03 [0C] - memsize : Memory Size */
    int     (*vec04)();    /* 04 [10] */
    int     (*vec05)();    /* 05 [14] */
    int     (*vec06)();    /* 06 [18] - getchar : get 1 charactor from console	*/
    int     (*vec07)();    /* 07 [1C] - putchar : put 1 charactor to console		*/
    int     (*vec08)();    /* 08 [20] */
    int     (*vec09)();    /* 09 [24] */
    int     (*vec10)();    /* 10 [28] */
    int     (*vec11)();    /* 11 [2C] */
    int     (*vec12)();    /* 12 [30] */
    int     (*vec13)();    /* 13 [34] */
    int     (*vec14)();    /* 14 [38] */
    int     (*vec15)();    /* 15 [3C] */
    int     (*vec16)();    /* 16 [40] */
    int     (*vec17)();    /* 17 [44] */
    int     (*vec18)();    /* 18 [48] */
    int     (*vec19)();    /* 19 [4C] */
    int     (*vec20)();    /* 20 [50] */
    int     (*vec21)();    /* 21 [54] */
    int     (*vec22)();    /* 22 [58] */
    int     (*vec23)();    /* 23 [5C] */
    int     (*vec24)();    /* 24 [60] */
    int     (*vec25)();    /* 25 [64] - abort : back to ROM Monitor */
    int     (*vec26)();    /* 26 [68] */
    int     (*vec27)();    /* 27 [6C] */
    int     (*vec28)();    /* 28 [70] */
    int     (*vec29)();    /* 29 [74] */
    int     (*vec30)();    /* 30 [78] */
    int     (*vec31)();    /* 31 [7C] */
    int     (*vec32)();    /* 32 [80] */
    int     (*vec33)();    /* 33 [84] */
    int     (*vec34)();    /* 34 [88] */
    int     (*vec35)();    /* 35 [8C] */
    int     (*vec36)();    /* 36 [90] */
    int     (*vec37)();    /* 37 [94] */
    int     (*vec38)();    /* 38 [98] */
    int     (*vec39)();    /* 39 [9C] */
    int     (*vec40)();    /* 40 [A0] */
    int     (*vec41)();    /* 41 [A4] */
    int     (*vec42)();    /* 42 [A8] */
    int     (*vec43)();    /* 43 [AC] */
    int     (*vec44)();    /* 44 [B0] */
    int     (*vec45)();    /* 45 [B4] */
    int     (*vec46)();    /* 46 [B8] -- number of plane */
    int     (*vec47)();    /* 47 [BC] */
    int     (*vec48)();    /* 48 [C0] */
    int     (*vec49)();    /* 49 [C4] */
    int     (*vec50)();    /* 50 [C8] */
    int     (*vec51)();    /* 51 [CC] */
    int     (*vec52)();    /* 52 [D0] */
    int     (*vec53)();    /* 53 [D4] */
    int     (*vec54)();    /* 54 [D8] */
    int     (*vec55)();    /* 55 [DC] */
    int     (*vec56)();    /* 56 [E0] */
    int     (*vec57)();    /* 57 [E4] */
    int     (*vec58)();    /* 58 [E8] */
    int     (*vec59)();    /* 59 [EC] */
    int     (*vec60)();    /* 60 [F0] */
    int     (*vec61)();    /* 61 [F4] */
    int     (*vec62)();    /* 62 [F8] */
    int     (*vec63)();    /* 63 [FC] */
};


