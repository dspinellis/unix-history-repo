/*
 * $Source: /mit/kerberos/src/lib/des/RCS/des_internal.h,v $
 * $Author: jtkohl $
 * $Header: des_internal.h,v 4.1 88/11/15 11:09:05 jtkohl Exp $ 
 *
 * Copyright 1987, 1988 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 *
 * Private include file for the Data Encryption Standard library.
 */

/* only do the whole thing once	 */
#ifndef DES_INTERNAL_DEFS
#define DES_INTERNAL_DEFS

#include "conf.h"

/*
 * number of iterations of the inner
 * loop of the DES algorithm.  The
 * standard is 16, but in case that is
 * too slow, we might do less.  Of
 * course, less also means less
 * security.
 */
#define	 AUTH_DES_ITER   16

#ifdef  BITS32
/* these are for 32 bit machines */

typedef struct {
    unsigned b0:6;
    unsigned b1:6;
    unsigned b2:6;
    unsigned b3:6;
    unsigned b4:6;
    unsigned b5:2;
}       sbox_in_a;

typedef struct {
    unsigned b5:4;
    unsigned b6:6;
    unsigned b7:6;
}       sbox_in_b;

typedef struct {
    unsigned b0:4;
    unsigned b1:4;
    unsigned b2:4;
    unsigned b3:4;
    unsigned b4:4;
    unsigned b5:4;
    unsigned b6:4;
    unsigned b7:4;
}       sbox_out;

#else	BITS32
/* for sixteen bit machines */

typedef struct {
    unsigned b0:6;
    unsigned b1:6;
    unsigned b2:4;
}       sbox_in_16_a;

typedef struct {
    unsigned b2:2;
    unsigned b3:6;
    unsigned b4:6;
    unsigned b5:2;
}       sbox_in_16_b;

typedef struct {
    unsigned b5:4;
    unsigned b6:6;
    unsigned b7:6;
}       sbox_in_16_c;

typedef struct {
    unsigned b0:4;
    unsigned b1:4;
    unsigned b2:4;
    unsigned b3:4;
    unsigned b4:4;
    unsigned b5:4;
    unsigned b6:4;
    unsigned b7:4;
}       sbox_out;
#endif	BITS32


#endif	DES_INTERNAL_DEFS
