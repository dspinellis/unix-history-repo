/* tailor.c -- target dependent functions
 * Copyright (C) 1992-1993 Jean-loup Gailly
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 */

/* tailor.c is a bunch of non portable routines.
 * It should be kept to a minimum.
 */

#include "tailor.h"
#include "gzip.h"

#ifndef lint
static char rcsid[] = "$Id: tailor.c,v 0.8 1993/02/24 18:24:54 jloup Exp $";
#endif

#ifdef __TURBOC__

/************************/
/*  Function fcalloc()  */
/************************/

/* Turbo C malloc() does not allow dynamic allocation of 64K bytes
 * and farmalloc(64K) returns a pointer with an offset of 8, so we
 * must fix the pointer. Warning: the pointer must be put back to its
 * original form in order to free it, use fcfree().
 * For MSC, use halloc instead of this function (see tailor.h).
 */
static ush ptr_offset = 0;

void * fcalloc(items, size)
    unsigned items; /* number of items */
    unsigned size;  /* item size */
{
    void * buf = farmalloc((ulg)items*size + 16L);
    if (buf == NULL) return NULL;
    /* Normalize the pointer to seg:0 */
    if (ptr_offset == 0) {
	ptr_offset = (ush)((uch*)buf-0);
    } else if (ptr_offset != (ush)((uch*)buf-0)) {
	error("inconsistent ptr_offset");
    }
    *((ush*)&buf+1) += (ptr_offset + 15) >> 4;
    *(ush*)&buf = 0;
    return buf;
}

void fcfree(ptr)
    void *ptr; /* region allocated with fcalloc() */
{
    /* Put the pointer back to its original form: */
    *((ush*)&ptr+1) -= (ptr_offset + 15) >> 4;
    *(ush*)&ptr = ptr_offset;
    farfree(ptr);
 }

#endif /* __TURBOC__ */
