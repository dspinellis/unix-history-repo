/*
 *	$Source: /u1/X/hacks/bitrot/RCS/twiddle.h,v $
 *	$Author: tony $
 *	$Locker:  $
 *	$Header: twiddle.h,v 10.1 86/02/01 15:43:23 tony Rel $
 */

#include <sys/types.h>

/* WARNING!  ASSUMES 8-BIT CHARACTERS! */

/* Each of these operates on the xth bit from map */
/* Map must be a pointer, not necessarily a u_char pointer */
#define fetch(map, x) (1 & (((u_char *) map)[x >> 3] >> (x & 7)))
#define set(map, x) ((((u_char *) map)[x>>3] |= 1 << (x & 7)))
#define reset(map, x) ((((u_char *) map)[x>>3] &= ~(1 << (x & 7))))
#define flip(map,x) ((((u_char *) map)[x>>3] ^=  1 << (x & 7)))
#define place(map, x, bit) ((bit ? set(map,x),0 : reset(map,x), 0))
