
/********************************************
sizes.h
copyright 1991, Michael D. Brennan

This is a source file for mawk, an implementation of
the AWK programming language.

Mawk is distributed without warranty under the terms of
the GNU General Public License, version 2, 1991.
********************************************/

/* $Log:	sizes.h,v $
 * Revision 5.1  91/12/05  07:59:35  brennan
 * 1.1 pre-release
 * 
*/

/*  sizes.h  */

#ifndef  SIZES_H
#define  SIZES_H

#if     ! HAVE_SMALL_MEMORY
#define EVAL_STACK_SIZE  256  /* limit on recursion */
/* number of fields at startup, must be a power of 2 
   and FBANK_SZ-1 must be divisible by 3! */
#define  FBANK_SZ	256
#define  FB_SHIFT	  8   /* lg(FBANK_SZ) */
#define  NUM_FBANK	128   /* see MAX_FIELD below */

#else  /* have to be frugal with memory */

#define EVAL_STACK_SIZE   64
#define  FBANK_SZ	64
#define  FB_SHIFT	 6   /* lg(FBANK_SZ) */
#define  NUM_FBANK	16   /* see MAX_FIELD below */

#endif  

#define  MAX_SPLIT	(FBANK_SZ-1)   /* needs to be divisble by 3*/
#define  MAX_FIELD	(NUM_FBANK*FBANK_SZ - 1)

#define  MIN_SPRINTF	400


#define  BUFFSZ         4096
  /* starting buffer size for input files, grows if 
     necessary */

#define  HASH_PRIME  53
#define  A_HASH_PRIME 37


#define  MAX_COMPILE_ERRORS  5 /* quit if more than 4 errors */



/* AWF showed the need for this */
#define  MAIN_PAGE_SZ    4096 /* max instr in main block */
#if 0
#define  PAGE_SZ    1024  /* max instructions for other blocks */
#endif 
/* these used to be different */
#define  PAGE_SZ    4096

#endif   /* SIZES_H */
