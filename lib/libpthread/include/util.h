/* ==== util.h ============================================================
 * Copyright (c) 1991, 1992, 1993 by Chris Provenzano, proven@athena.mit.edu	
 *
 * Description : Header file for generic utility functions.
 *
 * 91/08/31 proven - Added exchange.
 * Exchange any two objects of any size in any table.
 *
 * 91/10/06 proven - Cleaned out all the old junk. 
 *
 * 91/03/06 proven - Added getint. 
 */

#include <pthread/copyright.h>

#ifndef	NULL
#define NULL	0
#endif

#undef FALSE
#undef TRUE

typedef enum Boolean {
	FALSE,
	TRUE,
} Boolean;

#define OK					0
#define NUL					'\0'
#define NOTOK				-1

#if ! defined(min)
#define min(a,b) (((a)<(b))?(a):(b))
#define max(a,b) (((a)>(b))?(a):(b))
#endif

/* Alingn the size to the next multiple of 4 bytes */
#define ALIGN4(size)	((size + 3) & ~3)
#define ALIGN8(size)	((size + 7) & ~7)

#ifdef DEBUG
#define	DEBUG0(s)		printf(s)
#define	DEBUG1(s,a)		printf(s,a)
#define	DEBUG2(s,a,b)	printf(s,a,b)
#define	DEBUG3(s,a,b,c)	printf(s,a,b,c)
#else
#define	DEBUG0(s)	
#define	DEBUG1(s)	
#define	DEBUG2(s)	
#define	DEBUG3(s)	
#endif
