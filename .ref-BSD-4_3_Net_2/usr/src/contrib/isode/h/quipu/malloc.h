/* malloc.h - Quipu specific mallocing */

/* 
 * $Header: /f/osi/h/quipu/RCS/malloc.h,v 7.1 91/02/22 09:25:57 mrose Interim $
 *
 * 
 * $Log:	malloc.h,v $
 * Revision 7.1  91/02/22  09:25:57  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:57:06  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef _QUIPUMALLOC_
#define _QUIPUMALLOC_

extern unsigned mem_heap;
extern unsigned attr_index;

/*
#define MALLOCSTACK
*/

/* HEAP Selection */
/* Two main heaps - general and database.
 * Database is split into main critical part, 
 * and several parts for attribute values.
 */
#define GENERAL_HEAP	mem_heap = 0;	/* general mallocing */
#define DATABASE_HEAP	mem_heap = 1;	/* critical database structures */
#define ATTRIBUTE_HEAP	{if (mem_heap == 1) mem_heap = 2 + attr_index;}
					/* non critical database structures */
#define RESTORE_HEAP	{if (mem_heap >= 2) mem_heap = 1;}

#define SET_HEAP(x)	{if (mem_heap == 1) mem_heap = 2 + set_heap (x);}
#endif
