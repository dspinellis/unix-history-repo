/*******************************************************************
*                                                                  *
*    File: CIFPLOT/alloc.c                                         *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"

IMPORT int *malloc();
IMPORT int *realloc();

#define BLOCKSIZE 4		/* Memory is allocated in multiples of BLOCKSIZE */

/* ALLOC_ID, PALLOC_ID, & FREED_ID are used to mark memory as
 * allocated, permanently allocated, & freed, repectively */
#define ALLOC_ID  0xAA00FFFF
#define PALLOC_ID 0x55AAA0F0
#define FREED_ID  0xAFFA8118

/* size returns how much memory 'x' points to */
#define size(x)	  ( *(x-1) - ((int) x) + 1 )

/* These are variables that are used to keep track of how much
 * memory has been allocated.		*/
int AllocCount,FreeCount,AllocBytes;
int FreeBytes,PallocCount,PallocBytes,MaxBytes;

int *
alloc(nbytes)
int nbytes;
/* Returns a pointer to a block of memory of size 'nbytes' */
{
    int nunits;
    int *ptr;

#ifdef ADEBUG
    nbytes += sizeof(int);
#endif

    nunits = ((nbytes-1)/BLOCKSIZE) + 1;
    if (NULL == (ptr = malloc(nunits*BLOCKSIZE)))
	Error("Out of space!\n",INTERNAL);
    
#ifdef ADEBUG
    /* Set up an identifier to show this has been allocated */
    *ptr = ALLOC_ID;
    AllocCount++;
    AllocBytes += size(ptr);
    if( (AllocBytes-FreeBytes) > MaxBytes )
	MaxBytes = AllocBytes - FreeBytes;
    return(++ptr);
#else
    return(ptr);
#endif
    }

int *
palloc(n)
int n;
/* Allocate n bytes of permanent memory (this memory will not be freed) */
{
    int *p;
    p = alloc(n);
#ifdef ADEBUG
    PallocCount++;
    PallocBytes += n;
    *(p-1) = PALLOC_ID;
#endif
    return(p);
    }

Free(ptr)
int *ptr;
/* Free the block of memory pointed to by 'ptr'. This memory must
 * have been allocated by 'alloc'. */
{

#ifdef ADEBUG
    /* Check to see that the memory was allocated by 'alloc' */
    --ptr;
    if( *ptr != ALLOC_ID)
	if( *ptr == PALLOC_ID )
		Error("Tried to Free Permanent Storage",INTERNAL);
	    else
		if( *ptr == FREED_ID )
			Error("Tried to Free an already free block",INTERNAL);
		    else
			Error("Tried to Free an Unallocated Block",INTERNAL);
    /* Mark this memory as free, if memory is freed again flag an error */
    *ptr = FREED_ID;
    FreeCount++;
    FreeBytes += size(ptr);
#endif
    free(ptr);
    return;
   }

int *
expand(ptr, n)
int *ptr;
int n;
/* 'expand' changes the size of an array pointed to by 'ptr' to be an
 * array of n bytes. This is useful to avoid table overflow. Instead
 * of aborting on overflow we can just make the table bigger.	*/
{
    Free(ptr);
#ifdef ADEBUG
    n += sizeof(int);
    ptr--;
    if(*ptr != FREED_ID)
	Error("Block has not been freed",INTERNAL);
#endif
    ptr = realloc(ptr,n);
    if(ptr == NULL)
	Error("Out of memory",INTERNAL);
#ifdef ADEBUG
    if(*ptr != FREED_ID)
	Error("Block has not been copied correctly",INTERNAL);
    *ptr = ALLOC_ID;
    AllocCount++;
    AllocBytes += size(ptr);
    if( (AllocBytes-FreeBytes) > MaxBytes )
	MaxBytes = AllocBytes - FreeBytes;
    return(++ptr);
#else
    return(ptr);
#endif
    }

int acnt0,acnt1,acnt2,acnt3,acnt4,acnt5,acnt6,acnt7,acnt8,acnt9;
int bcnt0,bcnt1,bcnt2,bcnt3,bcnt4,bcnt5,bcnt6,bcnt7,bcnt8,bcnt9;

AllocSummary()
/* Prints out statistics of memory allocated */
{
   fprintf(stderr,"%6d blocks, %8d bytes allocated\n",AllocCount,AllocBytes);
   fprintf(stderr,"%6d blocks, %8d bytes permanent requests\n",PallocCount,PallocBytes);
   fprintf(stderr,"%6d blocks, %8d bytes freed\n",FreeCount,FreeBytes);
   fprintf(stderr,"%6d blocks, %8d bytes unretrieved\n",
			AllocCount-FreeCount,AllocBytes-FreeBytes);
   fprintf(stderr,"%8d bytes were required\n",MaxBytes);
   fprintf(stderr,"\nPlot Time allocation\n");
   fprintf(stderr,"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n",
	acnt0,acnt1,acnt2,acnt3,acnt4,acnt5,acnt6,acnt7,acnt8,acnt9);
   fprintf(stderr,"%d,%d,%d,%d,%d,%d,%d,%d,%d,%d\n",
	bcnt0,bcnt1,bcnt2,bcnt3,bcnt4,bcnt5,bcnt6,bcnt7,bcnt8,bcnt9);
   }
