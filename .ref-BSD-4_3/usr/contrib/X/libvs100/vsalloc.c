/* $Header: vsalloc.c,v 10.3 86/02/01 15:47:58 tony Rel $ */
/* vsalloc.c	routines to allocate and free vs framebuffer memory
 *
 *	VSMemInit	Initializes the free list
 *	VSAlloc		Allocates a chunk of memory
 *	VSFree		Frees a chunk of memory
 *
 *	The method used is a variant on the method described in Knuth
 * volume 1, page 441.  The modification is that since we can't directly
 * modify the memory in the vs, we maintain an ordered linked list of
 * extents with either free chain pointers or addresses of blocks
 *
 */

/****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

#include "vs100.h"
#include <errno.h>

extern int errno;

char *Xalloc();

#define slopsize 10	/* Amount we're willing to overallocate */

static VSArea listhead;

/* Initialize the memory structures.  Called by DownLoad, usually */

int VSMemInit()
{
	MemArea freefb, freepg;
	register VSArea *fb, *buf, *pg;
	VSArea *AllocVSArea();

	if (ReportStatus ((int *) NULL, (short *) NULL, (short *) NULL,
			  (BitMap *) NULL, &freefb, &freepg, (MemArea *) NULL,
			  0) == -1) return (-1);

	if ((fb = AllocVSArea()) == NULL) return (-1);
	if ((buf = AllocVSArea()) == NULL) return (-1);
	if ((pg = AllocVSArea()) == NULL) return (-1);

	/* Make the list of memory areas */

	listhead.next = buf->prev = fb;
	fb->next = pg->prev = buf;
	buf->next = listhead.prev = pg;
	pg->next = fb->prev = &listhead;

	/* Now set up the free list */

	listhead.vsFree.next = pg->vsFree.prev = fb;
	fb->vsFree.next = listhead.vsFree.prev = pg;
	pg->vsFree.next = fb->vsFree.prev = &listhead;

	/* Set up the sizes and addresses */

	listhead.vsPtr = NULL;
	listhead.vsFreeFlag = VS_INUSE;
	listhead.vsSize = 0;

	fb->vsPtr = *(caddr_t *) freefb.m_base;
	fb->vsFreeFlag = VS_FREE;
	fb->vsSize = *(long *) freefb.m_size;

	/* There's at least one version of the software that returns
	   bogus information there, so... (ug) */

	if (fb->vsPtr == (caddr_t) 0x12ee00) {
	    fb->vsPtr = (caddr_t) 0x117700;
	    fb->vsSize = 35072;
	}

	/* Hack to deal with the crummy framebuffer bug:  can't use
	   first 8 bytes */

	if (fb->vsPtr == (caddr_t) 0x117700) {
	    fb->vsPtr += 8;
	    fb->vsSize -= 8;
	}

	buf->vsPtr = 0;
	buf->vsFreeFlag = VS_INUSE;
	buf->vsSize = 0;

	pg->vsPtr = *(caddr_t *) freepg.m_base;
	pg->vsFreeFlag = VS_FREE;
	pg->vsSize = *(long *) freepg.m_size;

	return (0);
}

/* Allocate size bytes of specified type (bitmap, halftone, or font) */

VSArea *VSAlloc (size, type)
	int size, type;
{
	register VSArea *f = listhead.vsFree.next, *new;
	
	/* Make sure size is a multiple of 2 */

	if (size & 0x1) size++;

	while (f != &listhead && f->vsSize < size) f = f->vsFree.next;

	if (f == &listhead) {
	    DeallocateSpace();
	    errno = ENOMEM;
	    return (NULL);	/* No space */
	}

	if (f->vsSize <= size + slopsize) {	/* Don't split block */
	    f->vsFree.next->vsFree.prev = f->vsFree.prev;
	    f->vsFree.prev->vsFree.next = f->vsFree.next;
	    f->vsFreeFlag = VS_INUSE;
	    f->vsType = type;
	    return (f);

	} else {	/* Split into two smaller blocks */
	    if ((new = AllocVSArea()) == NULL) return (NULL);
	    new->prev = f->prev;
	    new->next = f;
	    f->prev->next = new;
	    f->prev = new;
	    new->vsSize = size;
	    f->vsSize -= size;
	    new->vsPtr = f->vsPtr;
	    f->vsPtr += size;

	    new->vsFreeFlag = VS_INUSE;
	    new->vsType = type;

	    return (new);
	}
}

/* Free the allocated storage */

int VSFree (block)
	register VSArea *block;
{
	register VSArea *temp;

	if (block->prev->vsFreeFlag == VS_FREE) { /* Coalesce areas */
	    temp = block;
	    block = block->prev;
	    block->vsSize += temp->vsSize;
	    block->next = temp->next;
	    temp->next->prev = block;
	    FreeVSArea (temp);
	    block->vsFree.next->vsFree.prev = block->vsFree.prev;
	    block->vsFree.prev->vsFree.next = block->vsFree.next;
	}

	if (block->next->vsFreeFlag == VS_FREE) { /* ditto */
	    temp = block;
	    block = block->next;
	    block->vsPtr = temp->vsPtr;
	    block->vsSize += temp->vsSize;
	    block->prev = temp->prev;
	    temp->prev->next = block;
	    FreeVSArea (temp);
	    block->vsFree.next->vsFree.prev = block->vsFree.prev;
	    block->vsFree.prev->vsFree.next = block->vsFree.next;
	}

	/* Link the block into the free list */

	block->vsFreeFlag = VS_FREE;
	block->vsFree.next = listhead.vsFree.next;
	block->vsFree.prev = &listhead;
	listhead.vsFree.next->vsFree.prev = block;
	listhead.vsFree.next = block;
}

/* Boring routines to manage the allocation of the memory area descriptors
 * on the Vax.
 */

static VSArea *freeVSAreaHead = NULL;
#define alloc_at_once 10

VSArea *AllocVSArea()
{
	register VSArea *block;
	register int i;

	if ((block = freeVSAreaHead) == NULL) {
	    block = (VSArea *) Xalloc (alloc_at_once * sizeof (VSArea));
	    freeVSAreaHead = block;
	    i = alloc_at_once;
	    while (--i)
		block = block->next = block + 1;
	    block->next = NULL;
	    block = freeVSAreaHead;
	}
	freeVSAreaHead = block->next;

	return (block);
}

FreeVSArea(block)
	VSArea *block;
{
	block->next = freeVSAreaHead;
	freeVSAreaHead = block;
}
