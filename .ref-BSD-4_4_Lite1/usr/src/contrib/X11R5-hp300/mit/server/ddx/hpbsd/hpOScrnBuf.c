/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/***********************************************************************
 *  file: hpbuf.c
 *
 *
 *  ******************************************************************
 *  *  (c) Copyright Hewlett-Packard Company, 1986.  All rights are  *
 *  *  reserved.  Copying or other reproduction of this program      *
 *  *  except for archival purposes is prohibited without prior      *
 *  *  written consent of Hewlett-Packard Company.		     *
 *  ******************************************************************
 *
 *  Off-screen display buffer allocator 
 *
 *		Hewlett Packard -- Corvallis Workstation Operation
 *		Project -- port of X to HP9000S300
 *		Dan Garfinkel -- MTS
 *              Bob Leichner -- MTS - port of allocator to X11
 *
 *
 */

#include "scrnintstr.h"
#include "hppriv.h"
#include "topcat/topcat.h"

#define USED	0
#define UNUSED	1
#define CNODE	2

static hpBufAllocInfoPtr screenBufs[MAXSCREENS] = {(hpBufAllocInfoPtr)NULL};

static void combine();
static hpChunk *getchunk();

/*
 * hpBufAlloc and hpBufFree are the off-screen memory buffer allocate 
 *	and free routines. hpBufAllocInit initializes the buffer allocator.
 */

void
hpBufAllocInit(pScreen, XHP_stride, XHP_memheight, XHP_width, XHP_height)
     ScreenPtr pScreen;
     int XHP_stride, XHP_memheight, XHP_width, XHP_height;
{
    register hpBufAllocInfoPtr allocInfo;
    register hpChunk *ch1, *ch2;
    hpPrivScreenPtr pPrivScreen = getPrivScreenPtr(pScreen);

    allocInfo = screenBufs[pScreen->myNum];
    if (allocInfo == (hpBufAllocInfoPtr)NULL)
    {
	allocInfo =
	    (hpBufAllocInfoPtr) xalloc (sizeof(hpBufAllocInfo));
	allocInfo->initialized = 0;
	screenBufs[pScreen->myNum] = allocInfo;
    }
    pPrivScreen->pBufAllocInfo = (pointer)screenBufs[pScreen->myNum];
    
    /*
     * if initialized, free any storage currently in use.
     */
    if (allocInfo->initialized)
    {
	ch1 = allocInfo->used;
	while (ch1)
	{
	    ch2 = ch1->next;
	    xfree (ch1);
	    ch1 = ch2;
	}
	ch1 = allocInfo->unused;
	while (ch1)
	{
	    ch2 = ch1->next;
	    xfree (ch1);
	    ch1 = ch2;
	}
    }
    allocInfo->used = (hpChunk *)NULL;
    allocInfo->unused = (hpChunk *)NULL;
    

    switch (pPrivScreen->gcid) {
      case GCID_TOPCAT:	/* got to check if it's a Catseye too! */
	{
	    TOPCAT *gp_hardware = getTcHardware(pScreen);  
	    (void) getchunk (allocInfo, 0, 0, XHP_stride, XHP_memheight);
	    if (gp_hardware->id_second <= ID2_LCC || gp_hardware->id_second == ID2_CC) 
		allocInfo->firstcol = 0;
	    else 
		allocInfo->firstcol = XHP_width;
        }
	break;

      case GCID_GATORBOX:
	(void) getchunk (allocInfo, 0, 0, XHP_width, 1024);
	allocInfo->firstcol = 0;
	break;

      case GCID_RENAISSANCE:
	(void) getchunk (allocInfo, 0, 0, XHP_stride, XHP_height);
	allocInfo->firstcol = 0;
	break;

      case GCID_DAVINCI:
      case GCID_HYPERION:
	(void) getchunk (allocInfo, 0, 0, XHP_stride, XHP_height);
	allocInfo->firstcol = XHP_width;
	break;
    }

    allocInfo->unused->parent = (hpCnode *) 0;
    allocInfo->initialized = 1;
}

hpChunk *
hpBufAlloc (pScreen, w, h)
     ScreenPtr pScreen;
     register h,w;
{
    register hpChunk *me, *ch1, *ch2;
    register hpCnode *p;
    register hpBufAllocInfoPtr allocInfo = screenBufs[pScreen->myNum];

    if (allocInfo == (hpBufAllocInfoPtr)NULL)
	/* called without initialization. */
	return (hpChunk *) 0;
    
    /*
     * check for h and w in range
     */

    if (h<=0 || w<=0)
	return (hpChunk *) 0;
    

    /*
     * set if there are any unused chunks
     */

    if (allocInfo->unused == (hpChunk *) 0)
	return (hpChunk *) 0;
    
    /* pad buffer request to a word boundary, needed for s800 and hp310
     * machines */
    while (w%4 != 0) w++;

    /*
     * find an unused chunk of the closest size
     */

    me = (hpChunk *) 0;

    for (ch1=allocInfo->unused; ch1 != (hpChunk *) 0; ch1 = ch1->next)
    {
	if (ch1->h >= h && ch1->w >= w)
	{
	    if (me == (hpChunk *) 0)
		me = ch1;
	    else
		if (ch1->h < me->h)
		    me = ch1;
	}
    }

    /*
     * if I fit in no chunk, return with failure
     */

    if (me == (hpChunk *) 0)
	return me;
    
    /*
     * take me out of the unused list 
     */

    if (me->next) me->next->prev = me->prev;

    if (me->prev)
	me->prev->next = me->next;
    else
	allocInfo->unused = me->next;
    

    /*
     * put me into the used list 
     */

    if (allocInfo->used) allocInfo->used->prev = me;
    me->next = allocInfo->used;
    me->prev = (hpChunk *) 0;
    allocInfo->used = me;


    /*
     * check on which way to divide and allocate storage as needed
     */

    ch1 = (hpChunk *) 0;
    ch2 = (hpChunk *) 0;
 
    if (me->x == allocInfo->firstcol)
    {
	/*					+------+--------+
	 * first we are dividing lengthwise	|  me  |  ch1   |
	 * then by height as the picture on	+------+--------+
	 * the right shows.			|      ch2      |
	 *					+---------------+
	 */
	if (me->w != w)
	    ch1 = getchunk (allocInfo, me->x+w, me->y, me->w-w, h);
	if (me->h != h)
	    ch2 = getchunk (allocInfo, me->x, me->y+h, me->w, me->h-h);
    }
    else
    {
	/*					+------+--------+
	 * first we are dividing heightwise	|  me  |        |
	 * then by width as the picture on	+------+  ch2   +
	 * the right shows.			|  ch1 |        |
	 *					+---------------+
	 */
	if (me->h != h)
	    ch1 = getchunk (allocInfo, me->x, me->y+h, w, me->h-h);
	if (me->w != w)
	    ch2 = getchunk (allocInfo, me->x+w, me->y, me->w-w, me->h);
    }

    /*
     * update my new height
     */

    me->h = h;
    me->w = w;

    
    /*
     * get a parent node and link all the chunks properly
     */

    p = (hpCnode *) xalloc (sizeof (hpCnode));

    p->c0 = (hpCnode *) me;
    p->c1 = (hpCnode *) ch1;
    p->c2 = (hpCnode *) ch2;

    p->t0 = USED;
    p->t1 = UNUSED;
    p->t2 = UNUSED;

    if ((p->parent = me->parent) != 0)
    {
	if (me->parent->c0 == (hpCnode *) me)
	{
	    me->parent->c0 = p;
	    me->parent->t0 = CNODE;
	}
	if (me->parent->c1 == (hpCnode *) me)
	{
	    me->parent->c1 = p;
	    me->parent->t1 = CNODE;
	}
	if (me->parent->c2 == (hpCnode *) me)
	{
	    me->parent->c2 = p;
	    me->parent->t2 = CNODE;
	}
    }

    me->parent = p;
    if (ch1) ch1->parent = p;
    if (ch2) ch2->parent = p;

    {
	void (*bitMover)();

	bitMover = getPrivScreenPtr(pScreen)->MoveBits;
	
	if (bitMover)
	{
	    /* clear the area of memory that is being given to the user */
	    (*bitMover)(pScreen, ~0, GXclear,
			me->x,
			me->y,
			me->x,
			me->y,
			me->w, me->h);
	    
	    /* wait for bitMover operation to complete */
	    WAIT_READY_TO_RENDER(pScreen);
	}
	else
	{
	    register int *pClrInt;
	    register int *pClrMax;
	    register short i;
	    int ClrWords, *pClrStart;

	    ClrWords = (me->w + 3) >> 2;
	    pClrStart = (int *)
		(getPrivScreenPtr(pScreen)->bits +
		 (me->y * getPrivScreenPtr(pScreen)->stride) +
		 me->x);

	    for (i=0; i < me->h; i++)
	    {
		pClrInt = pClrStart;
		pClrMax = pClrStart + ClrWords;
		while (pClrInt < pClrMax)
		    *pClrInt++ = 0;
		pClrStart = (int *)((char *)pClrStart +
				    getPrivScreenPtr(pScreen)->stride);
	    }
	}
    }
	
    /*
     * return the chunk id
     */

    return me;
}

void
hpBufFree (pScreen, ch)
     ScreenPtr pScreen;
     register hpChunk * ch;
{
    register hpBufAllocInfoPtr allocInfo = screenBufs[pScreen->myNum];

    /*
     * remove me from the used list and put me un the unused list
     */

    if (ch->next) ch->next->prev = ch->prev;

    if (ch->prev)
	ch->prev->next = ch->next;
    else
	allocInfo->used = ch->next;
    
    if (allocInfo->unused)
	allocInfo->unused->prev = ch;
    ch->next = allocInfo->unused;
    ch->prev = (hpChunk *) 0;
    allocInfo->unused = ch;

    /*
     * modify me parent to say I'm unused
     */

    if (ch->parent->c0 == (hpCnode *) ch) 
	ch->parent->t0 = UNUSED;
    if (ch->parent->c1 == (hpCnode *) ch) 
	ch->parent->t1 = UNUSED;
    if (ch->parent->c2 == (hpCnode *) ch) 
	ch->parent->t2 = UNUSED;

    /*
     * combine freed chunks
     */
 
    combine (allocInfo, ch->parent);
}

/*
 * internal routine to coalate chunks back together
 */

static void
combine (allocInfo, par)
     register hpBufAllocInfoPtr allocInfo;
     register hpCnode *par;
{
    register hpChunk *ch0, *ch1, *ch2;
    register hpCnode *pp;

    ch0 = (hpChunk *) par->c0;
    ch1 = (hpChunk *) par->c1;
    ch2 = (hpChunk *) par->c2;

    /*
     * combine ch0 and ch1
     */

    if (ch1)			/* if ch1 does not exist, go on */
    {
	if (par->t0 != UNUSED || par->t1 != UNUSED)
	    return;		/* return if either are not UNUSED */

	/*
	 * combine ch1 with ch0
         */

	if (ch0->y == ch1->y)
	    ch0->w += ch1->w;
	else
	    ch0->h += ch1->h;

	if (ch1->next)
	    ch1->next->prev = ch1->prev;
        if (ch1->prev)
	    ch1->prev->next = ch1->next;
	else
	    allocInfo->unused = ch1->next;

	xfree (ch1);
        par->c1 = 0;
    }

    /*
     * combine ch0 and ch2
     */

    if (ch2)			/* if ch2 does not exist, go on */
    {
	if (par->t0 != UNUSED || par->t2 != UNUSED)
	    return;		/* return if either are not UNUSED */

	/*
	 * combine ch2 with ch0
         */

	if (ch0->y == ch2->y)
	    ch0->w += ch2->w;
	else
	    ch0->h += ch2->h;

	if (ch2->next)
	    ch2->next->prev = ch2->prev;
        if (ch2->prev)
	    ch2->prev->next = ch2->next;
	else
	    allocInfo->unused = ch2->next;

	xfree (ch2);
    }

    /* 
     * only one chunk left, relink with parent of par and free par
     */

    if (pp = par->parent)
    {
	if (pp->c0 == par)
	{
	    pp->c0 = (hpCnode *) ch0;
	    pp->t0 = UNUSED;
	}
	if (pp->c1 == par)
	{
	    pp->c1 = (hpCnode *) ch0;
	    pp->t1 = UNUSED;
	}
	if (pp->c2 == par)
	{
	    pp->c2 = (hpCnode *) ch0;
	    pp->t2 = UNUSED;
	}
    }
    ch0->parent = pp;

    xfree (par);

    if (pp)
	combine (allocInfo, pp);
}

/*
 * getchunk returns a pointer to an unused chunk at x,y and size w,h
 */

static hpChunk *
getchunk (allocInfo, x, y, w, h)
     register hpBufAllocInfoPtr allocInfo;
     int x, y, w, h;
{
    hpChunk * ch;

    ch = (hpChunk *) xalloc (sizeof (hpChunk));
    ch->x = x;
    ch->y = y;
    ch->w = w;
    ch->h = h;

    /*
     * relink the new chunk to the front of the unused list
     */

    if (allocInfo->unused)
	allocInfo->unused->prev = ch;
    ch->next = allocInfo->unused;
    ch->prev = (hpChunk *) 0;
    ch->parent = (hpCnode *) 0;
    allocInfo->unused = ch;

    return ch;
}


/*
 * an interactive testing program
 */

#ifdef TEST
#include <stdio.h>
hpAllocTest () {
    register i, w, h;
    register hpChunk * ch;
    register hpCnode * par;
    char buf[80];
    ScreenPtr pScreen;
    register hpBufAllocInfoPtr allocInfo;
        
    pScreen =  &(screenInfo.screen[0]);

#ifdef notdef
    hpBufAllocInit (pScreen);
#endif

    allocInfo = screenBufs[pScreen->myNum];

    printf ("total size of buffer is width=%d, height=%d\n", 
		allocInfo->unused->w, allocInfo->unused->h);

    while (1) {
	printf ("\nSelect function -- examin, get, free, list, or quit>>");
	fflush (stdout);
	read (0, buf, 80);
	switch (buf[0]) {
	    case 'e':
		printf ("chunk or parent???>>");
		fflush (stdout);
		read (0, buf, 80);
		if (buf[0] == 'p') {
		    printf ("examin which parent>>");
		    fflush (stdout);
		    read (0, buf, 80);
		    par = (hpCnode *) atoi (buf);
		    printf ("c0 = %d is %d, c1 = %d is %d, c2 = %d is %d par = %d\n\n", par->c0, par->t0, par->c1, par->t1, par->c2, par->t2, par->parent);
		}
		else {
		    printf ("examin which chunk>>");
		    fflush (stdout);
		    read (0, buf, 80);
		    ch = (hpChunk *) atoi (buf);
		    printf ("next = %d, prev = %d\n\n", ch->next, ch->prev);
		}
		break;
	    case 'g':
		printf ("width>>");
		fflush (stdout);
		read (0, buf, 80);
		w = atoi (buf);
		printf ("height>>");
		fflush (stdout);
		read (0, buf, 80);
		h = atoi (buf);
                ch = hpBufAlloc (pScreen, w, h);
		if (ch == (hpChunk *) 0) {
		    printf ("cannot get block: %d by %d \n", w, h);
		}
		else {
		    printf ("block returned is block #%d\n", ch);
		    printf ("\tlocation of chunk is x=%d, y=%d\n", 
			ch->x, ch->y);
		    printf ("\tparent block is %d\n", ch->parent);
		}
		break;

	    case 'f':
		printf ("which block to free>>");
		fflush (stdout);
		read (0, buf, 80);
		ch = (hpChunk *) atoi (buf);
		hpBufFree (pScreen, ch);
		break;

	    case 'l':
		printf ("used = %d  unused = %d\n\n",
			allocInfo->used, allocInfo->unused);
		printf ("The list of FREE blocks are:\n");
		printf ("\tblock\theight\twidth\txloc\tyloc\tparent\n");
		if (allocInfo->unused != allocInfo->used) {
		    for (ch = allocInfo->unused; ch; ch = ch->next) {
			printf ("\t%d\t%d\t%d\t%d\t%d\t%d\n", ch,
			    ch->h, ch->w, ch->x, ch->y, ch->parent);
		    }
		}

		printf ("\nThe list of USED blocks are:\n");
		printf ("\tblock\theight\twidth\txloc\tyloc\tparent\n");
		for (ch = allocInfo->used; ch; ch = ch->next) {
		    printf ("\t%d\t%d\t%d\t%d\t%d\t%d\n", ch,
			    ch->h, ch->w, ch->x, ch->y, ch->parent);
		}
		printf ("\n");
		break;
	    case 'q':
		printf ("\n\n");
		exit ();
	    default:
		printf ("unknown command %c\n");
	}
    }

}
#endif

