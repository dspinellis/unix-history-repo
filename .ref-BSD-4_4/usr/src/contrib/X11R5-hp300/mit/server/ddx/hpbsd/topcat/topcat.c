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
/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
#include <stdio.h>
#include <sys/types.h>

#include <fcntl.h>
#include <sys/ioctl.h>
#include <grfioctl.h>

#include "X.h"
#define  NEED_EVENTS
#include "Xproto.h"
#include "scrnintstr.h"
#include "cursorstr.h"
#include "pixmapstr.h"
#include "inputstr.h"
#include "regionstr.h"

#include "hppriv.h"
#include "topcat.h"
#include "windowstr.h"
#include "resource.h"
#include "mibstore.h"

extern int TopcatBrainDamage;
extern int catseyeMono;
extern int lastEventTime;

extern void hpChangeScreens();
extern Bool hpInitCursor(), cfbCreateDefColormap();

extern RegionPtr hpcCopyArea();
extern void mfbRegisterCopyAreaProc();

extern hpPrivScreenPtr hp_screens[];

static char _topcat_ident[] = "@(#)98544A, 98545A, 98547A";

static Bool
topcatSaveScreen(pScreen, on)
    ScreenPtr pScreen;
    Bool on;
{
    TOPCAT *gp_hardware = getTcHardware(pScreen);

    if (on != SCREEN_SAVER_ON)
    {
	lastEventTime = GetTimeInMillis();
	/* Turn on Video */
	gp_hardware -> nblank = TC_VIDEO_ON;
	getPrivScreenPtr(pScreen)->screenBlanked = FALSE;
    }
    else
    {
	/* Turn off video */
	gp_hardware -> nblank = TC_VIDEO_OFF;
	getPrivScreenPtr(pScreen)->screenBlanked = TRUE;
    }
    
    return TRUE;
}

static void
topcatUpdateColormap(pScreen, index, count, rmap, gmap, bmap)
     ScreenPtr pScreen;
     register  int index, count;
     register u_char *rmap, *gmap, *bmap;
{
    register TOPCAT *gp_hardware = getTcHardware(pScreen);
    hpPrivScreenPtr pPrivScreen = getPrivScreenPtr(pScreen);
    register i;
    Bool toggleScreenSaver = pPrivScreen->screenBlanked;

    if (pPrivScreen->planesMask == 0x01)
	return;

    if (toggleScreenSaver)
	/* We hang in status check unless video enabled */
	(void) topcatSaveScreen(pScreen, SCREEN_SAVER_OFF);

    while (count--)
    {
	unsigned short id;

	/* Wait for color map not busy */
	while (gp_hardware -> colormap_status & 0x04)
	    for (i=0; i<20; i++);
	
	gp_hardware->plane_mask = 0xff;

	gp_hardware->red_data = rmap[index];
	id = gp_hardware->id_reset;
	gp_hardware->green_data = gmap[index];;
	id = gp_hardware->id_reset;
	gp_hardware->blue_data = bmap[index];;
	id = gp_hardware->id_reset;
	gp_hardware->color_index = ~index;
	id = gp_hardware->id_reset;
	gp_hardware->colormap_writestrobe = 0xff;
	id = gp_hardware->id_reset;

	/*
	 * Not sure just why we have to do this, but it sure doesn't work
	 * without it. Delay loop slowed and delay after last status added.
	 * Seemed to be needed for faster 350 processor. HACK!
	 */

	while (gp_hardware -> colormap_status & 0x04)
	{
	    id = gp_hardware->id_reset;
	    for (i=0; i<100; i++);
	}
	
	for (i=0; i<1000; i++);

	gp_hardware -> red_data = 0;
	id = gp_hardware->id_reset;
	gp_hardware -> green_data = 0;
	id = gp_hardware->id_reset;
	gp_hardware -> blue_data = 0;
	id = gp_hardware->id_reset;
	gp_hardware -> color_index = 0;
	id = gp_hardware->id_reset;

	index++;
    }

    if (toggleScreenSaver)
	(void) topcatSaveScreen(pScreen, SCREEN_SAVER_ON);
}

static void
catseyeUpdateColormap(pScreen, index, count, rmap, gmap, bmap)
     ScreenPtr pScreen;
     int	index;
     register int count;
     u_char	*rmap, *gmap, *bmap;
{
    register TOPCAT *gp_hardware = getTcHardware(pScreen);
    hpPrivScreenPtr pPrivScreen = getPrivScreenPtr(pScreen);
    Bool toggleScreenSaver = pPrivScreen->screenBlanked;
    int tmp;

    if (pPrivScreen->planesMask == 0x01)
	return;

    /* We hang in status check unless video enabled */
    if (toggleScreenSaver)
	(void) topcatSaveScreen(pScreen, SCREEN_SAVER_OFF);

    {
	register int i;
	register u_char *rp, *gp, *bp;

	i = index;
	rp = &rmap[i];
	gp = &gmap[i];
	bp = &bmap[i];
	for (; count--; i++)
	{
	    /* Wait for color map not busy */
	    do {
		tmp = 0x04;
	    } while (gp_hardware->colormap_status & tmp);
	    gp_hardware->plane_mask = 0xff;
	    gp_hardware->color_index = ~i;
	    gp_hardware->red_data = *rp++;
	    gp_hardware->green_data = *gp++;
	    gp_hardware->blue_data = *bp++;
	    gp_hardware->colormap_writestrobe = 0xff;
	}
    }
    do {
	tmp = 0x04;
    } while (gp_hardware->colormap_status & tmp);
    gp_hardware->color_index = 0;
    if (toggleScreenSaver)
	(void) topcatSaveScreen(pScreen, SCREEN_SAVER_ON);
}

Bool
topcatScreenInfo(index, argv, argc)
    int index;
    char **argv;
    int argc;
{
    hpPrivScreenPtr thisScreen;
    topcatPrivPtr topcat;
    int fd, gcid;

    thisScreen = (hpPrivScreenPtr) 
	xalloc(sizeof(hpPrivScreen) + sizeof(topcatPriv));
    if (!thisScreen)
	return FALSE;
    topcat = (topcatPrivPtr) (thisScreen + 1);

    hp_screens[index] = thisScreen;
    thisScreen->pHardwareScreen = (pointer)topcat;

    topcat->topcatDev = (TOPCAT *)NULL;
    topcat->InstalledMap = (ColormapPtr)NULL;

    if ((fd = open(argv[2], O_WRONLY)) <  0)
    {
        perror(argv[0]);
        ErrorF("%s: couldn't open %s \n", argv[0], argv[2]);
        return FALSE;
    }
    {
	struct grfinfo gi;

	if (ioctl(fd, GRFIOCGINFO, &gi) < 0 || ioctl(fd, GRFIOCON, 0) < 0)
	{
	    ErrorF("%s: couldn't GCON and GCID %s \n", argv[0], argv[2]);
	    return FALSE; 
	}
	thisScreen->fbOffset = gi.gd_regsize;
	gcid = gi.gd_id;
    }
    
    if (gcid != GCID_TOPCAT) /* Not a Topcat */
    {
	ErrorF("%s: device %s not this kind of display.\n", argv[0],
	       argv[2]);
	close(fd);
	return FALSE;
    }

    thisScreen->fd   = fd;
    thisScreen->gcid = gcid;

    /*
     * Map the topcat in to our address space.  We could ask the O.S.
     * to map it in where it prefers, but this would limit the amount of
     * data we can malloc() at a later time.
     * However, we don't know how much address space it will take up until
     * we can examine its memWide and memHigh values....
     */

    {
	u_char *Addr = (u_char *) 0;
	char *str_addr = (char *)getenv("XDISPADDR");

	if (str_addr)
	    Addr = (u_char *) atoi(str_addr);

	if (ioctl (fd, GRFIOCMAP, &Addr) < 0)
	{
	    (void) ioctl (fd, GRFIOCOFF, 0);
	    perror("GRFIOCMAP:");
	    ErrorF("%s: Error getting address of %s\n", argv[0], argv[1]);
	    close(fd);
	    return FALSE;
	}
	topcat->topcatDev = (TOPCAT *) Addr;
    }
    {
	TOPCAT *tc;

	tc = topcat->topcatDev;
	thisScreen->memHeight = (tc->t_memhigh << 8) | tc->b_memhigh;
	thisScreen->memWidth  = (tc->t_memwide << 8) | tc->b_memwide;

	if (tc->id_second == ID2_TC)
	    topcat->UpdateColormap = topcatUpdateColormap;
	else
	    topcat->UpdateColormap = catseyeUpdateColormap;

	/* Set up the appropriate wholeGlyph routine */
#define ceWholeGlyph tcWholeGlyph

	if (tc->id_second < ID2_LCC)
	{
#if 0
	    if (tc->bits)
		thisScreen->WholeGlyph = mrtcWholeGlyph;
	    else
#endif 
		thisScreen->WholeGlyph = tcWholeGlyph;
	}
	else
	    thisScreen->WholeGlyph = ceWholeGlyph;
    }

    /* store the screen minor number in the devPrivate structure;
     * if there are four arguments, the fourth is the screen minor number;
     */
    if (argc == 4)
	thisScreen->minor_num = atoi(argv[3]);
    else
	thisScreen->minor_num = 0;

    thisScreen->screenBlanked = FALSE;

    return TRUE;
}

/*
 * Configure the topcat hardware masks;
 */

void
topcatMaskConfig(pScreen, writeEnableMask, replacementRule)
    register ScreenPtr pScreen;
    register int writeEnableMask, replacementRule;
{
    register TOPCAT *gp_hardware = getTcHardware(pScreen);
    register unsigned int planes = getPlanesMask(pScreen);

    waitbusy(planes, gp_hardware); /* wait for all planes to quiet */
    writeEnableMask &= planes;
    gp_hardware -> write_enable = writeEnableMask;
    gp_hardware -> frame_buf_write_enable = writeEnableMask;
    gp_hardware -> pixel_write_replacement_rule = replacementRule;
}

/*
 * Wait for the topcat mover hardware to be not-busy and then initiate
 * the requested move operation.
 */
void
topcatMoveBits(pScreen, planeMask, alu,
	    sourceX, sourceY, destX, destY, width, height)
    ScreenPtr pScreen;
    u_char    planeMask;
    int       alu, sourceX, sourceY, destX, destY, width, height;
{
    u_char pMask		 = getPlanesMask(pScreen);
    register TOPCAT *gp_hardware = getTcHardware(pScreen);
    int       k;
    
    /* wait for hardware ready. test all possible planes */
    while ((gp_hardware -> move_active) & pMask)
	for (k=0; k<100; k++); /* delay to let topcat mover work */
    
    /* return without doing any other work if no planes are effected...
     Note: we have assured that any previous mover operation has completed.*/
    if ((planeMask &= pMask) == 0) return;

    /* XXX catseye */
    gp_hardware -> frame_buf_write_enable = planeMask;

    gp_hardware -> write_enable = planeMask;
    gp_hardware -> window_move_replacement_rule = alu;
    gp_hardware -> source_x = sourceX;
    gp_hardware -> source_y = sourceY;
    gp_hardware -> dest_x = destX;
    gp_hardware -> dest_y = destY;
    gp_hardware -> window_width = width;
    gp_hardware -> window_height = height;
    /* start move on all GC enabled planes supported by hardware */  
    gp_hardware -> start_move = planeMask;
}    

extern Bool tcCloseScreen();

static Bool
topcatCloseScreen(index, pScreen)
    int index;
    ScreenPtr pScreen;
{
    register topcatPrivPtr topcat = (topcatPrivPtr)
	getPrivScreenPtr(pScreen)->pHardwareScreen;
    register u_char pMask	 = getPlanesMask(pScreen);
    register TOPCAT *gp_hardware = topcat->topcatDev;
    static u_char colors[2] = {0x00, 0xff};

    getPrivScreenPtr(pScreen)->screenBlanked = FALSE;	
    (*topcat->UpdateColormap)(pScreen,0,2,colors,colors,colors);

    waitbusy (pMask, gp_hardware);
    gp_hardware -> nblank = TC_VIDEO_ON;
    gp_hardware -> write_enable = ~0;
    gp_hardware -> window_move_replacement_rule = GXclear;
    gp_hardware -> write_enable = ~0;
    gp_hardware -> pixel_write_replacement_rule = GXclear;
    gp_hardware -> source_x = 0;
    gp_hardware -> source_y = 0;
    gp_hardware -> dest_x = 0;
    gp_hardware -> dest_y = 0;
    gp_hardware -> window_width = pScreen->width;
    gp_hardware -> window_height = pScreen->height;
    gp_hardware -> start_move = ~0;

    return tcCloseScreen(index, pScreen);
}

static int
tcNumPlanes(base, pMask)
     VOLATILE u_char *base;
     u_char *pMask;
{
    u_char sample0, sample1, rwBits;
    int numPlanes;

    *base = 0;
    sample0 = *base;
    *base = 0xff;
    sample1 = *base;
    rwBits = sample0 ^ sample1;
    switch(rwBits)
    {
      case 0x00: numPlanes = 0; break;
      case 0x01: numPlanes = 1; break;
      case 0x03: numPlanes = 2; break;
      case 0x07: numPlanes = 3; break;
      case 0x0f: numPlanes = 4; break;
      case 0x1f: numPlanes = 5; break;
      case 0x3f: numPlanes = 6; break;
      case 0x7f: numPlanes = 7; break;
      case 0xff: numPlanes = 8; break;
      default: numPlanes = -1;
    }
    *pMask = rwBits;

    if (TopcatBrainDamage || (numPlanes < 4 && catseyeMono))
    {
	*pMask = 0x01;
	numPlanes = 1;
    }

    return numPlanes;
}

Bool
topcatScreenInit(index, pScreen, argc, argv)
    int index;
    ScreenPtr pScreen;
    int argc;		/* these two may NOT be changed */
    char **argv;
{
    int dpi;
    u_char numPlanes;
    topcatPrivPtr topcat;
    TOPCAT *gp_hardware;
    hpPrivScreenPtr pPrivScreen;

    /* always restore the devPrivate field here;
     * if it has not been allocated, this will null it out so code elsewhere
     * will be sure to allocate one;
     * If we've already allocated one, this will restore it;
     */
    pScreen->devPrivate = (pointer)hp_screens[index];
    pPrivScreen = getPrivScreenPtr(pScreen);
    topcat = (topcatPrivPtr)(pPrivScreen->pHardwareScreen);
    gp_hardware = topcat->topcatDev;

    pPrivScreen->MoveBits = topcatMoveBits;
    pPrivScreen->MaskConfig = topcatMaskConfig;
    pPrivScreen->ChangeScreen = hpChangeScreens;

    /* Video card initialized */
    
    gp_hardware -> write_enable = ~0;
    gp_hardware -> pixel_write_replacement_rule = GXcopy;
    gp_hardware -> frame_buf_write_enable = ~0;

    /* XXX catseye */
    if (gp_hardware->id_second != ID2_TC)
    {
        char *foo = (char *) gp_hardware;

	*((short *)(foo + 0x4510)) = 0x0;	/* VB      */
	*((short *)(foo + 0x4512)) = 0x0;	/* TCNTRL  */
	*((short *)(foo + 0x4514)) = 0x0;	/* ACNTRL  */
	*((short *)(foo + 0x4516)) = 0x0;	/* PNCNTRL */
	*((short *)(foo + 0x4206)) = 0x90;	/* RUG Command/Status */
	*((short *)(foo + 0x60A2)) = 0x0;	/* Overlay Mask       */
	*((short *)(foo + 0x60BC)) = 0x0;	/* Ram Select	      */

	dpi = 109; /* XXX probably bogus */
    }
    else
	dpi = 85;

    /*
     * Set up the color map
     */

    numPlanes = tcNumPlanes(((u_char *) gp_hardware) + pPrivScreen->fbOffset,
			    &pPrivScreen->planesMask);
    if (!tcScreenInit(pScreen,
		      ((u_char *) gp_hardware) + pPrivScreen->fbOffset,
		      (gp_hardware->t_dispwide << 8)+gp_hardware->b_dispwide,
		      (gp_hardware->t_disphigh << 8)+gp_hardware->b_disphigh,
		      dpi, dpi, numPlanes))
	return FALSE;

    pScreen->SaveScreen = topcatSaveScreen;
    pScreen->CloseScreen = topcatCloseScreen;

    mfbRegisterCopyPlaneProc (pScreen, tcCopyPlane);
    mfbRegisterCopyAreaProc (pScreen, hpcCopyArea);

    if (!hpInitCursor(pScreen, 8))
	return FALSE;

    if (!cfbCreateDefColormap(pScreen))
    {
	ErrorF("Can't alloc black & white pixels in topcatScreenInit\n");
	return FALSE;
    }

    (void) topcatSaveScreen (pScreen, SCREEN_SAVER_OFF);
    return TRUE;
}
