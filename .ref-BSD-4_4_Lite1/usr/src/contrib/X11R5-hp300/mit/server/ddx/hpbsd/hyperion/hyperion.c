/*
 */
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
#include "hyperion.h"

extern int lastEventTime;

extern void hpChangeScreens();
extern Bool hpInitCursor(), mfbCreateDefColormap();

extern hpPrivScreenPtr hp_screens[];

static Bool
hyperSaveScreen(pScreen, on)
    ScreenPtr pScreen;
    Bool on;
{
    HYPER *gp_hardware = getHyHardware(pScreen);

    if (on != SCREEN_SAVER_ON)
    {
	lastEventTime = GetTimeInMillis();
	/* Turn on Video */
	gp_hardware -> nblank = HY_VIDEO_ON;
	getPrivScreenPtr(pScreen)->screenBlanked = FALSE;
    }
    else
    {
	/* Turn off video */
	gp_hardware -> nblank = HY_VIDEO_OFF;
	getPrivScreenPtr(pScreen)->screenBlanked = TRUE;
    }
    
    return TRUE;
}

Bool
hyperScreenInfo(index, argv, argc)
    int index;
    char **argv;
    int argc;
{
    hpPrivScreenPtr thisScreen;
    int fd, gcid;

    thisScreen = (hpPrivScreenPtr) xalloc(sizeof(hpPrivScreen));
    if (!thisScreen)
	return FALSE;

    hp_screens[index] = thisScreen;

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
    
    if (gcid != GCID_HYPERION) /* Not a Hyperion */
    {
	ErrorF("%s: device %s not this kind of display.\n", argv[0],
	       argv[2]);
	close(fd);
	return FALSE;
    }

    thisScreen->fd   = fd;
    thisScreen->gcid = gcid;

    /*
     * Map the hyperion in to our address space.  We could ask the O.S.
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
	thisScreen->pHardwareScreen = (pointer) Addr;
    }
    {
	HYPER *hy = (HYPER *)thisScreen->pHardwareScreen;

	thisScreen->memHeight = (hy->t_memhigh << 8) | hy->b_memhigh;
	thisScreen->memWidth  = (hy->t_memwide << 8) | hy->b_memwide;
    }

#if 0
    thisScreen->WholeGlyph = hyWholeGlyph;
#endif

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

void
hyperMaskConfig(pScreen, writeEnableMask, replacementRule)
     register ScreenPtr pScreen;
     register int writeEnableMask, replacementRule;
{
    /* no-op */;
}

extern Bool hpmfbCloseScreen();

static Bool
hyperCloseScreen(index, pScreen)
    int index;
    ScreenPtr pScreen;
{
    register HYPER *gp_hardware = getHyHardware(pScreen);

    getPrivScreenPtr(pScreen)->screenBlanked = FALSE;	

    gp_hardware -> nblank = HY_VIDEO_ON;

    /* this should probably be done with a wrapper */
    return hpmfbCloseScreen(index, pScreen);
}


Bool
hyperScreenInit(index, pScreen, argc, argv)
    int index;
    ScreenPtr pScreen;
    int argc;		/* these two may NOT be changed */
    char **argv;
{
    int dpi;
    HYPER *gp_hardware;
    hpPrivScreenPtr pPrivScreen;

    /* always restore the devPrivate field here;
     * if it has not been allocated, this will null it out so code elsewhere
     * will be sure to allocate one;
     * If we've already allocated one, this will restore it;
     */
    pScreen->devPrivate = (pointer)hp_screens[index];
    pPrivScreen = getPrivScreenPtr(pScreen);
    gp_hardware = (HYPER *)(pPrivScreen->pHardwareScreen);

    pPrivScreen->MoveBits = hyperMoveBits;
    pPrivScreen->MaskConfig = hyperMaskConfig;
    pPrivScreen->ChangeScreen = hpChangeScreens;

    dpi = 109; /* XXX probably bogus */

    if (!hpmfbScreenInit(pScreen,
			 ((u_char *) gp_hardware) + pPrivScreen->fbOffset,
			 (gp_hardware->t_dispwide<<8)+gp_hardware->b_dispwide,
			 (gp_hardware->t_disphigh<<8)+gp_hardware->b_disphigh,
			 dpi, dpi))
	return FALSE;

    pScreen->SaveScreen = hyperSaveScreen;
    pScreen->CloseScreen = hyperCloseScreen;

    if (!hpInitCursor(pScreen, 1))
	return FALSE;

    pScreen->whitePixel = 1;
    pScreen->blackPixel = 0;
    if (!mfbCreateDefColormap(pScreen))
    {
	ErrorF("Can't alloc black & white pixels in hyperScreenInit\n");
	return FALSE;
    }

    (void) hyperSaveScreen(pScreen, SCREEN_SAVER_OFF);

    return TRUE;
}
