/*
 *  solBm.c -- 
 *
 *	remade by A.Fujita, DEC-16-1992
 */

#include "sol.h"
#include "solFb.h"


solBmCreate(sol_fb_info)
SolFbInfoPtr sol_fb_info;
{
        struct fb_rfc rfc;
#ifdef	DEBUG
	fprintf(stderr, "solBmCreate\t[solBm.c]\tStart\n");
#endif

	if ((sol_fb_info->fbfd = open("/dev/fb", O_RDWR)) < 0) {
		Error("Can't open /dev/fb");
		return FALSE;
	}

	sol_fb_info->fbmapsize = (FB_WIDTH/8)*FB_HEIGHT;
	sol_fb_info->fbmap = (char *) mmap((caddr_t) 0 ,(size_t) sol_fb_info->fbmapsize,
					    (PROT_READ | PROT_WRITE), MAP_SHARED,
					    sol_fb_info->fbfd, (off_t) 0);

	if (sol_fb_info->fbmap == (char *) -1) {
		Error("Can't mmap /dev/fb");
		return FALSE;
	}

	sol_fb_info->plane = sol_fb_info->fbmap + 8;

	rfc.rfc_hcnt =  -249;
	rfc.rfc_vcnt = -2075;
        (void) ioctl(sol_fb_info->fbfd, FBIOSETRFCT, &rfc);

#ifdef	DEBUG
	fprintf(stderr, "solBmCreate\t[solBm.c]\tEnd\n");
#endif

	return TRUE;
}

static Bool
solBmSaveScreen(pScreen, on)
ScreenPtr     pScreen;
Bool          on;
{
	return FALSE;
}

Bool
solBmInit(index, pScreen, argc, argv)
    int           index;
    ScreenPtr     pScreen;
    int           argc;
    char          **argv;
{
	SolFbInfoPtr pFbInfo;
	extern miPointerScreenFuncRec  solPointerScreenFuncs;

#ifdef	DEBUG
	fprintf(stderr, "solBmInit\t[solBm.c]\tStart\n");
#endif
	
	pFbInfo = (SolFbInfoPtr) pScreen->devPrivates[solScreenIndex].ptr;

	if (!monitorResolution) {
		monitorResolution = MONO_TV_RESOLUTION; 
	}

	if(!mfbScreenInit(pScreen, (pointer) pFbInfo->plane,
			  pFbInfo->scr_width, pFbInfo->scr_height,
			  monitorResolution, monitorResolution,
			  pFbInfo->fb_width)){
		ErrorF("mfbScreenInit error.\n");
		return FALSE;
	}

   	pScreen->whitePixel = 0;
   	pScreen->blackPixel = 1;

	miDCInitialize (pScreen, &solPointerScreenFuncs);

	mfbCreateDefColormap(pScreen);

	pScreen->SaveScreen = solBmSaveScreen;

	solBmSaveScreen(pScreen, SCREEN_SAVER_FORCER);

#ifdef	DEBUG
	fprintf(stderr, "solBmInit\t[solBm.c]\tEnd\n");
#endif
	return TRUE;
}

void
solBmGiveUp(sol_fb_info)
SolFbInfoPtr sol_fb_info;
{
#ifdef	DEBUG
	fprintf(stderr, "solBmGiveUp\t[solBm.c]\tStart\n");

	(void) close(sol_fb_info->fbfd);

	fprintf(stderr, "solBmGiveUp\t[solBm.c]\tEnd\n");
#endif
}
