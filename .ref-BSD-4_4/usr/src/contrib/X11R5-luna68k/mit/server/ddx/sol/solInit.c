/*
 *  solInit.c -- 
 *
 *	remade by A.Fujita, DEC-16-1992
 */

#include "sol.h"
#include "solFb.h"
#include "solKbd.h"

SolFbInfo	sol_fb_info;	

int	solScreenIndex;
static	unsigned long solGeneration;

static	Bool solScreenInit();
static	Bool solGetFbInfo();
static	void solSetConsoleMode();

static SolFbProc sol_fb_proc = {
	solBmCreate,  solBmInit,  solBmGiveUp
};

int
InitOutput(pScreenInfo, argc, argv)
ScreenInfo 	  *pScreenInfo;
int     	  argc;
char    	  **argv;
{
	static PixmapFormatRec  MonoFormats = {
		1, 1, BITMAP_SCANLINE_PAD,  /* 1-bit deep */
	};
	static Bool solFbInfo 	= FALSE;
	static Bool solDevsCreate = FALSE;

#ifdef	DEBUG
	fprintf(stderr, "InitOutput\t[solInit.c]\tStart\n");
#endif

	if (!solFbInfo) {
		if (solGetFbInfo(&sol_fb_info) != TRUE) 
			FatalError("Can't get framebuffer information.\n");
		solFbInfo = TRUE;
	}

	pScreenInfo->imageByteOrder     = IMAGE_BYTE_ORDER;
	pScreenInfo->bitmapScanlineUnit = BITMAP_SCANLINE_UNIT;
	pScreenInfo->bitmapScanlinePad  = BITMAP_SCANLINE_PAD;
	pScreenInfo->bitmapBitOrder     = BITMAP_BIT_ORDER;

	pScreenInfo->numPixmapFormats = 1;
	pScreenInfo->formats[0]       = MonoFormats;

	if (!solDevsCreate) {
		if (!(* sol_fb_info.func->CreateProc)(&sol_fb_info)) {
			FatalError("Can't create framebuffer.\n");
		}
		solSetConsoleMode();
		solDevsCreate = TRUE;
	}

	if(AddScreen(solScreenInit, argc, argv) < 0) {
		FatalError("Can't add screen\n");
	}

#ifdef	DEBUG
	fprintf(stderr, "InitOutput\t[solInit.c]\tEnd\n");
#endif
}

static Bool
solScreenInit(screenIndex, pScreen, argc, argv)
int		screenIndex;
ScreenPtr	pScreen;
int		argc;
char 		**argv;
{
#ifdef	DEBUG
	fprintf(stderr, "solScreenInit\t[solInit.c]\tStart\n");
#endif

	if (solGeneration != serverGeneration) {
		if((solScreenIndex = AllocateScreenPrivateIndex()) <0) {
			ErrorF("AllocateScreenPrivateIndex error.\n");
			return FALSE;
		}
		solGeneration = serverGeneration;
	}

	pScreen->devPrivates[solScreenIndex].ptr = (pointer) &sol_fb_info;

#ifdef	DEBUG
	fprintf(stderr, "solScreenInit\t[solInit.c]\tEnd\n");
#endif

	return((* sol_fb_info.func->InitProc)(screenIndex, pScreen, argc, argv));
}

int
InitInput(argc, argv)
    int    argc;
    char   **argv;
{
	DevicePtr p, k;

#ifdef	DEBUG
	fprintf(stderr, "InitInput\t[solInit.c]\tStart\n");
#endif

	p = AddInputDevice(solMouseProc, TRUE);
	k = AddInputDevice(solKbdProc, TRUE);
	
	RegisterPointerDevice(p);
	RegisterKeyboardDevice(k);
	
	miRegisterPointerDevice(screenInfo.screens[0], p);
	
	if (mieqInit (k, p) != TRUE) {
		FatalError("Enqueue init error.\n");
	}

	solSetIoHandler(solEnqueueEvents);

	if(RegisterBlockAndWakeupHandlers(NoopDDA, solWakeupProc, (pointer)0) != TRUE) {
		FatalError("Can't register WakeupHandler\n");
	}

#ifdef	DEBUG
	fprintf(stderr, "InitInput\t[solInit.c]\tEnd\n");
#endif
}

static Bool
solGetFbInfo(sol_fb_info)
SolFbInfoPtr sol_fb_info;
{
#ifdef	DEBUG
	fprintf(stderr, "solGetFbInfo\t[solInit.c]\tStart\n");
#endif

	sol_fb_info->fb_type	= FB_BM;
	sol_fb_info->func	= &sol_fb_proc;
	sol_fb_info->scr_width	= SCREEN_WIDTH;  
	sol_fb_info->scr_height	= SCREEN_HEIGHT;  
	sol_fb_info->fb_width	= FB_WIDTH;  
	sol_fb_info->fb_height	= FB_HEIGHT;  
	sol_fb_info->fb_depth	= 1;

#ifdef	DEBUG
	fprintf(stderr, "solGetFbInfo\t[solInit.c]\tEnd\n");
#endif

	return(TRUE);
}

static void
solSetConsoleMode()
{
#ifdef	DEBUG
	fprintf(stderr, "solSetConsoleMode\t[solInit.c]\tStart\n");
	fprintf(stderr, "solSetConsoleMode\t[solInit.c]\tEnd\n");
#endif
}

static void
solResetConsoleMode()
{
#ifdef	DEBUG
	fprintf(stderr, "solResetConsoleMode\t[solInit.c]\tStart\n");
	fprintf(stderr, "solResetConsoleMode\t[solInit.c]\tEnd\n");
#endif
}

void
AbortDDX()
{
#ifdef	DEBUG
	fprintf(stderr, "AbortDDX\t[solInit.c]\tStart\n");
#endif

	solResetConsoleMode();

#ifdef	DEBUG
	fprintf(stderr, "AbortDDX\t[solInit.c]\tEnd\n");
#endif
}

void
ddxGiveUp()
{
#ifdef	DEBUG
	fprintf(stderr, "ddxGiveUp\t[solInit.c]\tStart\n");
#endif

	solKbdGiveUp();
	solMouseGiveUp();
	if (sol_fb_info.func->GiveUpProc != NULL)
		(*sol_fb_info.func->GiveUpProc)(&sol_fb_info);
	solResetConsoleMode();

#ifdef	DEBUG
	fprintf(stderr, "ddxGiveUp\t[solInit.c]\tEnd\n");
#endif
}
