/*
 *  solMouse.c -- 
 *
 *	remade by A.Fujita, DEC-17-1992
 */

#include "sol.h"

static void solCrossScreen();
static void solWarpCursor();
static Bool solCursorOffScreen();

miPointerScreenFuncRec solPointerScreenFuncs = {
	solCursorOffScreen,
	solCrossScreen,
	solWarpCursor,
};

int
solMouseProc(pMouse,what)
DevicePtr 	pMouse;
int		what;
{
#ifdef	DEBUG
	fprintf(stderr, "solMouseProc\t[solMouse.c]\tStart\n");
	fprintf(stderr, "solMouseProc\t[solMouse.c]\tEnd\n");
#endif
	return (Success);
}

void
solMouseGiveUp()
{
#ifdef	DEBUG
	fprintf(stderr, "solMouseGiveUp\t[solMouse.c]\tStart\n");
	fprintf(stderr, "solMouseGiveUp\t[solMouse.c]\tEnd\n");
#endif
}

static Bool
solCursorOffScreen(pScreen, x, y)
    ScreenPtr	*pScreen;
    int		*x, *y;
{
	return FALSE;
}

static void
solCrossScreen(pScreen,x,y)
ScreenPtr	pScreen;
int		x;
int		y;
{
}

static void
solWarpCursor (pScreen, x, y)
	ScreenPtr   pScreen;
	int         x, y;
{
	int oldmask;

	oldmask = sigblock (sigmask(SIGABRT));
	miPointerWarpCursor (pScreen, x, y);
	sigsetmask (oldmask);
}
