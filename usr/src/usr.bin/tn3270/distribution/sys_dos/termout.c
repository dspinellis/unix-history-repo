/*
 *	Copyright (c) 1984, 1985, 1986 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

#ifndef lint
static	char	sccsid[] = "@(#)outbound.c	3.1  10/29/86";
#endif	/* lint */


#include <stdio.h>
#include <dos.h>
#include "../general/general.h"

#include "../telnet.ext"

#include "../api/disp_asc.h"
#include "../ascii/map3270.ext"

#include "../ctlr/hostctlr.h"
#include "../ctlr/inbound.ext"
#include "../ctlr/oia.h"
#include "../ctlr/options.ext"
#include "../ctlr/outbound.ext"
#include "../ctlr/screen.h"

#include "../general/globals.h"

#include "video.h"

extern void EmptyTerminal();

#define CorrectTerminalCursor() ((TransparentClock == OutputClock)? \
		terminalCursorAddress:UnLocked? CursorAddress: HighestScreen())


static int terminalCursorAddress;	/* where the cursor is on term */
static int screenInitd; 		/* the screen has been initialized */
static int screenStopped;		/* the screen has been stopped */

static int needToRing;			/* need to ring terinal bell */

typedef struct {
    char
	data,		/* The data for this position */
	attr;		/* The attributes for this position */
} ScreenBuffer;

ScreenBuffer Screen[MAXNUMBERLINES*MAXNUMBERCOLUMNS];
ScreenBuffer saveScreen[sizeof Screen/sizeof Screen[0]];

/* OurExitString - designed to keep us from going through infinite recursion */

static void
OurExitString(file, string, value)
FILE	*file;
char	*string;
int	value;
{
    static int recursion = 0;

    if (!recursion) {
	recursion = 1;
	ExitString(file, string, value);
    }
}


static void
GoAway(from, where)
char *from;		/* routine that gave error */
int	where;		/* cursor address */
{
	char foo[100];

	sprintf(foo, "ERR from %s at %d (%d, %d)\n",
		from, where, ScreenLine(where), ScreenLineOffset(where));
	OurExitString(stderr, foo, 1);
	/* NOTREACHED */
}

/*
 * Routines to deal with the screen.  These routines are lifted
 * from mskermit.
 */

#define	CRT_STATUS	0x3da		/* Color card */
#define	DISPLAY_ENABLE	0x08		/* Enable */
#define	scrseg()	((crt_mode == 7)? 0xb000 : 0xb800)
#define	scrwait()	if (crt_mode != 7) { \
			    while ((inp(CRT_STATUS)&DISPLAY_ENABLE) == 0) { \
				; \
			    } \
			}
static int
    		crt_mode,
		crt_cols,
		crt_lins,
		curpage;

/*
 * Set the cursor position to where it belongs.
 */

static void
setcursor(row, column, page)
int
    row,
    column,
    page;
{
    union REGS inregs, outregs;

    inregs.h.dh = row;
    inregs.h.dl = column;
    inregs.h.bh = page;
    inregs.h.ah = SetCursorPosition;

    int86(BIOS_VIDEO, &inregs, &outregs);
}
/*
 * Read the state of the video system.  Put the cursor somewhere
 * reasonable.
 */

static void
scrini()
{
    union REGS inregs, outregs;

    inregs.h.ah = CurrentVideoState;
    int86(BIOS_VIDEO, &inregs, &outregs);

    crt_mode = outregs.h.al;
    crt_cols = outregs.h.ah;
    crt_lins = 25;
    curpage = outregs.h.bh;

    inregs.h.ah = ReadCursorPosition;
    inregs.h.bh = curpage;

    int86(BIOS_VIDEO, &inregs, &outregs);

    if (outregs.h.dh > crt_lins) {
	outregs.h.dh = crt_lins;
    }
    if (outregs.h.dl > crt_cols) {
	outregs.h.dl = crt_cols;
    }
    inregs.h.dh = outregs.h.dh;
    inregs.h.dl = outregs.h.dl;
    inregs.h.bh = curpage;

    inregs.h.ah = SetCursorPosition;
    int86(BIOS_VIDEO, &inregs, &outregs);
}


static void
scrwrite(source, length, offset)
ScreenBuffer *source;
int
	length,
	offset;
{
    struct SREGS segregs;

    segread(&segregs);		/* read the current segment register */

    scrwait();
    movedata(segregs.ds, source, scrseg(), sizeof *source*offset,
						sizeof *source*length);
}

static void
scrsave(buffer)
ScreenBuffer *buffer;
{
    struct SREGS segregs;

    segread(&segregs);		/* read the current segment register */

    scrwait();
    movedata(scrseg(), 0, segregs.ds, buffer, crt_cols*crt_lins*2);
}

static void
scrrest(buffer)
ScreenBuffer *buffer;
{
    scrwrite(buffer, crt_cols*crt_lins, 0);
}

static void
TryToSend()
{
#define	STANDOUT	0x0a	/* Highlighted mode */
#define	NORMAL		0x02	/* Normal mode */
#define	NONDISPLAY	0x00	/* Don't display */

#define	DoAttribute(a) 	    \
			    if (screenIsFormatted) { \
				if (IsNonDisplayAttr(a)) { \
				    a = NONDISPLAY; 	/* don't display */ \
				} else if (IsHighlightedAttr(a)) { \
				    a = STANDOUT; \
				} else { \
				    a = NORMAL; \
				} \
			    } else  { \
				a = NORMAL;	/* do display on unformatted */\
			    }
    ScreenImage *p, *upper;
    ScreenBuffer *sp;
    int fieldattr;		/* spends most of its time == 0 or 1 */
    int screenIsFormatted = FormattedScreen();

/* OK.  We want to do this a quickly as possible.  So, we assume we
 * only need to go from Lowest to Highest.  However, if we find a
 * field in the middle, we do the whole screen.
 *
 * In particular, we separate out the two cases from the beginning.
 */
    if ((Highest != HighestScreen()) || (Lowest != LowestScreen())) {
	sp = &Screen[Lowest];
	p = &Host[Lowest];
	upper = &Host[Highest];
	fieldattr = FieldAttributes(Lowest);
	DoAttribute(fieldattr);	/* Set standout, non-display status */

	while (p <= upper) {
	    if (IsStartFieldPointer(p)) {	/* New field? */
		Highest = HighestScreen();
		Lowest = LowestScreen();
		TryToSend();		/* Recurse */
		return;
	    } else if (fieldattr) {	/* Should we display? */
				/* Display translated data */
		sp->data = disp_asc[GetHostPointer(p)];
	    } else {
		sp->data = ' ';
	    }
	    sp->attr = fieldattr;
	    p++;
	    sp++;
	}
    } else {		/* Going from Lowest to Highest */
	ScreenImage *End = &Host[ScreenSize]-1;

	sp = Screen;
	p = Host;
	fieldattr = FieldAttributes(LowestScreen());
	DoAttribute(fieldattr);	/* Set standout, non-display status */

	while (p <= End) {
	    if (IsStartFieldPointer(p)) {	/* New field? */
		fieldattr = FieldAttributesPointer(p);	/* Get attributes */
		DoAttribute(fieldattr);	/* Set standout, non-display */
	    }
	    if (fieldattr) {	/* Should we display? */
			    /* Display translated data */
		sp->data = disp_asc[GetHostPointer(p)];
	    } else {
		sp->data = ' ';
	    }
	    sp->attr = fieldattr;
	    p++;
	    sp++;
	}
    }
    terminalCursorAddress = CorrectTerminalCursor();
    /*
     * We might be here just to update the cursor address.
     */
    if (Highest >= Lowest) {
	scrwrite(Screen+Lowest, (1+Highest-Lowest), Lowest);
    }
    setcursor(ScreenLine(terminalCursorAddress),
		    ScreenLineOffset(terminalCursorAddress), 0);
    Lowest = HighestScreen()+1;
    Highest = LowestScreen()-1;
    if (needToRing) {
	DataToTerminal("\7", 1);
	needToRing = 0;
    }
    return;
}

/* InitTerminal - called to initialize the screen, etc. */

void
InitTerminal()
{
    InitMapping();		/* Go do mapping file (MAP3270) first */
    if (!screenInitd) { 	/* not initialized */
	MaxNumberLines = 24;	/* XXX */
	MaxNumberColumns = 80;	/* XXX */
	scrini();
	scrsave(saveScreen);	/* Save the screen buffer away */
	ClearArray(Screen);
	terminalCursorAddress = SetBufferAddress(0,0);
	screenInitd = 1;
	screenStopped = 0;		/* Not stopped */
    }
}


/* StopScreen - called when we are going away... */

void
StopScreen(doNewLine)
int doNewLine;
{
    if (screenInitd && !screenStopped) {
	scrrest(saveScreen);
	setcursor(NumberLines-1, 1, 0);
	if (doNewLine) {
	    StringToTerminal("\r\n");
	}
	EmptyTerminal();
	screenStopped = 1;
    }
}


/* RefreshScreen - called to cause the screen to be refreshed */

void
RefreshScreen()
{
    Highest = HighestScreen();
    Lowest = LowestScreen();
    TryToSend();
}


/* ConnectScreen - called to reconnect to the screen */

void
ConnectScreen()
{
    if (screenInitd) {
	RefreshScreen();
	screenStopped = 0;
    }
}

/* LocalClearScreen() - clear the whole ball of wax, cheaply */

void
LocalClearScreen()
{
    Clear3270();
    Lowest = LowestScreen(); /* everything in sync... */
    Highest = HighestScreen();
    TryToSend();
}

/*
 * Implement the bell/error message function.
 */

int
	bellwinup = 0;		/* If != 0, length of bell message */
static int
	bell_len = 0;		/* Length of error message */


void
BellOff()
{
    ScreenBuffer a[100];
    int i;

    if (bellwinup) {
	unsigned char blank = ' ';

	for (i = 0; i < bell_len; i++) {
	    a[i].attr = NORMAL;
	    a[i].data = ' ';
	}
    }
    scrwrite(a, bell_len, 24*80);		/* XXX */
}


void
RingBell(s)
char *s;
{
    needToRing = 1;
    if (s) {
	int i;
	ScreenBuffer bellstring[100];

	bell_len = strlen(s);
	bellwinup = 1;
	if (bell_len > sizeof bellstring-1) {
	    OurExitString(stderr, "Bell string too long.", 1);
	}
	for (i = 0; i < bell_len; i++) {
	    bellstring[i].attr = STANDOUT;
	    bellstring[i].data = s[i];
	}
	scrwrite(bellstring, bell_len, 24*80);		/* XXX */
    }
}

/*
 * Update the OIA area.
 */

void
ScreenOIA(oia)
OIA *oia;
{
}


/* returns a 1 if no more output available (so, go ahead and block),
    or a 0 if there is more output available (so, just poll the other
    sources/destinations, don't block).
 */

int
DoTerminalOutput()
{
	/* called just before a select to conserve IO to terminal */
    if (!(screenInitd||screenStopped)) {
	return 1;		/* No output if not initialized */
    }
    if ((Lowest <= Highest) || needToRing ||
			(terminalCursorAddress != CorrectTerminalCursor())) {
	TryToSend();
    }
    if (Lowest > Highest) {
	return 1;		/* no more output now */
    } else {
	return 0;		/* more output for future */
    }
}

/*
 * The following are defined to handle transparent data.
 */

void
TransStop()
{
    RefreshScreen();
}

void
TransOut(buffer, count, kind, control)
unsigned char	*buffer;
int		count;
int		kind;		/* 0 or 5 */
int		control;	/* To see if we are done */
{
    char *ptr;

    while (DoTerminalOutput() == 0) {
	;
    }
    for (ptr = buffer; ptr < buffer+count; ptr++) {
	*ptr &= 0x7f;		/* Turn off parity bit */
    }
    (void) DataToTerminal(buffer, count);
    if (control && (kind == 0)) {		/* Send in AID byte */
	SendToIBM();
    } else {
	TransInput(1, kind);			/* Go get some data */
    }
}

/*
 * init_screen()
 *
 * Initialize variables used by screen.
 */

void
init_screen()
{
    bellwinup = 0;
}


