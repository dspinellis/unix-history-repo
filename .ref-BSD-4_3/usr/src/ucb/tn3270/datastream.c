/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
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


/* this exists to patch over DataFromNetwork until it is ready */

#include <signal.h>
#include <sgtty.h>
#include <stdio.h>
#include <curses.h>

#include "ascebc.h"
#include "3270.h"
#include "screen.h"

#ifndef	lint
static char sccsid[] = "@(#)datastream.c	2.12\t12/16/85";
#endif	/* lint */

void EmptyTerminal();

#define CorrectTerminalCursor()	((TransparentClock == OutputClock)? terminalCursorAddress:UnLocked? CursorAddress: HighestScreen())

#define SetHighestLowest(position) { \
					if (position < Lowest) { \
					    Lowest = position; \
					} \
					if (position > Highest) { \
					    Highest = position; \
					} \
				    }

extern char	ebcasc[NEBCASC][NEBC];	/* translate table */

static int terminalCursorAddress;	/* where the cursor is on term */
static int screenInitd;			/* the screen has been initialized */
static int MAX_CHANGES_BEFORE_POLL;	/* how many characters before looking */
					/* at terminal and net again */

static int needToRing = 0;		/* need to ring terinal bell */
static char *bellSequence = "\07";	/* bell sequence (may be replaced by
					 * VB during initialization)
					 */
static char *KS, *KE;			/* Turn on and off keyboard */

static char Blanks[sizeof Terminal];	/* lots of blanks */

/* some globals */

int	OutputClock;			/* what time it is */
int	TransparentClock;		/* time we were last in transparent */


/* StartScreen - called to initialize the screen, etc. */

StartScreen()
{
    int save;
    struct sgttyb ourttyb;
    static int speeds[] = { 0, 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800,
		2400, 4800, 9600 };
    static char KSEbuffer[2050];
    char *lotsofspace = KSEbuffer, *tgetstr();

    if (!screenInitd) {
	ioctl(1, TIOCGETP, (char *) &ourttyb);
	if ((ourttyb.sg_ospeed < 0) || (ourttyb.sg_ospeed > B9600)) {
	    MAX_CHANGES_BEFORE_POLL = 1920;
	} else {
	    MAX_CHANGES_BEFORE_POLL = speeds[ourttyb.sg_ospeed]/10;
	    if (MAX_CHANGES_BEFORE_POLL < 40) {
		MAX_CHANGES_BEFORE_POLL = 40;
	    }
	}
	save = mode(0);
	initscr();			/* start up curses */
	nonl();
			/* the problem is that curses catches SIGTSTP to
			 * be nice, but it messes us up.
			 */
	signal(SIGTSTP, SIG_DFL);
	KS = tgetstr("ks", &lotsofspace);
	KE = tgetstr("ke", &lotsofspace);
	if (KS) {
	    StringToTerminal(KS);
	}
	DoARefresh();
	(void) mode(save);
	if (VB && *VB) {
	    bellSequence = VB;		/* use visual bell */
	}
	screenInitd = 1;
    }
}



/* Stop3270 - called when we are going away... */

Stop3270(doNewLine)
int doNewLine;
{
    if (screenInitd) {
	int save;

	move(NUMBERLINES-1, 1);
	DoARefresh();
	if (KE) {
	    StringToTerminal(KE);
	}
	if (doNewLine) {
	    StringToTerminal("\r\n");
	}
	EmptyTerminal();
	save = mode(0);
	endwin();
	(void) mode(save);
    }
}


/* ConnectScreen - called to reconnect to the screen */

ConnectScreen() 
{
    if (screenInitd) {
	if (KS) {
	    StringToTerminal(KS);
	}
	RefreshScreen();
	TryToSend();
    }
}

/* RefreshScreen - called to cause the screen to be refreshed */

RefreshScreen()
{
    clearok(curscr, TRUE);
    TryToSend();
}


/* Clear3270 - called to clear the screen */

Clear3270()
{
    bzero((char *)Host, sizeof(Host));
    DeleteAllFields();		/* get rid of all fields */
    BufferAddress = SetBufferAddress(0,0);
    CursorAddress = SetBufferAddress(0,0);
    Lowest = LowestScreen();
    Highest = HighestScreen();
}

/* LocalClear3270() - clear the whole ball of wax, cheaply */

LocalClear3270()
{
    outputPurge();		/* flush all data to terminal */
    clear();			/* clear in curses */
    bzero((char *)Terminal, sizeof Terminal);
    Clear3270();		/* clear host part */
    Lowest = HighestScreen()+1;	/* everything in sync... */
    Highest = LowestScreen()+1;
}

/* OurExitString - designed to keep us from going through infinite recursion */

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


RingBell()
{
    needToRing = 1;
}

/* AddHost - called to add a character to the buffer.
 * 	We use a macro in this module, since we call it so
 *	often from loops.
 *
 *	NOTE: It is a macro, so don't go around using AddHost(p, *c++), or
 *	anything similar.  (I don't define any temporary variables, again
 *	just for the speed.)
 */
AddHost(position, character)
int	position;
char	character;
{
#   define	AddHostA(p,c)					\
    {								\
	if (IsStartField(p)) {					\
	    DeleteField(p);					\
	    SetHighestLowest(p);				\
	}							\
	SetHost(p, c);						\
    }
#   define	AddHost(p,c)					\
    {								\
	AddHostA(p,c);						\
	if ((c != GetTerminal(p)) || TermIsStartField(p)) {	\
	    SetHighestLowest(p);				\
	}							\
    }	/* end of macro of AddHost */

    AddHost(position, character);
}

/* DoARefresh */

static
DoARefresh()
{
    if (ERR == refresh()) {
	OurExitString(stderr, "ERR from refresh\n", 1);
    }
}

/* TryToSend - send data out to user's terminal */

static
TryToSend()
{
    register int pointer;
    register int c;
    register int fieldattr;
    register int changes;

    static int inHighlightMode = 0;
    extern int HaveInput;	/* 1 if there is input to check out */

#   define  SetHighlightMode(p) { \
		if (!IsStartField(p) && IsHighlightedAttr(fieldattr)) { \
		    if (!inHighlightMode) { \
			inHighlightMode = 1; \
			standout(); \
		    } \
		} else { \
		    if (inHighlightMode) { \
			inHighlightMode = 0; \
			standend(); \
		    } \
		} \
	    }

#   define  DoCharacterAt(c,p) { \
		SetTerminal(p, c); \
		if (p != HighestScreen()) { \
		    c = TerminalCharacterAttr(ebcasc[0][c&0xff], p, \
								fieldattr); \
		    if (terminalCursorAddress != p) { \
			if (ERR == mvaddch(ScreenLine(p), \
						ScreenLineOffset(p), c)) {\
			    char foo[100]; \
			    \
			    sprintf(foo, "ERR from mvaddch at %d (%d, %d)\n", \
				    p, ScreenLine(p), ScreenLineOffset(p)); \
			    OurExitString(stderr, foo, 1); \
			} \
		    } else { \
			if (ERR == addch(c)) {\
			    char foo[100]; \
			    \
			    sprintf(foo, "ERR from addch at %d (%d, %d)\n", \
				    p, ScreenLine(p), ScreenLineOffset(p)); \
			    OurExitString(stderr, foo, 1); \
			} \
		    } \
		    terminalCursorAddress = ScreenInc(p); \
		} \
		/* if (pointer%LINESIZE == LINESIZE-1) { \
		    DoARefresh(); \
		    if (TtyChars() > MAX_CHANGES_BEFORE_POLL) { \
			EmptyTerminal(); \
		    } \
		} */ \
	    }

    /* run through screen, printing out non-null lines */

    /* There are two separate reasons for wanting to terminate this
     * loop early.  One is to respond to new input (either from
     * the terminal or from the network [host]).  For this reason,
     * we expect to see 'HaveInput' come true when new input comes in. 
     *
     * The second reason is a bit more difficult (for me) to understand.
     * Basically, we don't want to get too far ahead of the characters that
     * appear on the screen.  Ideally, we would type out a few characters,
     * wait until they appeared on the screen, then type out a few more.
     * The reason for this is that the user, on seeing some characters
     * appear on the screen may then start to type something.  We would
     * like to look at what the user types at about the same 'time'
     * (measured by characters being sent to the terminal) that the
     * user types them.  For this reason, what we would like to do
     * is update a bit, then call curses to do a refresh, flush the
     * output to the terminal, then wait until the terminal data
     * has been sent.
     *
     * Note that curses is useful for, among other things, deciding whether
     * or not to send :ce: (clear to end of line), so we should call curses
     * at end of lines (beginning of next lines).
     *
     * The problems here are the following:  If we do lots of write(2)s,
     * we will be doing lots of context switches, thus lots of overhead
     * (which we have already).  Second, if we do a select to wait for
     * the output to drain, we have to contend with the fact that NOW
     * we are scheduled to run, but who knows what the scheduler will
     * decide when the output has caught up.
     */

    if (Highest == HighestScreen()) {
	Highest = ScreenDec(Highest);	/* else, while loop will never end */
    }
    if (Lowest < LowestScreen()) {
	Lowest = LowestScreen();	/* could be -1 in some cases with
					 * unformatted screens.
					 */
    }
    if (Highest >= Lowest) {
		/* if there is anything to do, do it.  We won't terminate
		 * the loop until we've gone at least to Highest.
		 */
	pointer = Lowest;
	while ((pointer <= Highest) && !HaveInput) {

		/* point at the next place of disagreement */
	    pointer += (bunequal(Host+pointer, Terminal+pointer,
			(Highest-pointer+1)*sizeof Host[0])/sizeof Host[0]);

		/* how many characters to change until the end of the
		 * current line
		 */
	    changes = LINESIZE - ScreenLineOffset(pointer);

		/* what is the field attribute of the current position */
	    fieldattr = FieldAttributes(WhereAttrByte(pointer));

	    if ((IsStartField(pointer) != TermIsStartField(pointer)) ||
		    (IsStartField(pointer) &&
			fieldattr != TermAttributes(pointer))) {

		int oldterm;

		oldterm = TermAttributes(pointer);
		if (IsStartField(pointer)) {
		    TermNewField(pointer, fieldattr);
		    SetTerminal(pointer, 0);
		} else {
		    TermDeleteField(pointer);
		}
		    /* We always do the first character in a divergent
		     * field, since otherwise the start of a field in
		     * the Host structure may leave a highlighted blank
		     * on the screen, and the start of a field in the
		     * Terminal structure may leave a non-highlighted
		     * something in the middle of a highlighted field
		     * on the screen.
		     */
		SetHighlightMode(pointer);
		c = GetHost(pointer);
		DoCharacterAt(c,pointer);

		if (NotVisuallyCompatibleAttributes
				(pointer, fieldattr, oldterm)) {
		    int j;

		    j = pointer;

		    pointer = ScreenInc(pointer);
		    SetHighlightMode(pointer);	/* Turn on highlighting */
		    while (!IsStartField(pointer) &&
				!TermIsStartField(pointer)) {
			c = GetHost(pointer);
			DoCharacterAt(c,pointer);	/* MACRO */
			pointer = ScreenInc(pointer);
			if (!(--changes)) {
			    DoARefresh();
			    EmptyTerminal();
				/* We don't look at HaveInput here, since
				 * if we leave this loop before the end of
				 * the 3270 field, we could have pointer
				 * higher than Highest.  This would cause
				 * us to end the highest "while" loop,
				 * but we may, in fact, need to go around the
				 * screen once again.
				 */
			}
			/*		The loop needs to be protected
			 *	from the situation where there had been only
			 *	one field on the Terminal, and none on the Host.
			 *	In this case, we have just deleted our last
			 *	field.  Hence, the break.
			 */
			if (j == pointer) {
			    break;
			}
		    }
		    if (!TermIsStartField(pointer)) {
			    /* Remember what the terminal looked like */
			TermNewField(pointer, oldterm);
			SetTerminal(pointer, 0);
			/* The danger here is that the current position may
			 * be the start of a Host field.  If so, and the field
			 * is highlighted, and our terminal was highlighted,
			 * then we will leave a highlighted blank at this
			 * position.
			 */
			SetHighlightMode(pointer);
			c = 0;
			DoCharacterAt(c,pointer);
		    }
			/* We could be in the situation of needing to exit.
			 * This could happen if the current field wrapped around
			 * the end of the screen.
			 */
		    if (j > pointer) {
			break;
		    }
		} else {
		    c = GetHost(pointer);
			/* We always do the first character in a divergent
			 * field, since otherwise the start of a field in
			 * the Host structure may leave a highlighted blank
			 * on the screen, and the start of a field in the
			 * Terminal structure may leave a non-highlighted
			 * something in the middle of a highlighted field
			 * on the screen.
			 */
		    SetHighlightMode(pointer);
		    DoCharacterAt(c,pointer);
		}
	    } else {
		SetHighlightMode(pointer);
		/* The following will terminate at least when we get back
		 * to the original 'pointer' location (since we force
		 * things to be equal).
		 */
		while (((c = GetHost(pointer)) != GetTerminal(pointer)) &&
			!IsStartField(pointer) && !TermIsStartField(pointer)) {
		    DoCharacterAt(c, pointer);
		    pointer = ScreenInc(pointer);
		    if (!(--changes)) {
			DoARefresh();
			EmptyTerminal();
			if (HaveInput) {	/* if input came in, take it */
			    break;
			}
		    }
		}
	    }
	}
    }
    DoARefresh();
    Lowest = pointer;
    if (Lowest > Highest) {		/* if we finished input... */
	Lowest = HighestScreen()+1;
	Highest = LowestScreen()-1;
	terminalCursorAddress = CorrectTerminalCursor();
	if (ERR == move(ScreenLine(terminalCursorAddress),
			ScreenLineOffset(terminalCursorAddress))) {
	    OurExitString(stderr, "ERR from move\n", 1);
	}
	DoARefresh();
	if (needToRing) {
	    StringToTerminal(bellSequence);
	    needToRing = 0;
	}
    }
    EmptyTerminal();			/* move data along */
    return;
}


/* returns a 1 if no more output available (so, go ahead and block),
    or a 0 if there is more output available (so, just poll the other
    sources/destinations, don't block).
 */

int
DoTerminalOutput()
{
	/* called just before a select to conserve IO to terminal */
    if (Initialized &&
	    ((Lowest <= Highest) || needToRing ||
			(terminalCursorAddress != CorrectTerminalCursor()))) {
	TryToSend();
    }
    if (Lowest > Highest) {
	return(1);		/* no more output now */
    } else {
	return(0);		/* more output for future */
    }
}

/* returns the number of characters consumed */
int
DataFromNetwork(buffer, count, control)
register char	*buffer;		/* what the data is */
register int	count;			/* and how much there is */
int	control;			/* this buffer terminated block */
{
    int origCount;
    register int c;
    register int i;
    static int Command;
    static int Wcc;
    static int	LastWasTerminated = 0;	/* was "control" = 1 last time? */

    if (!Initialized) {		/* not initialized */
	int abort();

	bzero((char *)Host, sizeof Host);
	DeleteAllFields();
	for (i = 0; i < sizeof Blanks; i++) {
	    Blanks[i] = ' ';
	}
	bzero((char *)Terminal, sizeof Terminal);
	Lowest = HighestScreen()+1;
	Highest = LowestScreen()-1;
	terminalCursorAddress =
		CursorAddress =
		BufferAddress = SetBufferAddress(0,0);
	UnLocked = 1;
	StartScreen();
	LastWasTerminated = 1;
	Initialized = 1;
	OutputClock = 1;
	TransparentClock = -1;
	signal(SIGHUP, abort);
    }

    origCount = count;

    if (LastWasTerminated) {

	if (count < 2) {
	    if (count == 0) {
		StringToTerminal("Short count received from host!\n");
		return(count);
	    }
	    Command = buffer[0]&0xff;
	    switch (Command) {		/* This had better be a read command */
	    case CMD_READ_MODIFIED:
		DoReadModified();
		break;
	    case CMD_READ_BUFFER:
		DoReadBuffer();
		break;
	    default:
		break;
	    }
	    return(1);			/* We consumed everything */
	}
	Command = buffer[0]&0xff;
	Wcc = buffer[1]&0xff;
	if (Wcc & WCC_RESET_MDT) {
	    i = c = WhereAttrByte(LowestScreen());
	    do {
		if (HasMdt(i)) {
		    TurnOffMdt(i);
		}
		i = FieldInc(i);
	    } while (i != c);
	}

	switch (Command) {
	case CMD_ERASE_WRITE:
	    Clear3270();
	    if (TransparentClock == OutputClock) {
		clearok(curscr, TRUE);
	    }
	    break;
	case CMD_ERASE_ALL_UNPROTECTED:
	    CursorAddress = HighestScreen()+1;
	    for (i = LowestScreen(); i <= HighestScreen(); ScreenInc(i)) {
		if (IsUnProtected(i)) {
		    if (CursorAddress > i) {
			CursorAddress = i;
		    }
		    AddHost(i, '\0');
		}
		if (HasMdt(i)) {
		    TurnOffMdt(i);
		}
	    }
	    if (CursorAddress == HighestScreen()+1) {
		CursorAddress = SetBufferAddress(0,0);
	    }
	    UnLocked = 1;
	    AidByte = 0;
	    break;
	case CMD_WRITE:
	    break;
	default:
	    break;
	}

	count -= 2;			/* strip off command and wcc */
	buffer += 2;

    }
    LastWasTerminated = 0;		/* then, reset at end... */

    while (count) {
	count--;
	c = (*buffer++)&0xff;
	if (IsOrder(c)) {
	    /* handle an order */
	    switch (c) {
#		define Ensure(x)	if (count < x) { \
					    if (!control) { \
						return(origCount-(count+1)); \
					    } else { \
						/* XXX - should not occur */ \
						count = 0; \
						break; \
					    } \
					}
	    case ORDER_SF:
		Ensure(1);
		c = (*buffer++)&0xff;
		count--;
		if ( ! (IsStartField(BufferAddress) &&
					FieldAttributes(BufferAddress) == c)) {
		    if (NotVisuallyCompatibleAttributes(BufferAddress, c,
					FieldAttributes(BufferAddress))) {
			SetHighestLowest(BufferAddress);
		    }
		    if (GetTerminal(BufferAddress)) {
			SetHighestLowest(BufferAddress);
		    }
		    NewField(BufferAddress,c);
		}
		SetHost(BufferAddress, 0);
		BufferAddress = ScreenInc(BufferAddress);
		break;
	    case ORDER_SBA:
		Ensure(2);
		i = buffer[0];
		c = buffer[1];
		if (!i && !c) {	/* transparent write */
		    if (!control) {
			return(origCount-(count+1));
		    } else {
			while (Initialized &&
				((Lowest <= Highest) || needToRing ||
				(terminalCursorAddress !=
						CorrectTerminalCursor()))) {
			    extern int HaveInput;

			    HaveInput = 0;
			    TryToSend();
			}
			if (TransparentClock != OutputClock) {
			    if (!DoTerminalOutput()) {
				return(origCount-(count+1));
			    }
			    move(ScreenLine(CursorAddress),
					ScreenLineOffset(CursorAddress));
			    DoARefresh();
			}
			TransparentClock = OutputClock;		/* this clock */
			(void) DataToTerminal(buffer+2, count-2);
			SendToIBM();
			TransparentClock = OutputClock+1;	/* clock next */
			buffer += count;
			count -= count;
		    }
		} else {
		    BufferAddress = Addr3270(i, c);
		    buffer += 2;
		    count -= 2;
		}
		break;
	    case ORDER_IC:
		CursorAddress = BufferAddress;
		break;
	    case ORDER_PT:
		for (i = ScreenInc(BufferAddress); (i != HighestScreen());
				i = ScreenInc(i)) {
		    if (IsStartField(i)) {
			i = ScreenInc(i);
			if (!IsProtected(ScreenInc(i))) {
			    break;
			}
			if (i == HighestScreen()) {
			    break;
			}
		    }
		}
		CursorAddress = i;
		break;
	    case ORDER_RA:
		Ensure(2);
		i = Addr3270(buffer[0], buffer[1]);
		c = buffer[2];
		do {
		    AddHost(BufferAddress, c);
		    BufferAddress = ScreenInc(BufferAddress);
		} while (BufferAddress != i);
		buffer += 3;
		count -= 3;
		break;
	    case ORDER_EUA:    /* (from [here,there), ie: half open interval] */
		Ensure(2);
		c = FieldAttributes(WhereAttrByte(BufferAddress));
		for (i = Addr3270(buffer[0], buffer[1]); i != BufferAddress;
		    		BufferAddress = ScreenInc(BufferAddress)) {
		    if (!IsProtectedAttr(BufferAddress, c)) {
			AddHost(BufferAddress, 0);
		    }
		}
		buffer += 2;
		count -= 2;
		break;
	    case ORDER_YALE:		/* special YALE defined order */
		Ensure(2);	/* need at least two characters */
		if ((*buffer&0xff) == 0x5b) {
		    i = OptOrder(buffer+1, count-1, control);
		    if (i == 0) {
			return(origCount-(count+1));	/* come here again */
		    } else {
			buffer += 1 + i;
			count - = (1 + i);
		    }
		}
		break;
	    default:
		break;				/* XXX ? */
	    }
	    if (count < 0) {
		count = 0;
	    }
	} else {
	    /* Data comes in large clumps - take it all */
	    i = BufferAddress;
#ifdef	NOTDEF
	    AddHost(i, c);
#else	/* NOTDEF */
	    AddHostA(i, c);
	    SetHighestLowest(i);
#endif	/* NOTDEF */
	    i = ScreenInc(i);
	    while (count && !IsOrder(c = *(buffer)&0xff)) {
		buffer++;
#ifdef	NOTDEF
		AddHost(i, c);
#else	/* NOTDEF */
		AddHostA(i, c);
#endif	/* NOTDEF */
		i = ScreenInc(i);
#ifndef	NOTDEF
		if (i == LowestScreen()) {
		    SetHighestLowest(HighestScreen());
		}
#endif	/* NOTDEF */
		count--;
	    }
#ifndef	NOTDEF
	    SetHighestLowest(i);
#endif	/* NOTDEF */
	    BufferAddress = i;
	}
    }
    if (count == 0) {
	OutputClock++;		/* time rolls on */
	if (control) {
	    if (Wcc & WCC_RESTORE) {
		if (TransparentClock != OutputClock) {
		    AidByte = 0;
		}
		UnLocked = 1;
		(void) TerminalIn();	/* move along user's input */
	    }
	    if (Wcc & WCC_ALARM) {
		RingBell();
	    }
	}
	LastWasTerminated = control;	/* state for next time */
	return(origCount);
    } else {
	return(origCount-count);
    }
}
