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


#ifndef	lint
static	char	sccsid[] = "@(#)inbound.c	3.1  10/29/86";
#endif	/* ndef lint */


#include <stdio.h>

#include "../general.h"
#include "function.h"
#include "hostctlr.h"
#include "scrnctlr.h"
#include "screen.h"
#include "options.h"
#include "dctype.h"
#include "ebc_disp.h"

#include "../system/globals.h"
#include "inbound.ext"
#include "outbound.ext"
#include "../telnet.ext"

#define EmptyChar()	(ourPTail == ourPHead)
#define FullChar()	(ourPHead == ourBuffer+sizeof ourBuffer)


/*
 * We define something to allow us to to IsProtected() quickly
 * on unformatted screens (with the current algorithm for fields,
 * unprotected takes exponential time...).
 *
 *	The idea is to call SetXIsProtected() BEFORE the
 * loop, then use XIsProtected().
 */

#define	SetXIsProtected()	(XWasSF = 1)
#define	XIsProtected(p)	(IsStartField(p)? \
			    XWasSF = 1 : \
			    (XWasSF? \
				(XWasSF = 0, XProtected = IsProtected(p))  : \
				XProtected))

static char	ourBuffer[400];

static char	*ourPHead = ourBuffer,
		*ourPTail = ourBuffer;

static int	HadAid;			/* Had an AID haven't sent */

static int shifted,			/* Shift state of terminal */
	alted;				/* Alt state of terminal */

static int InsertMode;			/* is the terminal in insert mode? */

static int	XWasSF, XProtected;	/* For optimizations */
#if	!defined(PURE3274)
extern int TransparentClock, OutputClock;
#endif	/* !defined(PURE3274) */

#include "kbd.out"		/* Get keyboard mapping function */

/* the following are global variables */

extern int UnLocked;		/* keyboard is UnLocked? */


/*
 * init_inbound :
 *
 * Reset variables to initial state.
 */

void
init_inbound()
{
    ourPHead = ourPTail = ourBuffer;
    HadAid = 0;
    shifted = alted = 0;
    InsertMode = 0;
}


/* Tab() - sets cursor to the start of the next unprotected field */
static void
Tab()
{
    register int i, j;

    i = CursorAddress;
    j = WhereAttrByte(CursorAddress);
    do {
	if (IsStartField(i) && IsUnProtected(ScreenInc(i))) {
	    break;
	}
	i = FieldInc(i);
    } while (i != j);
    if (IsStartField(i) && IsUnProtected(ScreenInc(i))) {
	CursorAddress = ScreenInc(i);
    } else {
	CursorAddress = SetBufferAddress(0,0);
    }
}


/* BackTab() - sets cursor to the start of the most recent field */

static void
BackTab()
{
    register int i;

    i = ScreenDec(CursorAddress);
    for (;;) {
	if (IsStartField(ScreenDec(i)) && IsUnProtected(i)) {
	    CursorAddress = i;
	    break;
	}
	if (i == CursorAddress) {
	    CursorAddress = SetBufferAddress(0,0);
	    break;
	}
	i = ScreenDec(i);
    }
}


/* EraseEndOfField - erase all characters to the end of a field */

static void
EraseEndOfField()
{
    register int i;

    if (IsProtected(CursorAddress)) {
	RingBell("Protected Field");
    } else {
	TurnOnMdt(CursorAddress);
	if (FormattedScreen()) {
	    i = CursorAddress;
	    do {
		AddHost(i, 0);
		i = ScreenInc(i);
	    } while ((i != CursorAddress) && !IsStartField(i));
	} else {                            /* Screen is Unformatted */
	    i = CursorAddress;
	    do {
		AddHost(i, 0);
		i = ScreenInc(i);
	    } while (i != HighestScreen());
       } 
    }
}

/* Delete() - deletes a character from the screen
 *
 *	What we want to do is delete the section
 *	[where, from-1] from the screen,
 *	filling in with what comes at from.
 *
 *	The deleting continues to the end of the field (or
 *	until the cursor wraps).
 *
 *	From can be a start of a field.  We
 *	check for that.  However, there can't be any
 *	fields that start between where and from.
 *	We don't check for that.
 *
 *	Also, we assume that the protection status of
 *	everything has been checked by the caller.
 *
 */

static void
Delete(where, from)
register int	where,		/* Where to start deleting from */
		from;		/* Where to pull back from */
{
    register int i;

    TurnOnMdt(where);			/* Only do this once in this field */
    i = where;
    do {
	if (IsStartField(from)) {
	    AddHost(i, 0);		/* Stick the edge at the start field */
	} else {
	    AddHost(i, GetHost(from));
	    from = ScreenInc(from);		/* Move the edge */
	}
	i = ScreenInc(i);
    } while ((!IsStartField(i)) && (i != where));
}

static void
ColBak()
{
    register int i;

    i = ScreenLineOffset(CursorAddress);
    for (i = i-1; i >= 0; i--) {
	if (OptColTabs[i]) {
	    break;
	}
    }
    if (i < 0) {
	i = 0;
    }
    CursorAddress = SetBufferAddress(ScreenLine(CursorAddress), i);
}

static void
ColTab()
{
    register int i;

    i = ScreenLineOffset(CursorAddress);
    for (i = i+1; i < NumberColumns; i++) {
	if (OptColTabs[i]) {
	    break;
	}
    }
    if (i >= NumberColumns) {
	i = NumberColumns-1;
    }
    CursorAddress = SetBufferAddress(ScreenLine(CursorAddress), i);
}

static void
Home()
{
    register int i;
    register int j;

    i = SetBufferAddress(OptHome, 0);
    j = WhereLowByte(i);
    do {
	if (IsUnProtected(i)) {
	    CursorAddress = i;
	    return;
	}
	    /* the following could be a problem if we got here with an
	     * unformatted screen.  However, this is "impossible", since
	     * with an unformatted screen, the IsUnProtected(i) above
	     * should be true.
	     */
	i = ScreenInc(FieldInc(i));
    } while (i != j);
    CursorAddress = LowestScreen();
}

static
LastOfField(i)
register int	i;	/* position to start from */
{
    register int j;
    register int k;

    k = j = i;
    SetXIsProtected();
    while (XIsProtected(i) || Disspace(GetHost(i))) {
	i = ScreenInc(i);
	if (i == j) {
	    break;
	}
    }
	    /* We are now IN a word IN an unprotected field (or wrapped) */
    while (!XIsProtected(i)) {
	if (!Disspace(GetHost(i))) {
	    k = i;
	}
	i = ScreenInc(i);
	if (i == j) {
	    break;
	}
    }
    return(k);
}


static void
FlushChar()
{
    ourPTail = ourPHead = ourBuffer;
}


/*
 * Add one EBCDIC (NOT display code) character to the buffer.
 */

static void
AddChar(character)
char	character;
{
    if (FullChar()) {
	ourPTail += DataToNetwork(ourPTail, ourPHead-ourPTail, 0);
	if (EmptyChar()) {
	    FlushChar();
	} else {
	    char buffer[100];

	    sprintf(buffer, "File %s, line %d:  No room in network buffer!\n",
				__FILE__, __LINE__);
	    ExitString(stderr, buffer, 1);
	    /*NOTREACHED*/
	}
    }
    *ourPHead++ = character;
}


static void
SendUnformatted()
{
    register int i, j;
    register int Nulls;
    register int c;

			/* look for start of field */
    Nulls = 0;
    i = j = LowestScreen();
    do {
	c = GetHost(i);
	if (c == 0) {
	    Nulls++;
	} else {
	    while (Nulls) {
		Nulls--;
		AddChar(EBCDIC_BLANK);		/* put in blanks */
	    }
	    AddChar(disp_ebc[c]);
	}
	i = ScreenInc(i);
    } while (i != j);
}

static
SendField(i, command)
register int i;			/* where we saw MDT bit */
int	command;		/* The command code (type of read) */
{
    register int j;
    register int k;
    register int Nulls;
    register int c;

			/* look for start of field */
    i = j = WhereLowByte(i);

		/* On a test_request_read, don't send sba and address */
    if ((AidByte != AID_TREQ)
			|| (command == CMD_SNA_READ_MODIFIED_ALL)) {
	AddChar(ORDER_SBA);		/* set start field */
	AddChar(BufferTo3270_0(j));	/* set address of this field */
	AddChar(BufferTo3270_1(j));
    }
		/*
		 * Only on read_modified_all do we return the contents
		 * of the field when the attention was caused by a
		 * selector pen.
		 */
    if ((AidByte != AID_SELPEN)
			|| (command == CMD_SNA_READ_MODIFIED_ALL)) {
	if (!IsStartField(j)) {
	    Nulls = 0;
	    k = ScreenInc(WhereHighByte(j));
	    do {
		c = GetHost(j);
		if (c == 0) {
		    Nulls++;
		} else {
		    while (Nulls) {
			Nulls--;
			AddChar(EBCDIC_BLANK);		/* put in blanks */
		    }
		    AddChar(disp_ebc[c]);
		}
		j = ScreenInc(j);
	    } while ((j != k) && (j != i));
	}
    } else {
	j = FieldInc(j);
    }
    return(j);
}

/* Various types of reads... */
void
DoReadModified(command)
int	command;			/* The command sent */
{
    register int i, j;

    if (AidByte) {
	if (AidByte != AID_TREQ) {
	    AddChar(AidByte);
	} else {
		/* Test Request Read header */
	    AddChar(EBCDIC_SOH);
	    AddChar(EBCDIC_PERCENT);
	    AddChar(EBCDIC_SLASH);
	    AddChar(EBCDIC_STX);
	}
    } else {
	AddChar(AID_NONE);
    }
    if (((AidByte != AID_PA1) && (AidByte != AID_PA2)
	    && (AidByte != AID_PA3) && (AidByte != AID_CLEAR))
	    || (command == CMD_SNA_READ_MODIFIED_ALL)) {
	if ((AidByte != AID_TREQ)
	    || (command == CMD_SNA_READ_MODIFIED_ALL)) {
		/* Test request read_modified doesn't give cursor address */
	    AddChar(BufferTo3270_0(CursorAddress));
	    AddChar(BufferTo3270_1(CursorAddress));
	}
	i = j = WhereAttrByte(LowestScreen());
	/* Is this an unformatted screen? */
	if (!IsStartField(i)) {		/* yes, handle separate */
	    SendUnformatted();
	} else {
	    do {
		if (HasMdt(i)) {
		    i = SendField(i, command);
		} else {
		    i = FieldInc(i);
		}
	    } while (i != j);
	}
    }
    ourPTail += DataToNetwork(ourPTail, ourPHead-ourPTail, 1);
    if (EmptyChar()) {
	FlushChar();
	HadAid = 0;			/* killed that buffer */
    }
}

/* A read buffer operation... */

void
DoReadBuffer()
{
    register int i, j;

    if (AidByte) {
	AddChar(AidByte);
    } else {
	AddChar(AID_NONE);
    }
    AddChar(BufferTo3270_0(CursorAddress));
    AddChar(BufferTo3270_1(CursorAddress));
    i = j = LowestScreen();
    do {
	if (IsStartField(i)) {
	    AddChar(ORDER_SF);
	    AddChar(BufferTo3270_1(FieldAttributes(i)));
	} else {
	    AddChar(disp_ebc[GetHost(i)]);
	}
	i = ScreenInc(i);
    } while (i != j);
    ourPTail += DataToNetwork(ourPTail, ourPHead-ourPTail, 1);
    if (EmptyChar()) {
	FlushChar();
	HadAid = 0;			/* killed that buffer */
    }
}
/* Try to send some data to host */

void
SendToIBM()
{
#if	!defined(PURE3274)
    if (TransparentClock == OutputClock) {
	if (HadAid) {
	    AddChar(AidByte);
	    HadAid = 0;
	} else {
	    AddChar(AID_NONE_PRINTER);
	}
	do {
	    ourPTail += DataToNetwork(ourPTail, ourPHead-ourPTail, 1);
	} while (!EmptyChar());
	FlushChar();
    } else if (HadAid) {
	DoReadModified(CMD_READ_MODIFIED);
    }
#else	/* !defined(PURE3274) */
    if (HadAid) {
	DoReadModified(CMD_READ_MODIFIED);
    }
#endif	/* !defined(PURE3274) */
}

/* This takes in one character from the keyboard and places it on the
 * screen.
 */

static void
OneCharacter(c, insert)
int c;			/* character (Ebcdic) to be shoved in */
int insert;		/* are we in insert mode? */
{
    register int i, j;

    if (IsProtected(CursorAddress)) {
	RingBell("Protected Field");
	return;
    }
    if (insert) {
	/* is the last character in the field a blank or null? */
	i = ScreenDec(FieldInc(CursorAddress));
	j = GetHost(i);
	if (!Disspace(j)) {
	    RingBell("No more room for insert");
	    return;
	} else {
	    for (j = ScreenDec(i); i != CursorAddress;
			    j = ScreenDec(j), i = ScreenDec(i)) {
		AddHost(i, GetHost(j));
	    }
	}
    }
    AddHost(CursorAddress, c);
    TurnOnMdt(CursorAddress);
    CursorAddress = ScreenInc(CursorAddress);
    if (IsStartField(CursorAddress) &&
		((FieldAttributes(CursorAddress)&ATTR_AUTO_SKIP_MASK) ==
							ATTR_AUTO_SKIP_VALUE)) {
	Tab();
    }
}

/* go through data until an AID character is hit, then generate an interrupt */

int
DataFrom3270(buffer, count)
unsigned char	*buffer;		/* where the data is */
int	count;				/* how much data there is */
{
    int origCount;
    register int c;
    register int i;
    register int j;
    enum ctlrfcn ctlrfcn;
#   define HITNUM() ((shifted? 1:0) + ((alted?1:0)<<1))

    if (*buffer >= numberof(hits)) {
	ExitString(stderr,
		"Unknown scancode encountered in DataFrom3270.\n", 1);
	/*NOTREACHED*/
    }
    ctlrfcn = hits[*buffer].hit[HITNUM()].ctlrfcn;
    c = hits[*buffer].hit[HITNUM()].code;

    if (!UnLocked || HadAid) {
	if (HadAid) {
	    SendToIBM();
	    if (!EmptyChar()) {
		return(0);			/* nothing to do */
	    }
	}
#if	!defined(PURE3274)
	if (!HadAid && EmptyChar()) {
	    if ((ctlrfcn == FCN_RESET) || (ctlrfcn == FCN_MASTER_RESET)) {
		UnLocked = 1;
	    }
	}
#endif	/* !defined(PURE3274) */
	if (!UnLocked) {
	    return(0);
	}
    }
    /* now, either empty, or haven't seen aid yet */

    origCount = count;

#if	!defined(PURE3274)
    if (TransparentClock == OutputClock) {
	while (count) {
	    if (*buffer >= numberof(hits)) {
		ExitString(stderr,
			"Unknown scancode encountered in DataFrom3270.\n", 1);
		/*NOTREACHED*/
	    }
	    ctlrfcn = hits[*buffer].hit[HITNUM()].ctlrfcn;
	    c = hits[*buffer].hit[HITNUM()].code;
	    buffer++;
	    count--;
	    if (ctlrfcn == FCN_AID) {
		UnLocked = 0;
		InsertMode = 0;
		AidByte = (c);
		HadAid = 1;
	    } else {
		switch (ctlrfcn) {
		case FCN_ESCAPE:
		    StopScreen(1);
		    command(0);
		    ConnectScreen();
		    break;

		case FCN_RESET:
		case FCN_MASTER_RESET:
		    UnLocked = 1;
		    break;

		default:
		    return(origCount-(count+1));
		}
	    }
	}
    }
#endif	/* !defined(PURE3274) */

    while (count) {
	if (*buffer >= numberof(hits)) {
	    ExitString(stderr,
			"Unknown scancode encountered in DataFrom3270.\n", 1);
	    /*NOTREACHED*/
	}
	ctlrfcn = hits[*buffer].hit[HITNUM()].ctlrfcn;
	c = hits[*buffer].hit[HITNUM()].code;
	buffer++;
	count--;

	if (ctlrfcn == FCN_CHARACTER) {
			/* Add the character to the buffer */
	    OneCharacter(c, InsertMode);
	} else if (ctlrfcn == FCN_AID) {		/* got Aid */
	    if (c == AID_CLEAR) {
		LocalClearScreen();	/* Side effect is to clear 3270 */
	    }
	    UnLocked = 0;
	    InsertMode = 0;		/* just like a 3278 */
	    AidByte = c;
	    HadAid = 1;
	    SendToIBM();
	    return(origCount-count);
	} else {
	    switch (ctlrfcn) {

	    case FCN_MAKE_SHIFT:
		shifted++;
		break;
	    case FCN_BREAK_SHIFT:
		shifted--;
		if (shifted < 0) {
		    ExitString(stderr,
				"More BREAK_SHIFT than MAKE_SHIFT.\n", 1);
		    /*NOTREACHED*/
		}
		break;
	    case FCN_MAKE_ALT:
		alted++;
		break;
	    case FCN_BREAK_ALT:
		alted--;
		if (alted < 0) {
		    ExitString(stderr, "More BREAK_ALT than MAKE_ALT.\n", 1);
		    /*NOTREACHED*/
		}
		break;
	    case FCN_CURSEL:
		c = FieldAttributes(CursorAddress)&ATTR_DSPD_MASK;
		if (!FormattedScreen()
			|| ((c != ATTR_DSPD_DSPD) && (c != ATTR_DSPD_HIGH))) {
		    RingBell("Cursor not in selectable field");
		} else {
		    i = ScreenInc(WhereAttrByte(CursorAddress));
		    c = GetHost(i);
		    if (c == DISP_QUESTION) {
			AddHost(i, DISP_GREATER_THAN);
			TurnOnMdt(i);
		    } else if (c == DISP_GREATER_THAN) {
			AddHost(i, DISP_QUESTION);
			TurnOffMdt(i);
		    } else if (c == DISP_BLANK || c == DISP_NULL
						|| c == DISP_AMPERSAND) {
			UnLocked = 0;
			InsertMode = 0;
			if (c == DISP_AMPERSAND) {
			    TurnOnMdt(i);	/* Only for & type */
			    AidByte = AID_ENTER;
			} else {
			    AidByte = AID_SELPEN;
			}
			HadAid = 1;
			SendToIBM();
		    } else {
			RingBell(
			    "Cursor not in a selectable field (designator)");
		    }
		}
		break;

#if	!defined(PURE3274)
	    case FCN_ERASE:
		if (IsProtected(ScreenDec(CursorAddress))) {
		    RingBell("Protected Field");
		} else {
		    CursorAddress = ScreenDec(CursorAddress);
		    Delete(CursorAddress, ScreenInc(CursorAddress));
		}
		break;
	    case FCN_WERASE:
		j = CursorAddress;
		i = ScreenDec(j);
		if (IsProtected(i)) {
		    RingBell("Protected Field");
		} else {
		    SetXIsProtected();
		    while ((!XIsProtected(i) && Disspace(GetHost(i)))
							&& (i != j)) {
			i = ScreenDec(i);
		    }
		    /* we are pointing at a character in a word, or
		     * at a protected position
		     */
		    while ((!XIsProtected(i) && !Disspace(GetHost(i)))
							&& (i != j)) {
			i = ScreenDec(i);
		    }
		    /* we are pointing at a space, or at a protected
		     * position
		     */
		    CursorAddress = ScreenInc(i);
		    Delete(CursorAddress, j);
		}
		break;

	    case FCN_FERASE:
		if (IsProtected(CursorAddress)) {
		    RingBell("Protected Field");
		} else {
		    CursorAddress = ScreenInc(CursorAddress);	/* for btab */
		    BackTab();
		    EraseEndOfField();
		}
		break;

	    case FCN_RESET:
		InsertMode = 0;
		break;
	    case FCN_MASTER_RESET:
		InsertMode = 0;
		RefreshScreen();
		break;
#endif	/* !defined(PURE3274) */

	    case FCN_UP:
		CursorAddress = ScreenUp(CursorAddress);
		break;

	    case FCN_LEFT:
		CursorAddress = ScreenDec(CursorAddress);
		break;

	    case FCN_RIGHT:
		CursorAddress = ScreenInc(CursorAddress);
		break;

	    case FCN_DOWN:
		CursorAddress = ScreenDown(CursorAddress);
		break;

	    case FCN_DELETE:
		if (IsProtected(CursorAddress)) {
		    RingBell("Protected Field");
		} else {
		    Delete(CursorAddress, ScreenInc(CursorAddress));
		}
		break;

	    case FCN_INSRT:
		InsertMode = !InsertMode;
		break;

	    case FCN_HOME:
		Home();
		break;

	    case FCN_NL:
		/* The algorithm is to look for the first unprotected
		 * column after column 0 of the following line.  Having
		 * found that unprotected column, we check whether the
		 * cursor-address-at-entry is at or to the right of the
		 * LeftMargin AND the LeftMargin column of the found line
		 * is unprotected.  If this conjunction is true, then
		 * we set the found pointer to the address of the LeftMargin
		 * column in the found line.
		 * Then, we set the cursor address to the found address.
		 */
		i = SetBufferAddress(ScreenLine(ScreenDown(CursorAddress)), 0);
		j = ScreenInc(WhereAttrByte(CursorAddress));
		do {
		    if (IsUnProtected(i)) {
			break;
		    }
		    /* Again (see comment in Home()), this COULD be a problem
		     * with an unformatted screen.
		     */
		    /* If there was a field with only an attribute byte,
		     * we may be pointing to the attribute byte of the NEXT
		     * field, so just look at the next byte.
		     */
		    if (IsStartField(i)) {
			i = ScreenInc(i);
		    } else {
			i = ScreenInc(FieldInc(i));
		    }
		} while (i != j);
		if (!IsUnProtected(i)) {	/* couldn't find unprotected */
		    i = SetBufferAddress(0,0);
		}
		if (OptLeftMargin <= ScreenLineOffset(CursorAddress)) {
		    if (IsUnProtected(SetBufferAddress(ScreenLine(i),
							    OptLeftMargin))) {
			i = SetBufferAddress(ScreenLine(i), OptLeftMargin);
		    }
		}
		CursorAddress = i;
		break;

	    case FCN_EINP:
		if (!FormattedScreen()) {
		    i = CursorAddress;
		    TurnOffMdt(i);
		    do {
			AddHost(i, 0);
			i = ScreenInc(i);
		    } while (i != CursorAddress);
		} else {
			/*
			 * The algorithm is:  go through each unprotected
			 * field on the screen, clearing it out.  When
			 * we are at the start of a field, skip that field
			 * if its contents are protected.
			 */
		    i = j = FieldInc(CursorAddress);
		    do {
			if (IsUnProtected(ScreenInc(i))) {
			    i = ScreenInc(i);
			    TurnOffMdt(i);
			    do {
			       AddHost(i, 0);
			       i = ScreenInc(i);
			    } while (!IsStartField(i));
			} else {
			    i = FieldInc(i);
			}
		    } while (i != j);
		}
		Home();
		break;

	    case FCN_EEOF:
		EraseEndOfField();
		break;

	    case FCN_SPACE:
		OneCharacter(DISP_BLANK, InsertMode);  /* Add cent */
		break;

	    case FCN_CENTSIGN:
		OneCharacter(DISP_CENTSIGN, InsertMode);  /* Add cent */
		break;

	    case FCN_FM:
		OneCharacter(DISP_FM, InsertMode);  /* Add field mark */
		break;

	    case FCN_DP:
		if (IsProtected(CursorAddress)) {
		    RingBell("Protected Field");
		} else {
		    OneCharacter(DISP_DUP, InsertMode);/* Add dup character */
		    Tab();
		}
		break;

	    case FCN_TAB:
		Tab();
		break;

	    case FCN_BTAB:
		BackTab();
		break;

#ifdef	NOTUSED			/* Actually, this is superseded by unix flow
				 * control.
				 */
	    case FCN_XOFF:
		Flow = 0;			/* stop output */
		break;

	    case FCN_XON:
		if (!Flow) {
		    Flow = 1;			/* turn it back on */
		    DoTerminalOutput();
		}
		break;
#endif	/* NOTUSED */

#if	!defined(PURE3274)
	    case FCN_ESCAPE:
		/* FlushChar(); do we want to flush characters from before? */
		StopScreen(1);
		command(0);
		ConnectScreen();
		break;

	    case FCN_DISC:
		StopScreen(1);
		suspend();
		setconnmode();
		ConnectScreen();
		break;

	    case FCN_RESHOW:
		RefreshScreen();
		break;

	    case FCN_SETTAB:
		OptColTabs[ScreenLineOffset(CursorAddress)] = 1;
		break;

	    case FCN_DELTAB:
		OptColTabs[ScreenLineOffset(CursorAddress)] = 0;
		break;

		/*
		 * Clear all tabs, home line, and left margin.
		 */
	    case FCN_CLRTAB:
		for (i = 0; i < sizeof OptColTabs; i++) {
		    OptColTabs[i] = 0;
		}
		OptHome = 0;
		OptLeftMargin = 0;
		break;

	    case FCN_COLTAB:
		ColTab();
		break;

	    case FCN_COLBAK:
		ColBak();
		break;

	    case FCN_INDENT:
		ColTab();
		OptLeftMargin = ScreenLineOffset(CursorAddress);
		break;

	    case FCN_UNDENT:
		ColBak();
		OptLeftMargin = ScreenLineOffset(CursorAddress);
		break;

	    case FCN_SETMRG:
		OptLeftMargin = ScreenLineOffset(CursorAddress);
		break;

	    case FCN_SETHOM:
		OptHome = ScreenLine(CursorAddress);
		break;

		/*
		 * Point to first character of next unprotected word on
		 * screen.
		 */
	    case FCN_WORDTAB:
		i = CursorAddress;
		SetXIsProtected();
		while (!XIsProtected(i) && !Disspace(GetHost(i))) {
		    i = ScreenInc(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		/* i is either protected, a space (blank or null),
		 * or wrapped
		 */
		while (XIsProtected(i) || Disspace(GetHost(i))) {
		    i =  ScreenInc(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		CursorAddress = i;
		break;

	    case FCN_WORDBACKTAB:
		i = ScreenDec(CursorAddress);
		SetXIsProtected();
		while (XIsProtected(i) || Disspace(GetHost(i))) {
		    i = ScreenDec(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		    /* i is pointing to a character IN an unprotected word
		     * (or i wrapped)
		     */
		while (!Disspace(GetHost(i))) {
		    i = ScreenDec(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		CursorAddress = ScreenInc(i);
		break;

			/* Point to last non-blank character of this/next
			 * unprotected word.
			 */
	    case FCN_WORDEND:
		i = ScreenInc(CursorAddress);
		SetXIsProtected();
		while (XIsProtected(i) || Disspace(GetHost(i))) {
		    i = ScreenInc(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
			/* we are pointing at a character IN an
			 * unprotected word (or we wrapped)
			 */
		while (!Disspace(GetHost(i))) {
		    i = ScreenInc(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		CursorAddress = ScreenDec(i);
		break;

			/* Get to last non-blank of this/next unprotected
			 * field.
			 */
	    case FCN_FIELDEND:
		i = LastOfField(CursorAddress);
		if (i != CursorAddress) {
		    CursorAddress = i;		/* We moved; take this */
		} else {
		    j = FieldInc(CursorAddress);	/* Move to next field */
		    i = LastOfField(j);
		    if (i != j) {
			CursorAddress = i;	/* We moved; take this */
		    }
			/* else - nowhere else on screen to be; stay here */
		}
		break;
#endif	/* !defined(PURE3274) */

	    default:
				    /* We don't handle this yet */
		RingBell("Function not implemented");
	    }
	}
    }
    return(origCount-count);
}
