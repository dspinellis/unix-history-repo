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



/* test stub for DataFrom3270, etc. */

#define DEFINEAIDS
#include "m4.out"		/* output of termcodes.m4 */
#include "ascebc.h"
#include "3270.h"
#include "screen.h"
#include "options.h"
#include "ectype.h"

#ifndef	lint
static char sccsid[] = "@(#)keyboard.c	2.6	4/4/86";
#endif	/* ndef lint */

#define EmptyChar	(ourPTail == ourBuffer)
#define FullChar	(ourPTail == ourBuffer+sizeof ourBuffer)

extern char	ascebc[NASCEBC][NASCII];

static char	ourBuffer[4000];

static char	*ourPHead = ourBuffer,
		*ourPTail = ourBuffer;

static int	trTbl = AE_IN;		/* which ascii->ebcdic tr table */

static int	HadAid = 0;		/* Had an AID haven't sent */

/* the following are global variables */

extern int UnLocked;		/* keyboard is UnLocked? */

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

static
EraseEndOfField()
{
    register int i;

    if (IsProtected(CursorAddress)) {
	RingBell();
    } else {
	TurnOnMdt(CursorAddress);
	i = CursorAddress;
	do {
	    AddHost(i, 0);
	    i = ScreenInc(i);
	} while ((i != CursorAddress) && IsUnProtected(i));
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

static
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

ColTab()
{
    register int i;

    i = ScreenLineOffset(CursorAddress);
    for (i = i+1; i < LINESIZE; i++) {
	if (OptColTabs[i]) {
	    break;
	}
    }
    if (i >= LINESIZE) {
	i = LINESIZE-1;
    }
    CursorAddress = SetBufferAddress(ScreenLine(CursorAddress), i);
}

static
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
    while (IsProtected(i) || Eisspace(GetHost(i))) {
	i = ScreenInc(i);
	if (i == j) {
	    break;
	}
    }
	    /* We are now IN a word IN an unprotected field (or wrapped) */
    while (!IsProtected(i)) {
	if (!Eisspace(GetHost(i))) {
	    k = i;
	}
	i = ScreenInc(i);
	if (i == j) {
	    break;
	}
    }
    return(k);
}


static
FlushChar()
{
    ourPTail = ourPHead = ourBuffer;
}


static
AddChar(character)
char	character;
{
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
		AddChar(0x40);		/* put in blanks */
	    }
	    AddChar(c);
	}
	i = ScreenInc(i);
    } while (i != j);
}

static
SendField(i)
register int i;			/* where we saw MDT bit */
{
    register int j;
    register int k;
    register int Nulls;
    register int c;

			/* look for start of field */
    i = j = WhereLowByte(i);

    AddChar(ORDER_SBA);		/* set start field */
    AddChar(BufferTo3270_0(j));	/* set address of this field */
    AddChar(BufferTo3270_1(j));

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
		    AddChar(0x40);		/* put in blanks */
		}
		AddChar(c);
	    }
	    j = ScreenInc(j);
	} while ((j != k) && (j != i));
    }
    return(j);
}

/* Various types of reads... */
DoReadModified()
{
    register int i, j;

    if (AidByte) {
	AddChar(AidByte);
    } else {
	AddChar(0x60);
    }
    if ((AidByte != AID_PA1) && (AidByte != AID_PA2) && (AidByte != AID_PA3)
		    && (AidByte != AID_CLEAR)) {
	AddChar(BufferTo3270_0(CursorAddress));
	AddChar(BufferTo3270_1(CursorAddress));
	i = j = WhereAttrByte(LowestScreen());
	/* Is this an unformatted screen? */
	if (!IsStartField(i)) {		/* yes, handle separate */
	    SendUnformatted();
	} else {
	    do {
		if (HasMdt(i)) {
		    i = SendField(i);
		} else {
		    i = FieldInc(i);
		}
	    } while (i != j);
	}
    }
    ourPTail += DataToNetwork(ourPTail, ourPHead-ourPTail);
    if (ourPTail == ourPHead) {
	FlushChar();
	HadAid = 0;			/* killed that buffer */
    }
}

/* A read buffer operation... */

DoReadBuffer()
{
    register int i, j;

    if (AidByte) {
	AddChar(AidByte);
    } else {
	AddChar(0x60);
    }
    AddChar(BufferTo3270_0(CursorAddress));
    AddChar(BufferTo3270_1(CursorAddress));
    i = j = LowestScreen();
    do {
	if (IsStartField(i)) {
	    AddChar(ORDER_SF);
	    AddChar(BufferTo3270_1(FieldAttributes(i)));
	} else {
	    AddChar(GetHost(i));
	}
	i = ScreenInc(i);
    } while (i != j);
    ourPTail += DataToNetwork(ourPTail, ourPHead-ourPTail);
    if (ourPTail == ourPHead) {
	FlushChar();
	HadAid = 0;			/* killed that buffer */
    }
}
/* Try to send some data to host */

SendToIBM()
{
    extern int TransparentClock, OutputClock;

    if (TransparentClock == OutputClock) {
	if (HadAid) {
	    AddChar(AidByte);
	    HadAid = 0;
	} else {
	    AddChar(0xe8);
	}
	do {
	    ourPTail += DataToNetwork(ourPTail, ourPHead-ourPTail);
	} while (ourPTail != ourPHead);
	FlushChar();
    } else if (HadAid) {
	DoReadModified();
    }
    netflush();
}

/* This takes in one character from the keyboard and places it on the
 * screen.
 */

static
OneCharacter(c, insert)
int c;			/* character (Ebcdic) to be shoved in */
int insert;		/* are we in insert mode? */
{
    register int i, j;

    if (IsProtected(CursorAddress)) {
	RingBell();
	return;
    }
    if (insert) {
	/* is the last character in the field a blank or null? */
	i = ScreenDec(FieldInc(CursorAddress));
	j = GetHost(i);
	if (!Eisspace(j)) {
	    RingBell();
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

DataFrom3270(buffer, count)
char	*buffer;		/* where the data is */
int	count;			/* how much data there is */
{
    int origCount;
    register int c;
    register int i;
    register int j;
    static int InsertMode = 0;	/* is the terminal in insert mode? */

    extern int OutputClock, TransparentClock;

    if (!UnLocked || HadAid) {
	if (HadAid) {
	    SendToIBM();
	    if (!EmptyChar) {
		return(0);			/* nothing to do */
	    }
	}
	if (!HadAid && (((*buffer&0xff) == TC_RESET) ||
			((*buffer&0xff) == TC_MASTER_RESET)) && EmptyChar) {
	    UnLocked = 1;
	}
	if (!UnLocked) {
	    return(0);
	}
    }
    /* now, either empty, or haven't seen aid yet */

    origCount = count;

    if (TransparentClock == OutputClock) {
	while (count) {
	    c = (*buffer++)&0xff;
	    count--;
	    if (IsAid(c)) {
		UnLocked = 0;
		InsertMode = 0;
		AidByte = TCtoAid(c);
		HadAid = 1;
	    } else {
		switch (c) {
		case TC_ESCAPE:
		    Stop3270(1);
		    command(0);
		    ConnectScreen();
		    break;

		case TC_RESET:
		case TC_MASTER_RESET:
		    UnLocked = 1;
		    break;

		default:
		    return(origCount-(count+1));
		}
	    }
	}
    }

    while (count) {
	c = (*buffer++)&0xff;
	count--;

	if (!IsTc(c)) {
			/* Add the character to the buffer */
	    OneCharacter(ascebc[trTbl][c], InsertMode);
	} else if (IsAid(c)) {		/* got Aid */
	    if (c == TC_CLEAR) {
		LocalClear3270();
	    }
	    UnLocked = 0;
	    InsertMode = 0;		/* just like a 3278 */
	    AidByte = TCtoAid(c);
	    HadAid = 1;
	    SendToIBM();
	    return(origCount-count);
	} else {

			/* non-AID TC character */
	    switch (c) {

	    case TC_ERASE:
		if (IsProtected(ScreenDec(CursorAddress))) {
		    RingBell();
		} else {
		    CursorAddress = ScreenDec(CursorAddress);
		    Delete(CursorAddress, ScreenInc(CursorAddress));
		}
		break;

	    case TC_WERASE:
		j = CursorAddress;
		i = ScreenDec(j);
		if (IsProtected(i)) {
		    RingBell();
		} else {
		    while ((!IsProtected(i) && Eisspace(GetHost(i)))
							&& (i != j)) {
			i = ScreenDec(i);
		    }
		    /* we are pointing at a character in a word, or
		     * at a protected position
		     */
		    while ((!IsProtected(i) && !Eisspace(GetHost(i)))
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

	    case TC_FERASE:
		if (IsProtected(CursorAddress)) {
		    RingBell();
		} else {
		    CursorAddress = ScreenInc(CursorAddress);	/* for btab */
		    BackTab();
		    EraseEndOfField();
		}
		break;

	    case TC_RESET:
		InsertMode = 0;
		break;

	    case TC_MASTER_RESET:
		InsertMode = 0;
		RefreshScreen();
		break;

	    case TC_UP:
		CursorAddress = ScreenUp(CursorAddress);
		break;

	    case TC_LEFT:
		CursorAddress = ScreenDec(CursorAddress);
		break;

	    case TC_RIGHT:
		CursorAddress = ScreenInc(CursorAddress);
		break;

	    case TC_DOWN:
		CursorAddress = ScreenDown(CursorAddress);
		break;

	    case TC_DELETE:
		if (IsProtected(CursorAddress)) {
		    RingBell();
		} else {
		    Delete(CursorAddress, ScreenInc(CursorAddress));
		}
		break;

	    case TC_INSRT:
		InsertMode = !InsertMode;
		break;

	    case TC_HOME:
		Home();
		break;

	    case TC_NL:
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

	    case TC_EINP:
		i = j = ScreenInc(WhereAttrByte(LowestScreen()));
		do {
		    if (IsUnProtected(i)) {
			AddHost(i, 0);
			TurnOnMdt(i);
		    } else {
			    /* FieldInc() puts us at the start of the next
			     * field.
			     *
			     * We don't want to skip to the start of the
			     * next field if we are on the attribute byte,
			     * since we may be skipping over an otherwise
			     * unprotected field.
			     *
			     * Also, j points at the first byte of the first
			     * field on the screen, unprotected or not.  If
			     * we never point there, we might loop here for
			     * ever.
			     */
			if (!IsStartField(i)) {
			    i = FieldInc(i);
			}
		    }
		    i = ScreenInc(i);
		} while (i != j);
		Home();		/* get to home position */
		break;

	    case TC_EEOF:
		EraseEndOfField();
		break;

	    case TC_FM:
		if (IsProtected(CursorAddress)) {
		    RingBell();
		} else {
		    OneCharacter(EBCDIC_FM, InsertMode);  /* Add field mark */
		}
		break;

	    case TC_DP:
		if (IsProtected(CursorAddress)) {
		    RingBell();
		    break;
		}
		OneCharacter(EBCDIC_DUP, InsertMode);	/* Add dup character */
		Tab();
		break;

	    case TC_TAB:
		Tab();
		break;

	    case TC_BTAB:
		BackTab();
		break;

#ifdef	NOTUSED			/* Actually, this is superseded by unix flow
				 * control.
				 */
	    case TC_XOFF:
		Flow = 0;			/* stop output */
		break;

	    case TC_XON:
		if (!Flow) {
		    Flow = 1;			/* turn it back on */
		    DoTerminalOutput();
		}
		break;
#endif	/* NOTUSED */

	    case TC_ESCAPE:
		/* FlushChar(); do we want to flush characters from before? */
		Stop3270(1);
		command(0);
		ConnectScreen();
		break;

	    case TC_DISC:
		Stop3270(1);
		suspend();
		ConnectScreen();
		break;

	    case TC_RESHOW:
		RefreshScreen();
		break;

	    case TC_SETTAB:
		OptColTabs[ScreenLineOffset(CursorAddress)] = 1;
		break;

	    case TC_DELTAB:
		OptColTabs[ScreenLineOffset(CursorAddress)] = 0;
		break;

	    case TC_CLRTAB:
		for (i = 0; i < sizeof OptColTabs; i++) {
		    OptColTabs[i] = 0;
		}
		break;

	    case TC_COLTAB:
		ColTab();
		break;

	    case TC_COLBAK:
		ColBak();
		break;

	    case TC_INDENT:
		ColTab();
		OptLeftMargin = ScreenLineOffset(CursorAddress);
		break;

	    case TC_UNDENT:
		ColBak();
		OptLeftMargin = ScreenLineOffset(CursorAddress);
		break;

	    case TC_SETMRG:
		OptLeftMargin = ScreenLineOffset(CursorAddress);
		break;

	    case TC_SETHOM:
		OptHome = ScreenLine(CursorAddress);
		break;

		/*
		 * Point to first character of next unprotected word on
		 * screen.
		 */
	    case TC_WORDTAB:
		i = CursorAddress;
		while (!IsProtected(i) && !Eisspace(GetHost(i))) {
		    i = ScreenInc(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		/* i is either protected, a space (blank or null),
		 * or wrapped
		 */
		while (IsProtected(i) || Eisspace(GetHost(i))) {
		    i =  ScreenInc(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		CursorAddress = i;
		break;

	    case TC_WORDBACKTAB:
		i = ScreenDec(CursorAddress);
		while (IsProtected(i) || Eisspace(GetHost(i))) {
		    i = ScreenDec(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
		    /* i is pointing to a character IN an unprotected word
		     * (or i wrapped)
		     */
		while (!Eisspace(GetHost(i))) {
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
	    case TC_WORDEND:
		i = ScreenInc(CursorAddress);
		while (IsProtected(i) || Eisspace(GetHost(i))) {
		    i = ScreenInc(i);
		    if (i == CursorAddress) {
			break;
		    }
		}
			/* we are pointing at a character IN an
			 * unprotected word (or we wrapped)
			 */
		while (!Eisspace(GetHost(i))) {
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
	    case TC_FIELDEND:
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

	    default:
		RingBell();		/* We don't handle this yet */
	    }
	}
    }
    return(origCount-count);
}
