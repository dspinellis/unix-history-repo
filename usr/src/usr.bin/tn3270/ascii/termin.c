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
static	char	sccsid[] = "@(#)termin.c	3.1  10/29/86";
#endif	/* ndef lint */

/* this takes characters from the keyboard, and produces 3270 keystroke
	codes
 */

#include <stdio.h>
#include <ctype.h>

#include "../general/general.h"
#include "m4.out"		/* output of termcodes.m4 */
#include "state.h"

#include "../general/globals.h"
#include "../ctlr/function.h"
#include "../ctlr/inbound.ext"
#include "../ctlr/outbound.ext"
#include "../telnet.ext"
#include "termin.ext"

#define IsControl(c)	(!isprint(c) || (isspace(c) && ((c) != ' ')))

#define NextState(x)	(x->next)

/* XXX temporary - hard code in the state table */

#define MATCH_ANY 0xff			/* actually, match any character */

#include "astosc.out"

static char
	ourBuffer[100],		/* where we store stuff */
	*ourPHead = ourBuffer,	/* first character in buffer */
	*ourPTail = ourBuffer;	/* where next character goes */

static int InControl;
static int WaitingForSynch;

static state
	*headOfControl = 0;	/* where we enter code state table */

#define FullChar	((ourPTail+5) >= ourBuffer+sizeof ourBuffer)
#define EmptyChar	(ourPTail == ourPHead)


/*
 * init_keyboard()
 *
 * Initialize the keyboard variables.
 */

void
init_keyboard()
{
    ourPHead = ourPTail = ourBuffer;
    InControl = 0;
    WaitingForSynch = 0;
}


/*
 * Initialize the keyboard mapping file.
 */

void
InitMapping()
{
    extern state *InitControl();

    if (!headOfControl) {
	/* need to initialize */
	headOfControl = InitControl((char *)0, 0);
	if (!headOfControl) {		/* should not occur */
	    quit();
	}
    }
}


/* AddChar - put a character in our buffer */

static void
AddChar(c)
int	c;
{
    if (!FullChar) {
	if ((c >= numberof(asctosc)) || (c < 0)) {
	    ExitString(stderr,
		    "Unable to locate function in termout.c, AddChar()\n", 1);
	    /*NOTREACHED*/
	} else {
	    switch (asctosc[c].shift) {
	    case cantdo:
		if (c == ' ') {
		    *ourPTail++ = asctosc[TC_SPACE].scancode;
		} else {
		    RingBell("Keyboard not capable of function.");
		}
		break;
	    case normal:
		*ourPTail++ = asctosc[c].scancode;
		break;
	    case shifted:
		*ourPTail++ = asctosc[TC_MAKE_SHIFT].scancode;
		*ourPTail++ = asctosc[c].scancode;
		*ourPTail++ = asctosc[TC_BREAK_SHIFT].scancode;
		break;
	    case alted:
		*ourPTail++ = asctosc[TC_MAKE_ALT].scancode;
		*ourPTail++ = asctosc[c].scancode;
		*ourPTail++ = asctosc[TC_BREAK_ALT].scancode;
		break;
	    case shiftalted:
		*ourPTail++ = asctosc[TC_MAKE_SHIFT].scancode;
		*ourPTail++ = asctosc[TC_MAKE_ALT].scancode;
		*ourPTail++ = asctosc[c].scancode;
		*ourPTail++ = asctosc[TC_BREAK_ALT].scancode;
		*ourPTail++ = asctosc[TC_BREAK_SHIFT].scancode;
		break;
	    default:
		ExitString(stderr,
			    "Illegal 'shift' to AddChar() in termin.c\n", 1);
	    }
	}
    } else {
	RingBell("Typeahead buffer full");
    }
}

/* FlushChar - put everything where it belongs */

static void
FlushChar()
{
    ourPTail = ourBuffer;
    ourPHead = ourBuffer;
}

int
TerminalIn()
{
    /* send data from us to next link in stream */
    int count;

    count = 0;

    if (!EmptyChar) {			/* send up the link */
	count += DataFrom3270(ourPHead, ourPTail-ourPHead);
	ourPHead += count;
	if (EmptyChar) {
	    FlushChar();
	}
    }
	/* return value answers question: "did we do anything useful?" */
    return(count? 1:0);
}

int
DataFromTerminal(buffer, count)
register char	*buffer;		/* the data read in */
register int	count;			/* how many bytes in this buffer */
{
    register state *regControlPointer;
    register char c;
    register int result;
    int origCount;
    extern int bellwinup;
    static state *controlPointer;

    if (bellwinup) {
	BellOff();
    }

    origCount = count;

    while (count) {
	c = *buffer++&0x7f;
	count--;

	if (!InControl && !IsControl(c)) {
	    AddChar(c);			/* add ascii character */
	} else {
	    if (!InControl) {		/* first character of sequence */
		InControl = 1;
		controlPointer = headOfControl;
	    }
	    /* control pointer points to current position in state table */
	    for (regControlPointer = controlPointer; ;
			regControlPointer = NextState(regControlPointer)) {
		if (!regControlPointer) {	/* ran off end */
		    RingBell("Invalid control sequence");
		    regControlPointer = headOfControl;
		    InControl = 0;
		    break;
		}
		if ((regControlPointer->match == c) /* hit this character */
			|| (regControlPointer->match == MATCH_ANY)) {
		    result = regControlPointer->result;
		    if (result == TC_GOTO) {
			regControlPointer = regControlPointer->address;
			break;			/* go to next character */
		    }
		    if (WaitingForSynch) {
			if (result == TC_SYNCH) {
			    WaitingForSynch = 0;
			} else {
			    RingBell("Need to type synch character");
			}
		    }
		    else if (result == TC_FLINP) {
			FlushChar();		/* Don't add FLINP */
		    } else {
			if (result == TC_MASTER_RESET) {
			    FlushChar();
			}
			AddChar(result);		/* add this code */
		    }
		    InControl = 0;	/* out of control now */
		    break;
		}
	    }
	    controlPointer = regControlPointer;		/* save state */
	}
    }
    (void) TerminalIn();			/* try to send data */
    return(origCount-count);
}
