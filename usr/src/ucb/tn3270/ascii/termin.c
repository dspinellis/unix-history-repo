/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)termin.c	3.2 (Berkeley) 3/28/88";
#endif /* not lint */

/* this takes characters from the keyboard, and produces 3270 keystroke
	codes
 */

#include <stdio.h>
#include <ctype.h>

#include "../general/general.h"
#include "../ctlr/function.h"
#include "../ctlr/inbound.ext"
#include "../ctlr/outbound.ext"
#include "../telnet.ext"
#include "termin.ext"

#include "../api/astosc.h"
#include "state.h"

#include "../general/globals.h"

#define IsControl(c)	(!isprint(c) || (isspace(c) && ((c) != ' ')))

#define NextState(x)	(x->next)

/* XXX temporary - hard code in the state table */

#define MATCH_ANY 0xff			/* actually, match any character */


static unsigned char
	ourBuffer[100],		/* where we store stuff */
	*ourPHead = ourBuffer,	/* first character in buffer */
	*ourPTail = ourBuffer,	/* where next character goes */
	*TransPointer = 0;	/* For transparent mode data */

static int InControl;
static int WaitingForSynch;

static struct astosc
	*spacePTR = 0;		/* Space is hard to enter */

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
    register struct astosc *ptr;

    if (!headOfControl) {
	/* need to initialize */
	headOfControl = InitControl((char *)0, 0, ascii_to_index);
	if (!headOfControl) {		/* should not occur */
	    quit();
	}
	for (ptr = &astosc[0]; ptr <= &astosc[highestof(astosc)]; ptr++) {
	    if (ptr->function == FCN_SPACE) {
		spacePTR = ptr;
	    }
	}
    }
}


/* AddChar - put a function index in our buffer */

static void
AddChar(c)
int	c;
{
    if (!FullChar) {
	*ourPTail++ = c;
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

void
TransInput(onoff, mode)
int	mode;			/* Which KIND of transparent input */
int	onoff;			/* Going in, or coming out */
{
    if (onoff) {
	/* Flush pending input */
	FlushChar();
	TransPointer = ourBuffer;
    } else {
    }
}

int
TerminalIn()
{
    /* send data from us to next link in stream */
    int work = 0;
    register struct astosc *ptr;

    while (!EmptyChar) {			/* send up the link */
	if (*ourPHead == ' ') {
	    ptr = spacePTR;
	} else {
	    ptr = &astosc[*ourPHead];
	}
	if (AcceptKeystroke(ptr->scancode, ptr->shiftstate) == 1) {
	    ourPHead++;
	    work = 1;
	} else {
	    break;
	}
    }

    if (EmptyChar) {
	FlushChar();
    }
	/* return value answers question: "did we do anything useful?" */
    return work;
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

    if (TransPointer) {
	int i;

	if ((count+TransPointer) >= (ourBuffer+sizeof ourBuffer)) {
	    i = ourBuffer+sizeof ourBuffer-TransPointer;
	} else {
	    i = count;
	}
	while (i--) {
	    c = (*buffer++)&0x7f;
	    *TransPointer++ = c|0x80;
	    if (c == '\r') {
		SendTransparent(ourBuffer, TransPointer-ourBuffer);
		TransPointer = 0;		/* Done */
		break;
	    }
	}
	return count;
    }

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
		    count = 0;		/* Flush current input */
		    break;
		}
		if ((regControlPointer->match == c) /* hit this character */
			|| (regControlPointer->match == MATCH_ANY)) {
		    result = regControlPointer->result;
		    if (result == STATE_GOTO) {
			regControlPointer = regControlPointer->address;
			break;			/* go to next character */
		    }
		    if (WaitingForSynch) {
			if (astosc[result].function == FCN_SYNCH) {
			    WaitingForSynch = 0;
			} else {
			    RingBell("Need to type synch character");
			}
		    }
		    else if (astosc[result].function == FCN_FLINP) {
			FlushChar();		/* Don't add FLINP */
		    } else {
			if (astosc[result].function == FCN_MASTER_RESET) {
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
