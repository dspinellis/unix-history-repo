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

#include "../general.h"

#include "hostctlr.h"
#include "screen.h"
#include "ebc_disp.h"

#include "../system/globals.h"
#include "options.ext"
#include "../telnet.ext"
#include "inbound.ext"
#include "outbound.ext"
#include "../system/bsubs.ext"

#define SetHighestLowest(position) { \
					if (position < Lowest) { \
					    Lowest = position; \
					} \
					if (position > Highest) { \
					    Highest = position; \
					} \
				    }


static int	LastWasTerminated = 1;	/* was "control" = 1 last time? */

/* some globals */

#if	!defined(PURE3274)
int	OutputClock;		/* what time it is */
int	TransparentClock;		/* time we were last in transparent */
#endif	/* !defined(PURE3274) */

char CIABuffer[64] = {
    0x40, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
    0xc8, 0xc9, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
    0x50, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
    0xd8, 0xd9, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
    0x60, 0x61, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
    0xe8, 0xe9, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
    0xf8, 0xf9, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f
};

static struct orders_def orders_def[] = ORDERS_DEF;

/*
 * init_ctlr()
 *
 *	Initialize all data from the 'data' portion to their startup values.
 */

void
init_ctlr()
{
    LastWasTerminated = 1;
#if	!defined(PURE3274)
    OutputClock = TransparentClock = 0;
#endif	/* !defined(PURE3274) */
    init_inbound();
}


FieldInc(position)
register int	position;		/* Position in previous field */
{
    register ScreenImage *ptr;

    ptr = (ScreenImage *)memNSchr((char *)Host+position+1, ATTR_MASK,
			HighestScreen()-position, ATTR_MASK, sizeof Host[0]);
    if (ptr == 0) {
	ptr = (ScreenImage *)memNSchr((char *)Host+LowestScreen(), ATTR_MASK,
			position-LowestScreen(), ATTR_MASK, sizeof Host[0]);
	if (ptr == 0) {
	    return LowestScreen();
	}
    }
    return ptr-Host;
}

FieldDec(position)
int	position;
{
    register ScreenImage *ptr;

    ptr = (ScreenImage *)memNSchr((char *)(Host+position)-1, ATTR_MASK,
			position-LowestScreen(), ATTR_MASK, -sizeof Host[0]);
    if (ptr == 0) {
	ptr = (ScreenImage *)memNSchr((char *)Host+HighestScreen(), ATTR_MASK,
			HighestScreen()-position, ATTR_MASK, -sizeof Host[0]);
	if (ptr == 0) {
	    return LowestScreen();
	}
    }
    return ptr-Host;
}

/* Clear3270 - called to clear the screen */

void
Clear3270()
{
    ClearArray(Host);
    DeleteAllFields();		/* get rid of all fields */
    BufferAddress = SetBufferAddress(0,0);
    CursorAddress = SetBufferAddress(0,0);
    Lowest = LowestScreen();
    Highest = HighestScreen();
}

/* AddHost - called to add a character to the buffer.
 *	We use a macro in this module, since we call it so
 *	often from loops.
 *
 *	NOTE: It is a macro, so don't go around using AddHost(p, *c++), or
 *	anything similar.  (I don't define any temporary variables, again
 *	just for the speed.)
 */
void
AddHost(position, character)
int	position;
char	character;
{
#if	defined(SLOWSCREEN)
#   define	AddHostA(p,c)					\
    {								\
	if (IsStartField(p)) {					\
	    DeleteField(p);					\
	    Highest = HighestScreen();				\
	    Lowest = LowestScreen();				\
	    SetHighestLowest(p);				\
	}							\
	SetHost(p, c);						\
    }
#   define	AddHost(p,c)					\
    {								\
	if (c != GetHost(p)) {					\
	    SetHighestLowest(p);				\
	}							\
	AddHostA(p,c);						\
    }	/* end of macro of AddHost */
#else	/* defined(SLOWSCREEN) */
#   define	AddHost(p,c)					\
    {								\
	if (IsStartField(p)) {					\
	    DeleteField(p);					\
	    Highest = HighestScreen();				\
	    Lowest = LowestScreen();				\
	    SetHost(p, c);					\
	} else {						\
	    SetHost(p, c);					\
	    SetHighestLowest(p);				\
	}							\
    }	/* end of macro of AddHost */
#endif	/* defined(SLOWSCREEN) */

    AddHost(position, character);
}

/* returns the number of characters consumed */
int
DataFromNetwork(buffer, count, control)
register unsigned char	*buffer;		/* what the data is */
register int	count;				/* and how much there is */
int	control;				/* this buffer ended block? */
{
    int origCount;
    register int c;
    register int i;
    static int Command;
    static int Wcc;

    origCount = count;

    if (LastWasTerminated) {

	if (count < 2) {
	    if (count == 0) {
		ExitString(stderr, "Short count received from host!\n", 1);
		return(count);
	    }
	    Command = buffer[0];
	    switch (Command) {		/* This had better be a read command */
	    case CMD_READ_MODIFIED:
	    case CMD_SNA_READ_MODIFIED:
	    case CMD_SNA_READ_MODIFIED_ALL:
		OiaOnlineA(1);
		DoReadModified(Command);
		break;
	    case CMD_READ_BUFFER:
	    case CMD_SNA_READ_BUFFER:
		OiaOnlineA(1);
		DoReadBuffer();
		break;
	    default:
		{
		    char buffer[100];

		    sprintf(buffer,
			"Unexpected read command code 0x%x received.\n",
								    Command);
		    ExitString(stderr, buffer, 1);
		    break;
		}
	    }
	    return(1);			/* We consumed everything */
	}
	Command = buffer[0];
	Wcc = buffer[1];
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
	case CMD_ERASE_WRITE_ALTERNATE:
	case CMD_SNA_ERASE_WRITE:
	case CMD_SNA_ERASE_WRITE_ALTERNATE:
	    {
		int newlines, newcolumns;

		OiaOnlineA(1);
		OiaTWait(0);
		if ((Command == CMD_ERASE_WRITE)
				|| (Command == CMD_SNA_ERASE_WRITE)) {
		    newlines = 24;
		    newcolumns = 80;
		} else {
		    newlines = MaxNumberLines;
		    newcolumns = MaxNumberColumns;
		}
		if ((newlines != NumberLines)
				|| (newcolumns != NumberColumns)) {
			/*
			 * The LocalClearScreen() is really for when we
			 * are going from a larger screen to a smaller
			 * screen, and we need to clear off the stuff
			 * at the end of the lines, or the lines at
			 * the end of the screen.
			 */
		    LocalClearScreen();
		    NumberLines = newlines;
		    NumberColumns = newcolumns;
		    ScreenSize = NumberLines * NumberColumns;
		}
		Clear3270();
#if	!defined(PURE3274)
		if (TransparentClock == OutputClock) {
		    TransStop();
		}
#endif	/* !defined(PURE3274) */
		break;
	    }

	case CMD_ERASE_ALL_UNPROTECTED:
	case CMD_SNA_ERASE_ALL_UNPROTECTED:
	    OiaOnlineA(1);
	    OiaTWait(0);
	    CursorAddress = HighestScreen()+1;
	    for (i = LowestScreen(); i <= HighestScreen(); i = ScreenInc(i)) {
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
	    OiaSystemLocked(0);
	    TerminalIn();
	    break;
	case CMD_WRITE:
	case CMD_SNA_WRITE:
	    OiaOnlineA(1);
	    OiaTWait(0);
	    break;
	default:
	    {
		char buffer[100];

		sprintf(buffer,
			"Unexpected write command code 0x%x received.\n",
								Command);
		ExitString(stderr, buffer, 1);
		break;
	    }
	}

	count -= 2;			/* strip off command and wcc */
	buffer += 2;

    }
    LastWasTerminated = 0;		/* then, reset at end... */

    while (count) {
	count--;
	c = *buffer++;
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
		c = *buffer++;
		count--;
		if ( ! (IsStartField(BufferAddress) &&
					FieldAttributes(BufferAddress) == c)) {
		    SetHighestLowest(BufferAddress);
		    NewField(BufferAddress,c);
		}
		BufferAddress = ScreenInc(BufferAddress);
		break;
	    case ORDER_SBA:
		Ensure(2);
		i = buffer[0];
		c = buffer[1];
#if	!defined(PURE3274)
		if (!i && !c) { /* transparent write */
		    if (!control) {
			return(origCount-(count+1));
		    } else {
			TransparentClock = OutputClock;		/* clock next */
			TransOut(buffer+2, count-2);		/* output */
			SendToIBM();				/* ack block */
			TransparentClock = OutputClock+1;	/* clock next */
			buffer += count;
			count -= count;
		    }
		    break;
		}
#endif	/* !defined(PURE3274) */
		BufferAddress = Addr3270(i, c);
		buffer += 2;
		count -= 2;
		break;
	    case ORDER_IC:
		CursorAddress = BufferAddress;
		break;
	    /*
	     * XXX - PT is supposed to null fill the screen buffer
	     * under certain draconian conditions.
	     */
	    case ORDER_PT:
		i = BufferAddress;
		do {
		    if (IsStartField(i)) {
			if (!IsProtected(ScreenInc(i))) {
			    break;
			}
		    }
		    i = ScreenInc(i);
		} while (i != HighestScreen());
		BufferAddress = ScreenInc(i);
		break;
	    case ORDER_RA:
		Ensure(3);
		i = Addr3270(buffer[0], buffer[1]);
		c = buffer[2];
		if (c == ORDER_GE) {
		    Ensure(4);
		    c = buffer[3];
		    buffer += 4;
		    count -= 4;
		} else {
		    buffer += 3;
		    count -= 3;
		}
		do {
		    AddHost(BufferAddress, ebc_disp[c]);
		    BufferAddress = ScreenInc(BufferAddress);
		} while (BufferAddress != i);
		break;
	    case ORDER_EUA:    /* (from [here,there), ie: half open interval] */
		Ensure(2);
		/*
		 * Compiler error - msc version 4.0:
		 *			"expression too complicated".
		 */
		i = WhereAttrByte(BufferAddress);
		c = FieldAttributes(i);
		for (i = Addr3270(buffer[0], buffer[1]); i != BufferAddress;
				BufferAddress = ScreenInc(BufferAddress)) {
		    if (IsStartField(BufferAddress)) {
			c = FieldAttributes(BufferAddress);
		    } else if (!IsProtectedAttr(BufferAddress, c)) {
			AddHost(BufferAddress, 0);
		    }
		}
		buffer += 2;
		count -= 2;
		break;
	    case ORDER_GE:
		Ensure(2);
		/* XXX Should do SOMETHING! */
		buffer += 0;
		count -= 0;		/* For now, just use this character */
		break;
	    case ORDER_YALE:		/* special YALE defined order */
		Ensure(2);	/* need at least two characters */
		if (*buffer == 0x5b) {
		    i = OptOrder(buffer+1, count-1, control);
		    if (i == 0) {
			return(origCount-(count+1));	/* come here again */
		    } else {
			buffer += 1 + i;
			count  -= (1 + i);
		    }
		}
		break;
	    default:
		{
		    char buffer[100];
		    static struct orders_def unk_order
						= { 0, "??", "(unknown)" };
		    struct orders_def *porder = &unk_order;
		    int i;

		    for (i = 0; i <= highestof(orders_def); i++) {
			if (orders_def[i].code == c) {
			    porder = &orders_def[i];
			    break;
			}
		    }
		    sprintf(buffer,
			"Unsupported order '%s' (%s, 0x%x) received.\n",
			porder->long_name, porder->short_name, c);
		    ExitString(stderr, buffer, 1);
		    /*NOTREACHED*/
		}
	    }
	    if (count < 0) {
		count = 0;
	    }
	} else {
	    /* Data comes in large clumps - take it all */
	    i = BufferAddress;
#if	!defined(SLOWSCREEN)
	    AddHost(i, ebc_disp[c]);
#else	/* !defined(SLOWSCREEN) */
	    AddHostA(i, ebc_disp[c]);
	    SetHighestLowest(i);
#endif	/* !defined(SLOWSCREEN) */
	    i = ScreenInc(i);
	    c = *buffer;
	    while (count && !IsOrder(c)) {
#if	!defined(SLOWSCREEN)
		AddHost(i, ebc_disp[c]);
#else	/* !defined(SLOWSCREEN) */
		AddHostA(i, ebc_disp[c]);
#endif	/* !defined(SLOWSCREEN) */
		i = ScreenInc(i);
#if	defined(SLOWSCREEN)
		if (i == LowestScreen()) {
		    SetHighestLowest(HighestScreen());
		}
#endif	/* defined(SLOWSCREEN) */
		count--;
		buffer++;
		c = *buffer;
	    }
#if	defined(SLOWSCREEN)
	    SetHighestLowest(i);
#endif	/* defined(SLOWSCREEN) */
	    BufferAddress = i;
	}
    }
    if (count == 0) {
#if	!defined(PURE3274)
	OutputClock++;		/* time rolls on */
#endif	/* !defined(PURE3274) */
	if (control) {
	    if (Wcc & WCC_RESTORE) {
#if	!defined(PURE3274)
		if (TransparentClock != OutputClock) {
		    AidByte = 0;
		}
#else	/* !defined(PURE3274) */
		AidByte = 0;
#endif	/* !defined(PURE3274) */
		UnLocked = 1;
		OiaSystemLocked(0);
		TerminalIn();
	    }
	    if (Wcc & WCC_ALARM) {
		RingBell(0);
	    }
	}
	LastWasTerminated = control;	/* state for next time */
	return(origCount);
    } else {
	return(origCount-count);
    }
}

/*
 * Init3270()
 *
 * Initialize any 3270 (controller) variables to an initial state
 * in preparation for accepting a connection.
 */

void
Init3270()
{
    int i;

    OptInit();		/* initialize mappings */

    ClearArray(Host);

    ClearArray(Orders);
    for (i = 0; i <= highestof(orders_def); i++) {
	Orders[orders_def[i].code] = 1;
    }

    DeleteAllFields();		/* Clear screen */
    Lowest = HighestScreen()+1;
    Highest = LowestScreen()-1;
    CursorAddress = BufferAddress = SetBufferAddress(0,0);
    UnLocked = 1;
    OutputClock = 1;
    TransparentClock = -1;
}
