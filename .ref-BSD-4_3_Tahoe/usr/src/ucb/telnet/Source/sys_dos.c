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
static char sccsid[] = "@(#)sys_dos.c	1.3 (Berkeley) 3/8/88";
#endif /* not lint */

#if	defined(MSDOS)
#include <time.h>
#include <signal.h>
#include <process.h>
#include <fcntl.h>
#include <io.h>
#include <dos.h>
#include <ctype.h>

#include "externs.h"
#include "defines.h"

#if	!defined(SO_OOBINLINE)
#define	SO_OOBINLINE
#endif	/* !defined(SO_OOBINLINE) */



/*
 * MSDOS doesn't have anyway of deciding whether a full-edited line
 * is ready to be read in, so we need to do character-by-character
 * reads, and then do the editing in the program (in the case where
 * we are supporting line-by-line mode).
 *
 * The following routines, which are internal to the MSDOS-specific
 * code, accomplish this miracle.
 */

#define Hex(c)	HEX[(c)&0xff]

static survivorSetup = 0;		/* Do we have ^C hooks in? */

static int
	lineend = 0,		/* There is a line terminator */
	ctrlCCount = 0;

static char	linein[200],		/* Where input line is assembled */
		*nextin = linein,	/* Next input character */
		*nextout = linein;	/* Next character to be consumed */

static char
    savedInState, savedOutState;

#define consumechar() \
    if ((++nextout) >= nextin) { \
	nextout = nextin = linein; \
	lineend = 0; \
    }

#define	characteratatime()	(!MODE_LINE(globalmode))	/* one by one */


/*
 * killone()
 *
 *  Erase the last character on the line.
 */

static void
killone()
{
    if (lineend) {
	return;			/* ??? XXX */
    }
    if (nextin == linein) {
	return;			/* Nothing to do */
    }
    nextin--;
    if (!(isspace(*nextin) || isprint(*nextin))) {
	putchar('\b');
	putchar(' ');
	putchar('\b');
    }
    putchar('\b');
    putchar(' ');
    putchar('\b');
}


/*
 * setlineend()
 *
 *  Decide if it's time to send the current line up to the user
 * process.
 */

static void
setlineend()
{
    if (nextin == nextout) {
	return;
    }
    if (characteratatime()) {
	lineend = 1;
    } else if (nextin >= (linein+sizeof linein)) {
	lineend = 1;
    } else {
	int c = *(nextin-1);
	if ((c == termIntChar)
		|| (c == termQuitChar)
		|| (c == termEofChar)) {
	    lineend = 1;
	} else if (c == termFlushChar) {
	    lineend = 1;
	} else if ((c == '\n') || (c == '\r')) {
	    lineend = 1;
	}
    }
    /* Otherwise, leave it alone (reset by 'consumechar') */
}

/*
 * OK, what we do here is:
 *
 *   o  If we are echoing, then
 *	o  Look for character erase, line kill characters
 *	o  Echo the character (using '^' if a control character)
 *   o  Put the character in the input buffer
 *   o  Set 'lineend' as necessary
 */

static void
DoNextChar(c)
int	c;			/* Character to process */
{
    static char literalnextcharacter = 0;

    if (nextin >= (linein+sizeof linein)) {
	putchar('\7');		/* Ring bell */
	setlineend();
	return;
    }
    if (MODE_LOCAL_CHARS(globalmode)) {
	/* Look for some special character */
	if (!literalnextcharacter) {
	    if (c == termEraseChar) {
		killone();
		setlineend();
		return;
	    } else if (c == termKillChar) {
		while (nextin != linein) {
		    killone();
		}
		setlineend();
		return;
	    } else if (c == termLiteralNextChar) {
		literalnextcharacter = 1;
		return;
	    }
	}

	if (MODE_LOCAL_ECHO(globalmode)) {
	    if ((literalnextcharacter == 0) && ((c == '\r') || (c == '\n'))) {
		putchar('\r');
		putchar('\n');
		c = '\n';
	    } else if (!isprint(c) && !isspace(c)) {
		putchar('^');
		putchar(c^0x40);
	    } else {
		putchar(c);
	    }
	}
	literalnextcharacter = 0;
    }
    *nextin++ = c;
    setlineend();
}

static int
inputExists()
{
    int input;
    static state = 0;

    while (ctrlCCount) {
	DoNextChar(0x03);
	ctrlCCount--;
    }
    if (lineend) {
	return 1;
    }
#if	1	/* For BIOS variety of calls */
    if (kbhit() == 0) {
	return lineend;
    }
    input = getch();			/* MSC - get console character */
    if ((input&0xff) == 0) {
	DoNextChar(0x01);		/* ^A */
    } else {
	DoNextChar(input&0xff);
    }
#else	/* 0 */
    if ((input = dirconio()) == -1) {
	return lineend;
    }
    if ((input&0xff) == 0) {
	if ((input&0xff00) == 0x0300) {		/* Null */
	    DoNextChar(0);
	} else {
	    DoNextChar(0x01);
	    if (input&0x8000) {
		DoNextChar(0x01);
		DoNextChar((input>>8)&0x7f);
	    } else {
		DoNextChar((input>>8)&0xff);
	    }
	}
    } else {
	DoNextChar(input&0xff);
    }
#endif	/* 0 */
    return lineend;
}


void
CtrlCInterrupt()
{
    if (!MODE_COMMAND_LINE(globalmode)) {
	char far *Bios_Break = (char far *) (((long)0x40<<16)|0x71);

	ctrlCCount++;		/* XXX */
	signal(SIGINT, CtrlCInterrupt);
    } else {
	closeallsockets();
	exit(1);
    }
}

int
dosbinary(fd, onoff)
int	fd;
int	onoff;
{
    union REGS regs;
    int oldstate;

    /* Get old stuff */
    regs.h.ah = 0x44;
    regs.h.al = 0;
    regs.x.bx = fd;
    intdos(&regs, &regs);
    oldstate = regs.h.dl&(1<<5);		/* Save state */

    /* Set correct bits in new mode */
    regs.h.dh = 0;
    if (onoff) {
	regs.h.dl |= 1<<5;
    } else {
	regs.h.dl &= ~(1<<5);
    }

    /* Set in new mode */
    regs.h.ah = 0x44;
    regs.h.al = 1;
    regs.x.bx = fd;
    intdos(&regs, &regs);

    return oldstate;
}

/*
 * The MSDOS routines, called from elsewhere.
 */


int
TerminalAutoFlush()				/* MSDOS */
{
    return 1;
}

int
TerminalCanRead()
{
    return inputExists();
}


/*
 * Flush output to the terminal
 */
 
void
TerminalFlushOutput()				/* MSDOS */
{
}


void
TerminalNewMode(fd_in, fd_out, f)		/* MSDOS */
int	fd_in, fd_out;			/* File descriptors */
register int f;
{
    union REGS inregs;
    struct SREGS segregs;
    static old_1b_offset = 0, old_1b_segment = 0;

    globalmode = f;
    if (MODE_COMMAND_LINE(f)) {
	signal(SIGINT, SIG_DFL);
	if (old_1b_segment|old_1b_offset) {
	    inregs.h.ah = 0x25;
	    inregs.h.al = 0x1b;
	    inregs.x.dx = old_1b_offset;
	    segregs.ds = old_1b_segment;
	    intdosx(&inregs, &inregs, &segregs);
	    old_1b_segment = old_1b_offset = 0;
	}
	if (setmode(fd_out, O_TEXT) == -1) {
	    ExitPerror("setmode (text)", 1);
	}
	(void) dosbinary(fileno(stdout), 0);
	if (setmode(fd_out, O_TEXT) == -1) {
	    ExitPerror("setmode (text)", 1);
	}
	(void) dosbinary(fileno(stdin), 0);
    } else {
	signal(SIGINT, CtrlCInterrupt);
	if ((old_1b_segment|old_1b_offset) == 0) {
	    extern void iret_subr();
	    void (far *foo_subr)() = iret_subr;

	    inregs.h.ah = 0x35;
	    inregs.h.al = 0x1b;
	    intdosx(&inregs, &inregs, &segregs);
	    old_1b_segment = segregs.es;
	    old_1b_offset = inregs.x.bx;
	    inregs.h.ah = 0x25;
	    inregs.h.al = 0x1b;
	    inregs.x.dx = FP_OFF(foo_subr);
	    segregs.ds = FP_SEG(foo_subr);
	    intdosx(&inregs, &inregs, &segregs);
	}
	if (MODE_LOCAL_CHARS(f)) {
	    if (setmode(fd_out, O_TEXT) == -1) {
		ExitPerror("setmode (text)", 1);
	    }
	    (void) dosbinary(fileno(stdout), 0);
	    if (setmode(fd_in, O_TEXT) == -1) {
		ExitPerror("setmode (text)", 1);
	    }
	    (void) dosbinary(fileno(stdin), 0);
	} else {
	    if (setmode(fd_out, O_BINARY) == -1) {
		ExitPerror("setmode (binary)", 1);
	    }
	    (void) dosbinary(fileno(stdout), 1);
	    if (setmode(fd_in, O_BINARY) == -1) {
		ExitPerror("setmode (binary)", 1);
	    }
	    (void) dosbinary(fileno(stdin), 1);
	}
    }
}

int
TerminalRead(fd, buffer, count)
int	fd;
char	*buffer;
int	count;
{
    int done = 0;

    for (;;) {
	while (inputExists() && (done < count)) {
	    *buffer++ = *nextout;
	    consumechar();
	    done++;
	}
	if (done) {
	    return(done);
	} else {
	    return 0;
	}
    }
}


void
TerminalSaveState()				/* MSDOS */
{
    termEofChar = '\4';
    termEraseChar = '\10';
    termFlushChar = '\17';
    termIntChar = '\3';
    termKillChar = '\25';
    termLiteralNextChar = '\26';
    termQuitChar = '\0';;

    savedInState = dosbinary(fileno(stdin), 0);
    savedOutState = dosbinary(fileno(stdout), 0);
}

int
TerminalSpecialChars(c)			/* MSDOS */
{
    return 1;
}


void
TerminalRestoreState()				/* MSDOS */
{
    (void) dosbinary(fileno(stdin), savedInState);
    (void) dosbinary(fileno(stdout), savedOutState);
}


int
TerminalWrite(fd, buffer, count)		/* MSDOS */
int	fd;
char	*buffer;
int	count;
{
    return fwrite(buffer, sizeof (char), count, stdout);	/* XXX */
}


int
NetClose(fd)
{
    return closesocket(fd);
}

void
NetNonblockingIO(fd, onoff)				/* MSDOS */
int
	fd,
	onoff;
{
    if (SetSockOpt(fd, SOL_SOCKET, SO_NONBLOCKING, onoff)) {
	perror("setsockop (SO_NONBLOCKING) ");
	ExitString(stderr, "exiting\n", 1);
    }
}

void
NetSigIO(fd)				/* MSDOS */
int fd;
{
}

void
NetSetPgrp(fd)				/* MSDOS */
int fd;
{
}


#endif	/* defined(MSDOS) */
