/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: erro.c,v 2.5 85/08/22 16:02:02 timo Exp $";

/*
 * B editor -- Handle error messages.
 */

#include "b.h"
#include "feat.h"
#include "node.h"


extern bool hushbaby;
extern bool dflag;

string querepr();

extern int winheight; /* From scrn.c */
extern int winstart; /* From scrn.c */
extern int llength; /* From scrn.c */

#define SOBIT 0200 /* Interface with wind.c */

#define MAXMSG 1000

static char msgbuffer[MAXMSG];
static bool ringbell;
static int priority;


/*
 * Status line.  A combination of scroll bar, error message etc.
 * Put the message on the screen and clear the buffers for next time.
 * If there is no message, show status and copy buffer and recording mode.
 */

Visible Procedure
stsline(totlines, topline, scrlines, copybuffer, recording)
	int totlines;
	int topline;
	int scrlines;
	value copybuffer;
	bool recording;
{
	register string bp;

	if (ringbell && !hushbaby)
		trmbell();
	if (msgbuffer[0]) {
		msgbuffer[llength-1] = '\0'; /* Truncate */
		if (ringbell) {
			for (bp = msgbuffer; *bp; ++bp)
				*bp |= SOBIT;
		}
	}
	else {
		bp = msgbuffer;
#ifdef SCROLLBAR
		bp += addscrollbar(totlines, topline, scrlines);
#endif SCROLLBAR
		if (recording) {
			strcpy(bp, "[Recording] ");
			bp += (sizeof "[Recording] ") - 1;
		}
		if (copybuffer) {
#ifdef SHOWBUF
			sprintf(bp, "[Copy buffer: %.80s]",
				querepr(copybuffer));
			while (*bp)
				++bp;
			if (bp >= msgbuffer+80)
				strcpy(msgbuffer+75, "...]");
#else !SHOWBUF
			strcpy(bp, "[Copy buffer]");
#endif !SHOWBUF
		}
	}
	trmputdata(winheight, winheight, 0, msgbuffer);
	msgbuffer[0] = 0;
	priority = 0;
	ringbell = No;
}

#ifdef SCROLLBAR

/*
 * Paint a beautiful scroll bar so the user can see about what part of the
 * unit is visible on the screen (considering logical lines).
 */

Hidden int
addscrollbar(totlines, topline, scrlines)
	int totlines;
	int topline;
	int scrlines;
{
	int endline;
	register int i;

	if (winstart > 0 || scrlines > totlines)
		return 0; /* Nothing outside screen */
	if (totlines <= 0)
		totlines = 1; /* Don't want to divide by 0 */
	topline = topline*winheight / totlines;
	endline = topline + (scrlines*winheight + totlines-1) / totlines;
	if (endline > winheight)
		endline = winheight;
	if (topline >= endline)
		topline = endline-1;
	for (i = 0; i < topline; ++i)
		msgbuffer[i] = '-';
	for (; i < endline; ++i)
		msgbuffer[i] = '#';
	for (; i < winheight; ++i)
		msgbuffer[i] = '-';
	msgbuffer[i++] = ' ';
	msgbuffer[i] = '\0';
	return i;
}

#endif SCROLLBAR

/*
 * Issue an error message.  These have highest priority.
 * Once an error message is in the buffer, further error messages are ignored
 * until it has been displayed.
 */

/* VARARGS 1 */
Visible Procedure
error(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	string fmt;
{
	ringbell = Yes;
	if (fmt && priority < 3) {
		priority = 3;
		sprintf(msgbuffer, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
	}
}


/*
 * Issue an informative message.  These have medium priority.
 * Unlike error messages, the last such message is displayed.
 */

/* VARARGS 1 */
Visible Procedure
message(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	register string fmt;
{
	if (fmt && priority <= 2) {
		priority = 2;
		sprintf(msgbuffer, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
	}
}


/*
 * Issue a debugging message.  These  have lowest priority and
 * are not shown to ordinary users.
 */

/* VARARGS1 */
Visible Procedure
debug(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
	string fmt;
{
#ifndef NDEBUG
	if (fmt && priority <= 1) {
		priority = 1;
		sprintf(msgbuffer,
			fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
	}
#endif NDEBUG
}


/*
 * Dump any error message still remaining to stderr.
 */

Visible Procedure
enderro()
{
	if (msgbuffer[0]) {
		fprintf(stderr, "%s\n", msgbuffer);
	}
	msgbuffer[0] = 0;
	priority = 0;
	ringbell = No;
}


/*
 * This #define causes "erro.h" to compile a table of error messages.
 */

#define _ERROR(name, message) char name[] = message

#include "erro.h"
