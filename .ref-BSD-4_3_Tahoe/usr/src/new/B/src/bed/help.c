/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
static char rcsid[] = "$Header: help.c,v 2.6 85/08/22 16:03:38 timo Exp $";

/*
 * B editor -- Print help blurb.
 */

#include "feat.h"

#ifdef HELPFUL

#include "unix.h"

#ifdef IBMPC
#define getchar() getch()
#endif IBMPC

#ifdef SIGNAL
#include <signal.h>
#endif SIGNAL

#ifdef SGTTY_H
#include <sgtty.h>
#endif SGTTY_H

#include "b.h"

string unixerror();

#ifndef HELPBLURB
#define HELPBLURB "/usr/new/lib/B/Bed_help"
#endif

#define SOBIT 0200

extern int winheight;
extern int llength;
extern int winstart;


/*
 * Interrupt catcher while printing message.
 */

Hidden int intr;

#ifdef SIGNAL
Hidden Procedure
stop()
{
	intr = Yes;
	signal(SIGINT, stop);
}
#endif SIGNAL


/*
 * Print help blurb.
 * This is done through the standard screen interface.
 * An interrupt during the printing only stops the printing.
 * The user must type [return] to continue.
 */

Visible bool
help()
{
	FILE *fp;
	string helpblurb;
	char buffer[81];
	int len = sizeof buffer;
	string cp;
#ifdef SIGNAL
	int (*prevsig)();
#endif SIGNAL
#ifdef SGTTY_H
	struct sgttyb tty;
#endif SGTTY_H
	int c;
	
	helpblurb = getenv("BED_HELP");
	if (!helpblurb || !helpblurb[0])
		helpblurb = HELPBLURB;
	fp = fopen(helpblurb, "r");
	if (!fp) {
		error("Sorry, I can't help [%s]", unixerror(helpblurb));
		return No;
	}
#ifdef SIGNAL
	intr = No;
	prevsig = signal(SIGINT, stop);
#endif SIGNAL
	if (llength < (sizeof buffer)-1)
		len = llength+1;
	while (!intr && fgets(buffer, len, fp)) {
		cp = index(buffer, '\n');
		if (cp)
			*cp = '\0';
		trmputdata(winheight, winheight, 0, buffer);
		trmscrollup(0, winheight, 1);
		trmsync(winheight, 0);
	}
	fclose(fp);
#ifdef SIGNAL
	signal(SIGINT, prevsig);
	if (intr)
		trmundefined();
		/* UNIX discards output when interrupted */
		/* so output position is uncertain */
#endif SIGNAL
	trmputdata(winheight, winheight, 0, "");
	strcpy(buffer, "Press [return] to continue");
	for (cp = buffer; *cp; )
		*cp++ |= SOBIT;
	trmputdata(winheight, winheight, 0, buffer);
	trmsync(winheight, cp - buffer);
	c = getchar();
	while (c != '\n' && c != '\r' && c != EOF) {
		trmbell();
		c = getchar();
	}
	trmputdata(winheight, winheight, 0, "");
	winstart = winheight;
	return Yes;
}
#endif HELPFUL
