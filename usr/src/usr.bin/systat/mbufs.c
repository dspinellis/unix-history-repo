/*-
 * Copyright (c) 1980, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)mbufs.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/mbuf.h>

#include <stdlib.h>
#include <string.h>
#include <nlist.h>
#include <paths.h>
#include "systat.h"
#include "extern.h"

static struct mbstat *mb;

char *mtnames[] = {
	"free",
	"data",
	"headers",
	"sockets",
	"pcbs",
	"routes",
	"hosts",
	"arps",
	"socknames",
	"zombies",
	"sockopts",
	"frags",
	"rights",
	"ifaddrs",
};

#define	NNAMES	(sizeof (mtnames) / sizeof (mtnames[0]))

WINDOW *
openmbufs()
{
	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

void
closembufs(w)
	WINDOW *w;
{
	if (w == NULL)
		return;
	wclear(w);
	wrefresh(w);
	delwin(w);
}

void
labelmbufs()
{
	wmove(wnd, 0, 0); wclrtoeol(wnd);
	mvwaddstr(wnd, 0, 10,
	    "/0   /5   /10  /15  /20  /25  /30  /35  /40  /45  /50  /55  /60");
}

void
showmbufs()
{
	register int i, j, max, index;
	char buf[10];

	if (mb == 0)
		return;
	for (j = 0; j < wnd->_maxy; j++) {
		max = 0, index = -1; 
		for (i = 0; i < wnd->_maxy; i++)
			if (mb->m_mtypes[i] > max) {
				max = mb->m_mtypes[i];
				index = i;
			}
		if (max == 0)
			break;
		if (j > NNAMES)
			mvwprintw(wnd, 1+j, 0, "%10d", index);
		else
			mvwprintw(wnd, 1+j, 0, "%-10.10s", mtnames[index]);
		wmove(wnd, 1 + j, 10);
		if (max > 60) {
			sprintf(buf, " %d", max);
			max = 60;
			while (max--)
				waddch(wnd, 'X');
			waddstr(wnd, buf);
		} else {
			while (max--)
				waddch(wnd, 'X');
			wclrtoeol(wnd);
		}
		mb->m_mtypes[index] = 0;
	}
	wmove(wnd, 1+j, 0); wclrtobot(wnd);
}

static struct nlist namelist[] = {
#define	X_MBSTAT	0
	{ "_mbstat" },
	{ "" }
};

int
initmbufs()
{
	if (namelist[X_MBSTAT].n_type == 0) {
		if (kvm_nlist(kd, namelist)) {
			nlisterr(namelist);
			return(0);
		}
		if (namelist[X_MBSTAT].n_type == 0) {
			error("namelist on %s failed", _PATH_UNIX);
			return(0);
		}
	}
	if (mb == 0)
		mb = (struct mbstat *)calloc(1, sizeof (*mb));
	return(1);
}

void
fetchmbufs()
{
	if (namelist[X_MBSTAT].n_type == 0)
		return;
	NREAD(X_MBSTAT, mb, sizeof (*mb));
}
