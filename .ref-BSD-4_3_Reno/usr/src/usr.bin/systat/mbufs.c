/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)mbufs.c	5.3 (Berkeley) 5/11/89";
#endif not lint

#include "systat.h"
#include <sys/mbuf.h>
#include <paths.h>

WINDOW *
openmbufs()
{
	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

closembufs(w)
	WINDOW *w;
{
	if (w == NULL)
		return;
	wclear(w);
	wrefresh(w);
	delwin(w);
}

struct	mbstat *mb;

labelmbufs()
{
	wmove(wnd, 0, 0); wclrtoeol(wnd);
	mvwaddstr(wnd, 0, 10,
	    "/0   /5   /10  /15  /20  /25  /30  /35  /40  /45  /50  /55  /60");
}

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

static struct nlist nlst[] = {
#define	X_MBSTAT	0
	{ "_mbstat" },
	{ "" }
};

initmbufs()
{
	if (nlst[X_MBSTAT].n_type == 0) {
		nlist(_PATH_UNIX, nlst);
		if (nlst[X_MBSTAT].n_type == 0) {
			error("namelist on %s failed", _PATH_UNIX);
			return(0);
		}
	}
	if (mb == 0)
		mb = (struct mbstat *)calloc(1, sizeof (*mb));
	return(1);
}

fetchmbufs()
{
	if (nlst[X_MBSTAT].n_type == 0)
		return;
	lseek(kmem, nlst[X_MBSTAT].n_value, L_SET);
	read(kmem, mb, sizeof (*mb));
}
