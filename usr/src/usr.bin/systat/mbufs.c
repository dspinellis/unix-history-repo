#ifndef lint
static char sccsid[] = "@(#)mbufs.c	1.3 (Lucasfilm) %G%";
#endif

#include "systat.h"
#include <sys/param.h>
#include <sys/mbuf.h>
#include <sys/file.h>
#include <nlist.h>

WINDOW *
openmbufs()
{
	static WINDOW *w = NULL;

	if (w == NULL)
        	w = newwin(20, 70, 3, 5);
	return (w);
}

closembufs(w)
	WINDOW *w;
{

	if (w == NULL)
		return;
	move(5, 0);
	clrtobot();
	wclear(w);
	wrefresh(w);
}

struct	mbstat *mb;

labelmbufs()
{

        move(5, 0); clrtoeol();
        mvaddstr(5, 20,
                "/0   /5   /10  /15  /20  /25  /30  /35  /40  /45  /50");
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
};

showmbufs()
{
	register int i, j, max, index;
	char buf[10];

	if (mb == 0)
		return;
	for (j = 0; j < 15; j++) {
		max = 0, index = -1; 
		for (i = 0; i < 15; i++)
			if (mb->m_mtypes[i] > max) {
				max = mb->m_mtypes[i];
				index = i;
			}
		if (max == 0)
			break;
		wmove(wnd, 3 + j, 0);
		waddstr(wnd, mtnames[index]);
		wmove(wnd, 3 + j, 15);
		if (max > 50) {
			sprintf(buf, " %d", max);
			max = 50;
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
	while (j++ < 15) {
		wmove(wnd, 3 + j, 0);
		wclrtoeol(wnd);
	}
}

static struct nlist nlst[] = {
#define	X_MBSTAT	0
	{ "_mbstat" },
        { "" }
};

initmbufs()
{

	if (nlst[X_MBSTAT].n_type == 0) {
		nlist("/vmunix", nlst);
		if (nlst[X_MBSTAT].n_type == 0) {
			error("namelist on /vmunix failed");
			return;
		}
	}
	if (mb == 0)
		mb = (struct mbstat *)calloc(1, sizeof (*mb));
}

fetchmbufs()
{

	if (nlst[X_MBSTAT].n_type == 0)
		return;
	lseek(kmem, nlst[X_MBSTAT].n_value, L_SET);
	read(kmem, mb, sizeof (*mb));
}
