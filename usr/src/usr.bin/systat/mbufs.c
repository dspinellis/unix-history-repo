#ifndef lint
static char sccsid[] = "@(#)mbufs.c	1.1 (Lucasfilm) %G%";
#endif

#include "systat.h"
#include <sys/mbuf.h>

#define	X_MBSTAT	16

struct	mbstat *mb;

initmbufs()
{

	if (mb == 0)
		mb = (struct mbstat *)calloc(1, sizeof (*mb));
}

fetchmbufs()
{

	lseek(kmem, nlst[X_MBSTAT].n_value, L_SET);
	read(kmem, mb, sizeof (*mb));
}

labelmbufs()
{

        move(5, 0);
        clrtoeol();
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
