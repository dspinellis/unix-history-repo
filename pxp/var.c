#
/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"
#include "tree.h"

STATIC	int varcnt -1;
/*
 * Var declaration part
 */
varbeg(l)
	int l;
{

	line = l;
	if (nodecl)
		printoff();
	puthedr();
	putcm();
	ppnl();
	indent();
	ppkw("var");
	ppgoin(DECL);
	varcnt = 0;
}

var(vline, vidl, vtype)
	int vline;
	register int *vidl;
	int *vtype;
{

	putcm();
	setline(vline);
	ppitem();
	if (vidl != NIL)
		for (;;) {
			ppid(vidl[1]);
			vidl = vidl[2];
			if (vidl == NIL)
				break;
			ppsep(", ");
		}
	else
		ppid("{identifier list}");
	ppsep(":");
	gtype(vtype);
	ppsep(";");
	setinfo(vline);
	putcml();
	varcnt++;
}

varend()
{

	if (varcnt == -1)
		return;
	if (varcnt == 0)
		ppid("{variable decls}");
	ppgoout(DECL);
	varcnt = -1;
}
