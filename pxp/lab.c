#
/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"

/*
 * Label declaration part
 */
label(r, l)
	int *r, l;
{
	register *ll;

	if (nodecl)
		printoff();
	puthedr();
	setline(l);
	ppnl();
	indent();
	ppkw("label");
	ppgoin(DECL);
	ppnl();
	indent();
	ppbra(NIL);
	ll = r;
	if (ll != NIL)
		for (;;) {
			pplab(ll[1]);
			ll = ll[2];
			if (ll == NIL)
				break;
			ppsep(", ");
		}
	else
		ppid("{label list}");
	ppket(";");
	putcml();
	ppgoout(DECL);
}

/*
 * Goto statement
 */
gotoop(s)
	char *s;
{

	gocnt++;
	ppkw("goto");
	ppspac();
	pplab(s);
}

/*
 * A label on a statement
 */
labeled(s)
	char *s;
{

	linopr();
	indentlab();
	pplab(s);
	ppsep(":");
}
