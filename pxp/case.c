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

/*
 * Case statement
 */
caseop(r)
	int *r;
{
	register *cl, *cs, i;
	struct pxcnt scnt;

	savecnt(&scnt);
	ppkw("case");
	ppspac();
	rvalue(r[2], NIL);
	ppspac();
	ppkw("of");
	for (cl = r[3]; cl != NIL;) {
		cs = cl[1];
		if (cs == NIL)
			continue;
		baroff();
		ppgoin(DECL);
		setline(cs[1]);
		ppnl();
		indent();
		ppbra(NIL);
		cs = cs[2];
		if (cs != NIL) {
			i = 0;
			for (;;) {
				gconst(cs[1]);
				cs = cs[2];
				if (cs == NIL)
					break;
				i++;
				if (i == 7) {
					ppsep(",");
					ppitem();
					i = 0;
				} else
					ppsep(", ");
			}
		} else
			ppid("{case label list}");
		ppket(":");
		cs = cl[1];
		cs = cs[3];
		getcnt();
		ppgoin(STAT);
		if (cs != NIL && cs[0] == T_BLOCK) {
			ppnl();
			indent();
			baron();
			ppstbl1(cs, STAT);
			baroff();
			ppstbl2();
			baron();
		} else {
			baron();
			statement(cs);
		}
		ppgoout(STAT);
		ppgoout(DECL);
		cl = cl[2];
		if (cl == NIL)
			break;
		ppsep(";");
	}
	if (rescnt(&scnt))
		getcnt();
	ppnl();
	indent();
	ppkw("end");
}
