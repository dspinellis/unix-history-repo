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

STATIC	int constcnt -1;

/*
 * The const declaration part
 */
constbeg(l)
	int l;
{

	line = l;
	if (nodecl)
		printoff();
	puthedr();
	putcm();
	ppnl();
	indent();
	ppkw("const");
	ppgoin(DECL);
	constcnt = 0;
}

const(cline, cid, cdecl)
	int cline;
	char *cid;
	int *cdecl;
{

	putcm();
	setline(cline);
	ppitem();
	ppid(cid);
	ppsep(" = ");
	gconst(cdecl);
	ppsep(";");
	constcnt++;
	setinfo(cline);
	putcml();
}

constend()
{

	if (constcnt == -1)
		return;
	if (nodecl)
		return;
	if (constcnt == 0)
		ppid("{const decls}");
	ppgoout(DECL);
	constcnt = -1;
}

/*
 * A constant in an expression
 * or a declaration.
 */
gconst(r)
	int *r;
{
	register *cn;

	cn = r;
loop:
	if (cn == NIL) {
		ppid("{constant}");
		return;
	}
	switch (cn[0]) {
		default:
			panic("gconst");
		case T_PLUSC:
			ppop("+");
			cn = cn[1];
			goto loop;
		case T_MINUSC:
			ppop("-");
			cn = cn[1];
			goto loop;
		case T_ID:
			ppid(cn[1]);
			return;
		case T_CBINT:
		case T_CINT:
		case T_CFINT:
			ppnumb(cn[1]);
			if (cn[0] == T_CBINT)
				ppsep("b");
			return;
		case T_CSTRNG:
			ppstr(cn[1]);
			return;
	}
}
