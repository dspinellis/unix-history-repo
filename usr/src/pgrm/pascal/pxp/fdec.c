/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)fdec.c	5.2 (Berkeley) 3/17/87";
#endif not lint

/*
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "tree.h"

/*
 * Program, procedure or function "header", i.e.:
 *
 *	function sin: real;
 */
funchdr(r)
	int *r;
{
	register **rl, *il;

	if (inpflist(r[2])) {
		optstk['z'-'a'] <<= 1;
		optstk['z'-'a'] |= opts['z'-'a'];
		opts['z'-'a'] = 1;
	}
	cbn++;
	lastbn = cbn;
	getcnt();
	if (nojunk && !inpflist(r[2]))
		setprint();
	else
		printon();
	if (r[0] == T_PROG && noinclude && bracket)
		printoff();
	if (cbn > 1 && !justify)
		ppgoin(PRFN);
	puthedr();
	if (noblank(setline(r[1])))
		ppnl();
	cnttab(r[2], pfcnt++);
	ppnl();
	indent();
	switch (r[0]) {
		case T_PROG:
			ppkw("program");
			break;
		case T_PDEC:
			ppkw("procedure");
			break;
		case T_FDEC:
			ppkw("function");
			break;
		default:
			panic("funchdr");
	}
	ppspac();
	ppid(r[2]);
	if (r[0] != T_PROG) {
		rl = r[3];
		if (rl != NIL) {
			ppbra("(");
			for (;;) {
				if (rl[1] == NIL) {
					rl = rl[2];
					continue;
				}
				switch (rl[1][0]) {
					case T_PVAR:
						ppkw("var");
						ppspac();
						break;
					case T_PPROC:
						ppkw("procedure");
						ppspac();
						break;
					case T_PFUNC:
						ppkw("function");
						ppspac();
						break;
				}
				il = rl[1][1];
				if (il != NIL)
					for (;;) {
						ppid(il[1]);
						il = il[2];
						if (il == NIL)
							break;
						ppsep(", ");
					}
				else
					ppid("{identifier list}");
				if (rl[1][0] != T_PPROC) {
					ppsep(":");
					gtype(rl[1][2]);
				}
				rl = rl[2];
				if (rl == NIL)
					break;
				ppsep(";");
				ppspac();
			}
			ppket(")");
		}
		if (r[0] == T_FDEC && r[4] != NIL) {
			ppsep(":");
			gtype(r[4]);
		}
		ppsep(";");
	} else {
		rl = r[3];
		if (rl != NIL) {
			ppbra("(");
			for (;;) {
				ppid(rl[1]);
				rl = rl[2];
				if (rl == NIL)
					break;
				ppsep(", ");
			}
			ppket(")");
		}
		ppsep(";");
	}
fhout:
	setline(r[1]);
	putcml();
	savecnt(&pfcnts[cbn]);
	setprint();
	--cbn;
	if (cbn && !justify)
		ppgoout(PRFN);
	return (r[2]);
}

/*
 * Forward declaration i.e. the second line of
 *
 *	procedure fum(var i: integer);
 *	    forward;
 */
funcfwd(fp)
	char *fp;
{

	baroff();
	ppgoin(DECL);
	ppnl();
	indent();
	ppkw("forward");
	ppsep(";");
	ppgoout(DECL);
	baron();
	return (fp);
}

/*
 * The "body" of a procedure, function, or program declaration,
 * i.e. a non-forward definition encounter.
 */
funcbody(fp)
	char *fp;
{

	if (cbn && !justify)
		ppgoin(PRFN);
	cbn++;
	lastbn = cbn;
	return (fp);
}

/*
 * The guts of the procedure, function or program, i.e.
 * the part between the begin and the end.
 */
funcend(fp, bundle, binfo)
	char *fp;
	int *bundle, *binfo;
{
	int *blk;
	extern int cntstat;

	cntstat = 0;
	blk = bundle[2];
	rescnt(&pfcnts[cbn]);
	setprint();
	if (cbn == 1 && noinclude && bracket)
		printoff();
	if (lastbn > cbn)
		unprint();
	if (cbn == 1)
		puthedr();
	if (noblank(setline(bundle[1])) && lastbn > cbn)
		ppnl();
	ppnl();
	indent();
	ppkw("begin");
	setline(bundle[1]);
	if (putcml() == 0 && lastbn > cbn)
		ppsname(fp);
	ppgoin(DECL);
	statlist(blk);
	setinfo(bundle[1]);
	putcmp();
	ppgoout(DECL);
	ppnl();
	indent();
	ppkw("end");
	ppsep(cbn == 1 ? "." : ";");
	setinfo(binfo);
	if (putcml() == 0)
		ppsname(fp);
	cbn--;
	if (cbn && !justify)
		ppgoout(PRFN);
	if (inpflist(fp)) {
		opts['z'-'a'] = optstk['z'-'a'] & 1;
		optstk['z'-'a'] >>= 1;
	}
	if (cbn == 0) {
		flushcm();
		printon();
		ppnl();
	}
}

ppsname(fp)
	char *fp;
{
	if (fp == NIL)
		return;
	ppsep(" { ");
	ppid(fp);
	ppsep(" }");
}

/*
 * Segend is called at the end of a routine segment (a separately
 * compiled segment that is not the main program). Since pxp only works
 * with a single pascal file, this routine should never be called.
 */
segend()
{

	if ( profile ) {
	    error("Missing program statement and program body");
	}
}

/*
 * External declaration i.e. the second line of
 *
 *	procedure fum(var i: integer);
 *	    external;
 */
struct nl *
funcext(fp)
	struct nl *fp;
{

	baroff();
	ppgoin(DECL);
	ppnl();
	indent();
	ppkw("external");
	ppsep(";");
	ppgoout(DECL);
	baron();
	return (fp);
}
