#ifndef lint
static char sccsid[] = "@(#)1.init.c	4.1	(Berkeley)	%G%";
#endif not lint

#include <stdio.h>
#include "1.defs.h"
#include  "1.incl.h"
#include "def.h"


prog_init()
	{
	endline = endcom = 0;	endchar = -1;
	comchar = -1;
	graph = challoc(sizeof(*graph) * maxnode);
	}

routinit()
	{
	graf_init();
	progtype = !sub;
	routbeg = endline + 1;
	rtnbeg = endchar + 1;
	nameline = 0;
	stflag = UNDEFINED;
	}
line_init()
	{
	struct lablist *makelab();
	freelabs();
	newlab = linelabs = makelab(0L);
	flag = counter = nlabs = lswnum = swptr = p1 = 0;
	p3 = 5;
	endcom = endline;
	comchar = endchar;
	begline = endline + 1;	begchar = endchar + 1;
	reflab = endlab = errlab = 0;
	r1 = r2 = 0;
	}
graf_init()
	{
	int arctype[3];  long arclab[3];
	nodenum = 0;
	doptr = UNDEFINED;
	retvert = stopvert = UNDEFINED;
	ENTLST = FMTLST = 0;

	
	arctype[0] = -2;  arclab[0] = implicit;
	START = makenode(DUMVX,FALSE,FALSE,implicit,1,arctype,arclab);
	}

