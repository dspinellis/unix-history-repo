/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)sparc.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include	"gprof.h"

    /*
     *	a namelist entry to be the child of indirect calls
     */
nltype	indirectchild = {
	"(*)" ,				/* the name */
	(unsigned long) 0 ,		/* the pc entry point */
	(unsigned long) 0 ,		/* entry point aligned to histogram */
	(double) 0.0 ,			/* ticks in this routine */
	(double) 0.0 ,			/* cumulative ticks in children */
	(long) 0 ,			/* how many times called */
	(long) 0 ,			/* times called by live arcs */
	(long) 0 ,			/* how many calls to self */
	(double) 1.0 ,			/* propagation fraction */
	(double) 0.0 ,			/* self propagation time */
	(double) 0.0 ,			/* child propagation time */
	(short) 0 ,			/* print flag */
	(short) 0 ,			/* flags */
	(int) 0 ,			/* index in the graph list */
	(int) 0 , 			/* graph call chain top-sort order */
	(int) 0 ,			/* internal number of cycle on */
	(int) 0 ,			/* number of live parent arcs */
	(struct nl *) &indirectchild ,	/* pointer to head of cycle */
	(struct nl *) 0 ,		/* pointer to next member of cycle */
	(arctype *) 0 ,			/* list of caller arcs */
	(arctype *) 0 			/* list of callee arcs */
};

findcall(parentp, p_lowpc, p_highpc)
	nltype		*parentp;
	unsigned long	p_lowpc;
	unsigned long	p_highpc;
{
	register u_long pc;
	nltype *childp;
	unsigned long destpc;
	register long op;
	register int off;

	if (textspace == 0)
		return;
	if (p_lowpc < s_lowpc)
		p_lowpc = s_lowpc;
	if (p_highpc > s_highpc)
		p_highpc = s_highpc;

	for (pc = p_lowpc; pc < p_highpc; pc += 4) {
		off = pc - s_lowpc;
		op = *(u_long *)&textspace[off];
		if ((op & 0xc0000000) == 0x40000000) {
			/*
			 * a pc relative call insn -- check that this
			 * is the address of a function.
			 */
			off = (op & 0x3fffffff) << 2;
			destpc = pc + off;
			if (destpc >= s_lowpc && destpc <= s_highpc) {
				childp = nllookup(destpc);
				if (childp != 0 && childp->value == destpc)
					addarc(parentp, childp, 0L);
			}
		} else if ((op & 0xfff80000) == 0x9fc00000)
			/*
			 * A jmpl with rd = 15 (%o7) -- an indirect call.
			 */
			addarc(parentp, &indirectchild, 0L);
	}
}
