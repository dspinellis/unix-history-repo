#ifndef lint
static char sccsid[] = "@(#)opset.c	1.4 (Berkeley) %G%";
#endif

/*
 * adb - instruction decoding
 */

#include "defs.h"
#include "optab.h"

struct	optab *ioptab[256];	/* index by opcode to optab */

/* set up ioptab */
mkioptab()
{
	register struct optab *p;

	for (p = optab; p->iname; p++)
		ioptab[p->val] = p;
}

/*
 * Print one instruction, and leave dotinc set to the number of bytes
 * it occupied.
 */
printins(space)
	int space;
{
	u_char ins;		/* instruction opcode */
	int argno;		/* argument index */
	register int mode;	/* mode */
	register int r;		/* register name */
	register int d;		/* assembled byte, word, long or float */
	register int dotoff;	/* offset from dot of current byte */
	register u_char *ap;
	register struct optab *ip;
	union {
		u_char	ub;
		char	b;
		short	w;
		int	l;
	} mem;
	extern char *syscalls[];
	extern int nsys;
#define	snarfbytes(nbytes) \
	(void) adbread(space, inkdot(dotoff), &mem.b, nbytes); \
	checkerr(); \
	dotoff += (nbytes)

	if (space == SP_NONE)
		ins = (u_char)dot;
	else {
		(void) adbread(space, dot, &ins, 1);
		checkerr();
	}
	if ((ip = ioptab[ins]) == NULL) {
		adbprintf("?%2x", ins);
		dotinc = 1;
		return;
	}
	adbprintf("%s%8t", ip->iname);
	dotoff = 1;
	ap = ip->argtype;
	for (argno = 0; argno < ip->nargs; argno++, ap++) {
		var[argno] = 0x80000000;
		if (argno != 0)
			printc(',');
again:
		if (*ap & ACCB)		/* branch displacement */
			mode = 0xAF + ((*ap & 7) << 5);
		else {
			snarfbytes(1);
			mode = mem.ub;
		}
		r = mode & 0xF;
		mode >>= 4;
		switch (mode) {

		case 0: case 1: case 2: case 3:
			/* short literal */
			d = mode << 4 | r;
			goto immed;

		case 4:	/* [r] */
			adbprintf("[%s]", regname[r]);
			goto again;

		case 5:	/* r */
			adbprintf("%s", regname[r]);
			continue;

		case 6:	/* (r) */
			adbprintf("(%s)", regname[r]);
			continue;

		case 7:	/* -(r) */
			adbprintf("-(%s)", regname[r]);
			continue;

		case 9:	/* *(r)+ */
			printc('*');
			/* FALLTHROUGH */

		case 8:	/* (r)+ */
			if (r == 0xf) {
				/* PC immediate */
				snarfbytes(4);
				d = mem.l;
			} else if (mode == 8 && (r == 8 || r == 9)) {
				/* absolute */
				snarfbytes((r & 1) + 1);
				d = r == 8 ? mem.b : mem.w;
			} else {
				adbprintf("(%s)+", regname[r]);
				continue;
			}
	immed:
			printc('$');
			if (ins == KCALL && (u_int)d < nsys && syscalls[d])
				prints(syscalls[d]);
			else
				adbprintf("%R", d);
			var[argno] = d;
			continue;

		case 0xA:	/* byte displacement */
		case 0xB:	/* byte displacement deferred */
			d = 1;
			break;

		case 0xC:	/* word displacement */
		case 0xD:	/* word displacement deferred */
			d = 2;
			break;

		case 0xE:	/* long displacement */
		case 0xF:	/* long displacement deferred */
			d = 4;
			break;
		}

		/* displacement or displacement deferred */
		if (mode & 1)
			printc('*');
		snarfbytes(d);
		switch (d) {
		case 1:
			d = mem.b;
			break;
		case 2:
			d = mem.w;
			break;
		case 4:
			d = mem.l;
			break;
		}
		if (r == 0xF) {	/* PC offset addressing */
			d += dot + dotoff;
			psymoff("%R", (addr_t)d, SP_DATA, maxoff, "");
		} else
			adbprintf("%V(%s)", d, regname[r]);
		var[argno] = d;
	}
	if (ins == CASEL) {
		register addr_t adjdot;

		if (inkdot(dotoff) & 01)	/* align */
			dotoff++;
		adjdot = inkdot(dotoff);
		for (argno = 0; argno <= var[2]; ++argno) {
			adbprintf("\n    %R:  ", argno + var[1]);
			snarfbytes(2);
			psymoff("%R", adjdot + mem.w, SP_DATA, maxoff, "");
		}
	}
	dotinc = dotoff;
}
