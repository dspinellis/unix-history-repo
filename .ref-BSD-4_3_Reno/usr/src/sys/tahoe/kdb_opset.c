/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_opset.c	7.6 (Berkeley) 6/22/90
 */

#include "../kdb/defs.h"

/*
 * Instruction printing.
 */

REGLIST kdbreglist[] = {
	"p2lr",	&kdbpcb.pcb_p2lr,	"p2br",	(int *)&kdbpcb.pcb_p2br,
	"p1lr",	&kdbpcb.pcb_p1lr,	"p1br",	(int *)&kdbpcb.pcb_p1br,
	"p0lr",	&kdbpcb.pcb_p0lr,	"p0br",	(int *)&kdbpcb.pcb_p0br,
	"ksp",	&kdbpcb.pcb_ksp,	"hfs",	&kdbpcb.pcb_hfs,
	"psl",	&kdbpcb.pcb_psl,	"pc",	&kdbpcb.pcb_pc,
	"ach",	&kdbpcb.pcb_ach,	"acl",	&kdbpcb.pcb_acl,
	"usp",	&kdbpcb.pcb_usp,	"fp",	&kdbpcb.pcb_fp,
	"r12",	&kdbpcb.pcb_r12,	"r11",	&kdbpcb.pcb_r11,
	"r10",	&kdbpcb.pcb_r10,	"r9",	&kdbpcb.pcb_r9,
	"r8",	&kdbpcb.pcb_r8,	"r7",	&kdbpcb.pcb_r7,
	"r6",	&kdbpcb.pcb_r6,	"r5",	&kdbpcb.pcb_r5,
	"r4",	&kdbpcb.pcb_r4,	"r3",	&kdbpcb.pcb_r3,
	"r2",	&kdbpcb.pcb_r2,	"r1",	&kdbpcb.pcb_r1,
	"r0",	&kdbpcb.pcb_r0,
	0
};

/*
 * Argument access types
 */
#define ACCA	(8<<3)	/* address only */
#define ACCR	(1<<3)	/* read */
#define ACCW	(2<<3)	/* write */
#define ACCM	(3<<3)	/* modify */
#define ACCB	(4<<3)	/* branch displacement */
#define ACCI	(5<<3)	/* XFC code */

/*
 * Argument data types
 */
#define TYPB	0	/* byte */
#define TYPW	1	/* word */
#define TYPL	2	/* long */
#define TYPQ	3	/* quad */
#define TYPF	4	/* float */
#define TYPD	5	/* double */

struct optab {
	char *iname;
	char val;
	char nargs;
	char argtype[6];
};

static	struct optab *ioptab[256];	/* index by opcode to optab */
static	struct optab optab[] = {	/* opcode table */
#define OP(a,b,c,d,e,f,g,h,i) {a,b,c,d,e,f,g,h,i}
#include "../tahoe/kdb_instrs"
0};
static	char *regname[] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10", "r11", "r12", "fp", "sp", "pc"
};
static	int type, space, incp;

/* set up ioptab */
kdbsetup()
{
	register struct optab *p;

	for (p = optab; p->iname; p++)
		ioptab[p->val&LOBYTE] = p;
}

static long
snarf(nbytes, idsp)
{
	register long value;

	value = (u_int)kdbchkget((off_t)kdbinkdot(incp), idsp);
	incp += nbytes;
	return(value>>(4-nbytes)*8);
}

kdbprintins(idsp, ins)
	register long ins;
{
	short argno;		/* argument index */
	register mode;		/* mode */
	register r;		/* register name */
	register long d;	/* assembled byte, word, long or float */
	register char *ap;
	register struct optab *ip;

	type = DSYM;
	space = idsp;
	ins = byte(ins);
	if ((ip = ioptab[ins]) == (struct optab *)0) {
		kdbprintf("?%2x%8t", ins);
		kdbdotinc = 1;
		return;
	}
	kdbprintf("%s%8t",ip->iname);
	incp = 1;
	ap = ip->argtype;
	for (argno = 0; argno < ip->nargs; argno++, ap++) {
		kdbvar[argno] = 0x80000000;
		if (argno!=0) kdbprintc(',');
	  top:
		if (*ap&ACCB)
			mode = 0xAF + ((*ap&7)<<5);  /* branch displacement */
		else {
			mode = kdbbchkget(kdbinkdot(incp),idsp); ++incp;
		}
		r = mode&0xF;
		mode >>= 4;
		switch ((int)mode) {
			case 0: case 1:
			case 2: case 3:	/* short literal */
				kdbprintc('$');
				d = mode<<4|r;
				goto immed;
			case 4: /* [r] */
				kdbprintf("[%s]", regname[r]);
				goto top;
			case 5: /* r */
				kdbprintf("%s", regname[r]);
				break;
			case 6: /* (r) */
				kdbprintf("(%s)", regname[r]);
				break;
			case 7: /* -(r) */
				kdbprintf("-(%s)", regname[r]);
				break;
			case 9: /* *(r)+ */
				kdbprintc('*');
			case 8: /* (r)+ */
				if (r == 0xF ||
				    mode == 8 && (r == 8 || r == 9)) {
					kdbprintc('$');
					d = snarf((r&03)+1, idsp);
				} else {	/*it's not PC immediate or abs*/
					kdbprintf("(%s)+", regname[r]);
					break;
				}
			immed:
				if (ins == KCALL && d >= 0 && d < 200) {
					kdbprintf("%R", d);
					break;
				}
				goto disp;
			case 0xB:	/* byte displacement deferred */
			case 0xD:	/* word displacement deferred */
			case 0xF:	/* long displacement deferred */
				kdbprintc('*');
			case 0xA:	/* byte displacement */
			case 0xC:	/* word displacement */
			case 0xE:	/* long displacement */
				d = snarf(1<<((mode>>1&03)-1), idsp);
				if (r==0xF) { /* PC offset addressing */
					d += kdbdot+incp;
					kdbpsymoff(d,type,"");
					kdbvar[argno]=d;
					break;
				}
			disp:
				if (d >= 0 && d < kdbmaxoff)
					kdbprintf("%R", d);
				else
					kdbpsymoff(d,type,"");
				if (mode >= 0xA)
					kdbprintf("(%s)", regname[r]);
				kdbvar[argno] = d;
				break;
		}
	}
	if (ins == CASEL) {
		if (kdbinkdot(incp)&01)	/* align */
			incp++;
		for (argno = 0; argno <= kdbvar[2]; ++argno) {
			kdbprintc(EOR);
			kdbprintf("    %R:  ", argno+kdbvar[1]);
			d = shorten(kdbget(kdbinkdot(incp+argno+argno), idsp));
			if (d&0x8000)
				d -= 0x10000;
			kdbpsymoff(kdbinkdot(incp)+d, type, "");
		}
		incp += kdbvar[2]+kdbvar[2]+2;
	}
	kdbdotinc = incp;
}
ADDR	kdblastframe;
ADDR	kdbcallpc;


kdbstacktrace(dolocals)
	int dolocals;
{
	register int narg;
	register ADDR argp, frame;
	int tramp;

	if (kdbadrflg) {
		frame = kdbadrval;
		kdbcallpc = getprevpc(frame);
	} else {
		frame = kdbpcb.pcb_fp;
		kdbcallpc = kdbpcb.pcb_pc;
	}
	kdblastframe = NOFRAME;
	while (kdbcntval-- && frame != NOFRAME) {
		char *name;

		kdbchkerr();
		/* check for pc in pcb (signal trampoline code) */
		if (issignalpc(kdbcallpc)) {
			tramp = 1;
			name = "sigtramp";
		} else {
			tramp = 0;
			(void) kdbfindsym((long)kdbcallpc, ISYM);
			if (kdbcursym)
				name = kdbcursym->n_un.n_name;
			else
				name = "?";
		}
		kdbprintf("%s(", name);
		narg = getnargs(frame);
		if (narg > 10)
			narg = 10;
		argp = frame;
		if (!tramp)
			while (narg) {
				kdbprintf("%R",
				    kdbget((off_t)(argp = nextarg(argp)),
					DSP));
				if (--narg != 0)
					kdbprintc(',');
			}
		kdbprintf(") at ");
		kdbpsymoff((long)kdbcallpc, ISYM, "\n");

		if (dolocals) {
			register ADDR word;

			while (kdblocalsym((long)frame)) {
				word = kdbget((off_t)kdblocalval, DSP);
				kdbprintf("%8t%s:%10t",
				    kdbcursym->n_un.n_name);
				if (kdberrflg) {
					kdbprintf("?\n");
					kdberrflg = 0;
				} else
					kdbprintf("%R\n", word);
			}
		}
		if (!tramp) {
			kdbcallpc = getprevpc(frame);
			kdblastframe = frame;
			frame = getprevframe(frame);
		} else
			kdbcallpc = getsignalpc(kdblastframe);
		if (!kdbadrflg && !INSTACK(frame))
			break;
	}
}
