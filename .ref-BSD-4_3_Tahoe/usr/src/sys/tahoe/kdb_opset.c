/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_opset.c	7.4 (Berkeley) 5/21/88
 */

#include "../kdb/defs.h"

/*
 * Instruction printing.
 */

REGLIST reglist[] = {
	"p2lr",	&pcb.pcb_p2lr,	"p2br",	(int *)&pcb.pcb_p2br,
	"p1lr",	&pcb.pcb_p1lr,	"p1br",	(int *)&pcb.pcb_p1br,
	"p0lr",	&pcb.pcb_p0lr,	"p0br",	(int *)&pcb.pcb_p0br,
	"ksp",	&pcb.pcb_ksp,	"hfs",	&pcb.pcb_hfs,
	"psl",	&pcb.pcb_psl,	"pc",	&pcb.pcb_pc,
	"ach",	&pcb.pcb_ach,	"acl",	&pcb.pcb_acl,
	"usp",	&pcb.pcb_usp,	"fp",	&pcb.pcb_fp,
	"r12",	&pcb.pcb_r12,	"r11",	&pcb.pcb_r11,
	"r10",	&pcb.pcb_r10,	"r9",	&pcb.pcb_r9,
	"r8",	&pcb.pcb_r8,	"r7",	&pcb.pcb_r7,
	"r6",	&pcb.pcb_r6,	"r5",	&pcb.pcb_r5,
	"r4",	&pcb.pcb_r4,	"r3",	&pcb.pcb_r3,
	"r2",	&pcb.pcb_r2,	"r1",	&pcb.pcb_r1,
	"r0",	&pcb.pcb_r0,
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

	value = (u_int)chkget((off_t)inkdot(incp), idsp);
	incp += nbytes;
	return(value>>(4-nbytes)*8);
}

printins(idsp, ins)
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
		printf("?%2x%8t", ins);
		dotinc = 1;
		return;
	}
	printf("%s%8t",ip->iname);
	incp = 1;
	ap = ip->argtype;
	for (argno = 0; argno < ip->nargs; argno++, ap++) {
		var[argno] = 0x80000000;
		if (argno!=0) printc(',');
	  top:
		if (*ap&ACCB)
			mode = 0xAF + ((*ap&7)<<5);  /* branch displacement */
		else {
			mode = bchkget(inkdot(incp),idsp); ++incp;
		}
		r = mode&0xF;
		mode >>= 4;
		switch ((int)mode) {
			case 0: case 1:
			case 2: case 3:	/* short literal */
				printc('$');
				d = mode<<4|r;
				goto immed;
			case 4: /* [r] */
				printf("[%s]", regname[r]);
				goto top;
			case 5: /* r */
				printf("%s", regname[r]);
				break;
			case 6: /* (r) */
				printf("(%s)", regname[r]);
				break;
			case 7: /* -(r) */
				printf("-(%s)", regname[r]);
				break;
			case 9: /* *(r)+ */
				printc('*');
			case 8: /* (r)+ */
				if (r == 0xF ||
				    mode == 8 && (r == 8 || r == 9)) {
					printc('$');
					d = snarf((r&03)+1, idsp);
				} else {	/*it's not PC immediate or abs*/
					printf("(%s)+", regname[r]);
					break;
				}
			immed:
				if (ins == KCALL && d >= 0 && d < 200) {
					printf("%R", d);
					break;
				}
				goto disp;
			case 0xB:	/* byte displacement deferred */
			case 0xD:	/* word displacement deferred */
			case 0xF:	/* long displacement deferred */
				printc('*');
			case 0xA:	/* byte displacement */
			case 0xC:	/* word displacement */
			case 0xE:	/* long displacement */
				d = snarf(1<<((mode>>1&03)-1), idsp);
				if (r==0xF) { /* PC offset addressing */
					d += dot+incp;
					psymoff(d,type,"");
					var[argno]=d;
					break;
				}
			disp:
				if (d >= 0 && d < maxoff)
					printf("%R", d);
				else
					psymoff(d,type,"");
				if (mode >= 0xA)
					printf("(%s)", regname[r]);
				var[argno] = d;
				break;
		}
	}
	if (ins == CASEL) {
		if (inkdot(incp)&01)	/* align */
			incp++;
		for (argno = 0; argno <= var[2]; ++argno) {
			printc(EOR);
			printf("    %R:  ", argno+var[1]);
			d = shorten(get(inkdot(incp+argno+argno), idsp));
			if (d&0x8000)
				d -= 0x10000;
			psymoff(inkdot(incp)+d, type, "");
		}
		incp += var[2]+var[2]+2;
	}
	dotinc = incp;
}
