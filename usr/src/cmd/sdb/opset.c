static	char sccsid[] = "@(#)opset.c 4.1 10/9/80";
#
/*
 *
 *	UNIX debugger
 *
 *		Instruction printing routines.
 *		MACHINE DEPENDENT.
 */

#include "head.h"
SCCSID(@(#)opset.c	2.4);

STRING		errflg;
L_INT		dot;
INT		dotinc;
L_INT		vvar[36];


/* instruction printing */

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
#define TYPF	4	/* floating */
#define TYPD	5	/* double floating */


TYPE	struct optab	*OPTAB;
struct optab {
	char *iname;
	char val;
	char nargs;
	char argtype[6];
} optab[];
#define SYSTAB struct systab
SYSTAB {
	int	argc;
	char	*sname;
} systab[];
STRING	regname[];
STRING	fltimm[];
POS	type, space, incp;

int ioptab[256]; /* index by opcode to optab */

mkioptab() {/* set up ioptab */
REG OPTAB p=optab; while (p->iname) {ioptab[p->val&LOBYTE]=p-optab; p++;}
}

extern char *fmtr;
extern char *fmtR;

printins(fmt,idsp,ins)
char fmt;
#ifndef vax
REG INT		ins;
#else
REG L_INT	ins;
#endif
{
	short i,b,mode; char **r; long d; char *fmat;
	struct proct *procp;
	REG char *	ap;
	REG OPTAB	ip;
#ifndef vax
	struct {char b_2,b_3,b_0,b_1;};
#else
	struct {char b_0,b_1,b_2,b_3;};
#endif
	procp = adrtoprocp(dot);
	if (procp->paddr == dot) {
		printf("0x%04.4x", ins & 0xffff);
		oincr = 2;
		return;
	}

	type=DSYM; space=idsp;
	ins &= LOBYTE;
	ip=optab+ioptab[ins]; printf("%s\t",ip->iname); incp=1;
	ap=ip->argtype;
	for (i=0; i<ip->nargs; i++,ap++) {
		vvar[i]=0x80000000;
		if (i!=0) printc(',');
	  top:
		if (*ap&ACCB) b= 0xAF + ((*ap&7)<<5);  /* branch displacement */
		else {b=bchkget(inkdot(incp),idsp); ++incp;}
		if (b&0300) {/* not short literal */
			char *slnptr;
			int regno;
			regno = b & 0xF;
			if (fmt=='i' && regno >= 6 && regno <= 11 && 
			    adrtoregvar(regno, procp) != -1) {
				slnptr = sl_name;
				r = &slnptr;
			}
			else
				r= &regname[regno];
			mode= b >>= 4;
		  mid:
			switch ((int)mode) {
				case 4: /* [r] */ printf("[%s]",*r); goto top;
				case 5: /* r */ printf("%s",*r); break;
				case 7: /* -(r) */ printc('-');
			  base:
				case 6: /* (r) */ printf("(%s)",*r); break;
				case 9: /* *(r)+ */ printc('*');
				case 8: /* (r)+ */
				  if (r==(regname+0xF)) {/* PC: immediate or absolute */
					printc('$'); if (b==9) goto abs;
					mode=((*ap&7)<<1)+0xA; goto mid;
				  }
				  printf("(%s)+",*r); break;
				case 0xB: printc('*');
				case 0xA: d=bchkget(inkdot(incp),idsp); ++incp;
				  if (d&0x80) d -= 0x100; fmat=fmtr;
			  disp:
				  vvar[i]=d;
				  if (r==(regname+0xF) && b>=0xA) vvar[i] += dot+incp;
				  if (psymoff(vvar[i],r,fmt) && r!=regname+0xF)
					goto base;
				  break;
				case 0xD: printc('*');
				case 0xC: d=0;
					d.b_0 = bchkget(inkdot(incp),idsp); ++incp;
					d.b_1 = bchkget(inkdot(incp),idsp); ++incp;
					if (d&0x8000) d -= 0x10000; fmat=fmtr;
					goto disp;
				case 0xF: printc('*');
				case 0xE:
			  abs:
					d.b_0 = bchkget(inkdot(incp),idsp); ++incp;
					d.b_1 = bchkget(inkdot(incp),idsp); ++incp;
					d.b_2 = bchkget(inkdot(incp),idsp); ++incp;
					d.b_3 = bchkget(inkdot(incp),idsp); ++incp;
					fmat=fmtR; goto disp;
			}
		} else {/* short literal */
			vvar[i]=b;
			if ((*ap&7)==TYPF || (*ap&7)==TYPD) 
				printf("$%s",fltimm[b]);
			else printf("$%d",b);
		}
	}
	if (ins==0xCF || ins==0xAF || ins==0x8F) {/* CASEx instr */
		for (i=0; i<=vvar[2]; ++i) {
			printc(EOR); printf("    %d:  ",i+vvar[1]);
			d=get(inkdot(incp+i+i),idsp)&0xFFFF;
			if (d&0x8000) d -= 0x10000;
			psymoff(inkdot(incp)+d,type,fmt);
		}
		incp += vvar[2]+vvar[2]+2;
	}
	oincr=incp;
}

L_INT	inkdot(incr)
{
	L_INT		newdot;

	newdot=dot+incr;
	return(newdot);
}

printc(c)
char c; {
	printf("%c", c);
}

psymoff(v, r, fmt)
L_INT v; char fmt, **r; {
	struct proct *procp;
	register int diff;
	if (fmt == 'i') {
		if (r == regname + 12) {   /* parameter */
			if ((diff = adrtoparam((ADDR) v, adrtoprocp(dot)))
					!= -1) {
				printf("%s", sl_name);
				prdiff(diff);
				return(0);
			}
		}
		if (r == regname + 13) {   /* local */
			if ((diff = adrtolocal((ADDR) -v, adrtoprocp(dot))
					) != -1) {
				printf("%s", sl_name);
				prdiff(diff);
				return(0);
			}
		}
		if (v < firstdata) {
			if ((procp = adrtoprocp((ADDR) v)) != badproc) {
				prlnoff(procp, v);
				return(0);
			}
		} else {
			if ((diff = adrtoext((ADDR) v)) != -1) {
				printf("%s", sl_name);
				prdiff(diff);
				return(0);
			}
		}
	}
	prhex(v);
	return(1);
}


prdiff(diff) {
	if (diff) {
		printf("+");
		prhex(diff);
	}
}
