#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
SCCSID(@(#)opset.c	2.3);

STRING		errflg;
L_INT		dot;
INT		dotinc;
L_INT		var[];


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

printins(f,idsp,ins)
#ifndef vax
REG INT		ins;
#else
REG L_INT	ins;
#endif
{
	short i,b,mode; char **r; long d; char *fmat;
	REG char *	ap;
	REG OPTAB	ip;
#ifndef vax
	struct {char b2,b3,b0,b1;};
#else
	struct {char b0,b1,b2,b3;};
#endif

	type=DSYM; space=idsp;
	ins &= LOBYTE;
	ip=optab+ioptab[ins]; printf("%s%8t",ip->iname); incp=1;
	ap=ip->argtype;
	for (i=0; i<ip->nargs; i++,ap++) {
		var[i]=0x80000000;
		if (i!=0) printc(',');
	  top:
		if (*ap&ACCB) b= 0xAF + ((*ap&7)<<5);  /* branch displacement */
		else {b=bchkget(inkdot(incp),idsp); ++incp;}
		if (b&0300) {/* not short literal */
			r= &regname[b&0xF]; mode= b >>= 4;
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
				  var[i]=d;
				  if (r==(regname+0xF) && b>=0xA) var[i] += dot+incp;
				  psymoff(var[i],type,"");
				  if (r!=regname+0xF) goto base;
				  break;
				case 0xD: printc('*');
				case 0xC: d=0;
					d.b0 = bchkget(inkdot(incp),idsp); ++incp;
					d.b1 = bchkget(inkdot(incp),idsp); ++incp;
					if (d&0x8000) d -= 0x10000; fmat=fmtr;
					goto disp;
				case 0xF: printc('*');
				case 0xE:
			  abs:
					d.b0 = bchkget(inkdot(incp),idsp); ++incp;
					d.b1 = bchkget(inkdot(incp),idsp); ++incp;
					d.b2 = bchkget(inkdot(incp),idsp); ++incp;
					d.b3 = bchkget(inkdot(incp),idsp); ++incp;
					fmat=fmtR; goto disp;
			}
		} else {/* short literal */
			var[i]=b;
			if ((*ap&7)==TYPF || (*ap&7)==TYPD) printf("$%s",fltimm[b]);
			else printf("$%r",b);
		}
	}
	if (ins==0xCF || ins==0xAF || ins==0x8F) {/* CASEx instr */
		for (i=0; i<=var[2]; ++i) {
			printc(EOR); printf("    %R:  ",i+var[1]);
			d=get(inkdot(incp+i+i),idsp)&0xFFFF;
			if (d&0x8000) d -= 0x10000;
			psymoff(inkdot(incp)+d,type,"");
		}
		incp += var[2]+var[2]+2;
	}
	dotinc=incp;
}
