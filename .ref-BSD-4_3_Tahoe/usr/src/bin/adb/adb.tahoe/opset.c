#ifndef lint
static	char sccsid[] = "@(#)opset.c	1.1 (Berkeley) 2/25/86";
#endif
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"
#include "optab.h"

STRING		errflg;
L_INT		dot;
INT		dotinc;
L_INT		var[];


/* instruction printing */

POS	type, space, incp;

OPTAB ioptab[256]; /* index by opcode to optab */

mkioptab() {/* set up ioptab */
	REG OPTAB p=optab;
	while (p->iname){
		ioptab[p->val&LOBYTE]=p;
		p++;
	}
}

printins(idsp,ins)
	REG L_INT	ins;
{
	short	argno;		/* argument index */
	REG	mode;		/* mode */
	REG	r;		/* register name */
	REG	d;		/* assembled byte, word, long or float */
	long	snarf();
	REG char *	ap;
	REG OPTAB	ip;

	type = DSYM;
	space = idsp;
	ins = byte(ins);
	if((ip=ioptab[ins]) == (OPTAB)0) {
		printf("?%2x%8t", ins);
		dotinc = 1;
		return;
	}
	printf("%s%8t",ip->iname);
	incp = 1;
	ap = ip->argtype;
	for (argno=0; argno<ip->nargs; argno++,ap++) {
		var[argno] = 0x80000000;
		if (argno!=0) printc(',');
	  top:
		if (*ap&ACCB)
			mode = 0xAF + ((*ap&7)<<5);  /* branch displacement */
		else{
			mode = bchkget(inkdot(incp),idsp); ++incp;
		}
		r = mode&0xF;
		mode >>= 4;
		switch ((int)mode) {
			case 0:
			case 1:
			case 2:
			case 3:
				/* short literal */
				printc('$');
				d = mode<<4|r;
				goto immed;
			case 4: /* [r] */
				printf("[%s]",regname[r]);
				goto top;
			case 5: /* r */
				printf("%s",regname[r]);
				break;
			case 6: /* (r) */
				printf("(%s)",regname[r]);
				break;
			case 7: /* -(r) */
				printf("-(%s)",regname[r]);
				break;
			case 9: /* *(r)+ */
				printc('*');
			case 8: /* (r)+ */
				if(r==0xF || mode==8 && (r==8 || r==9)) {
					printc('$');
					d = snarf((r&03)+1, idsp);
				} else {	/*it's not PC immediate or abs*/
					printf("(%s)+",regname[r]);
					break;
				}
			immed:
				if(ins == KCALL && d>=0 && d<SYSSIZ) {
					if(systab[d])
						printf(systab[d]);
					else
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
				if(d>=0 && d<maxoff)
					printf("%R", d);
				else
					psymoff(d,type,"");
				if (mode>=0xA)
					printf("(%s)",regname[r]);
				var[argno]=d;
				break;
		} /* end of the mode switch */
	}
	if (ins==CASEL) {
		if(inkdot(incp)&01)	/* align */
			incp++;
		for (argno=0; argno<=var[2]; ++argno) {
			printc(EOR);
			printf("    %R:  ",argno+var[1]);
			d=shorten(get(inkdot(incp+argno+argno),idsp));
			if (d&0x8000) d -= 0x10000;
			psymoff(inkdot(incp)+d,type,"");
		}
		incp += var[2]+var[2]+2;
	}
	dotinc=incp;
}

long snarf (nbytes, idsp)
{
	register long value;

	value = chkget(inkdot(incp), idsp);
	incp += nbytes;
	return(value>>(4-nbytes)*8);
}
