#
char AS2[] = "@(#)as2.c 1.29 79/03/21 22:27:31";	/* sccs ident */
#include	<stdio.h>
#include	"as.h"
#define JBR 0x11
#define BRW 0x31
extern struct symtab *nextsym;
struct tshdr {/* overlays symtab struct */
	char tsflag; /* 0xFF==active, 0xFE==inactive */
	char tsbump; /* 0->short, 1->long */
	short tsfear; /* possible bump value */
	short tsline; /* source line number, for debugging and error messages */
	short dest;  /* symtab ordinal of destination label */
	char type;
	char tag;
	short index; /* usetab ordinal of csect */
	long value;  /* estimated PC of following instruction */
	char	ptype;
	char	other;
	short	desc;
};

jxxxbump(ts1,displ) register struct tshdr *ts1; {
register struct symtab *sp2;
/* printf("\njxxxbump %d  %.8s",displ,symtab[ts1->dest].name);  */
ts1->tsflag=0xFE; ts1->tsbump=1; /* no longer in doubt, alas */
for (sp2=symtab; sp2<nextsym; sp2++)
	if ((sp2->type&XTYPE)==ts1->type && sp2->index==ts1->index &&
			sp2->value>=ts1->value && sp2!=ts1) sp2->value += ts1->tsfear;
usedot[ts1->index].xvalue += ts1->tsfear;
}

jxxxfix() {/* pass 1.5, resolve jxxx instructions and .align in .text */
register struct tshdr *sp1; register struct symtab *sp2,*sp3;
#define TUNNEL 4
register struct tshdr *tunnel;
register int displ,nchange;
for (;;) {/* lazy topological sort */
	nchange=0;
	tunnel=0;
	for (sp1=symtab; sp1<nextsym; sp1++) {
		if (sp1->dest== -1) continue; /* .align in .text */
		displ=sp1->tsflag&0xFF;
		if (sp1->tsfear==1 || displ==0xFE && sp1->tsbump) tunnel=sp1;
		if (displ<0xFF) continue;
		sp2= &symtab[sp1->dest];
		if (sp1->index!=sp2->index) {
			yyerror("Intersegment jxxx, line %d",sp1->tsline); continue;
		}
		displ=sp2->value - sp1->value;
		if (displ<-128 || displ>127) {
			if (1<sp1->tsfear && tunnel && tunnel->dest==sp1->dest
			  && tunnel->index==sp1->index
			  && tunnel->value - sp1->value>=-125) {/* entrance is 3 back */
				sp1->tsfear=TUNNEL; sp1->dest=((struct symtab *)tunnel)-symtab;
				sp1->tsflag=0xFE; continue;
			} else {jxxxbump(sp1,displ); tunnel=sp1; ++nchange; continue;}
		}
			/* immediate lose */
		if (displ>=0) {/* forward search for intervening jxxx */
			for (sp3=sp1+1; sp3<nextsym; sp3++) {
				if ((((struct tshdr *)sp3)->tsflag&0xFF)<0xFF
					|| sp3->index!=sp1->index) continue;
				if (sp3->value > sp2->value) break; /* beyond destination */
				displ += sp3->tsfear; /* assume worst case */
			}
			if (displ<=127) sp1->tsflag=0xFE; /* win! */
		} else {/* backward search for intervening jxxx */
			for (sp3=sp1-1; sp3>=symtab; sp3--) {
				if ((((struct tshdr *)sp3)->tsflag&0xFF)<0xFF
					|| sp3->index!=sp1->index) continue;
				if (sp3->value <= sp2->value) break; /* beyond destination */
				displ -= sp3->tsfear; /* assume worst case */
			}
			if (displ>=-128) sp1->tsflag=0xFE; /* win! */
		}
	}
	if (nchange==0) break;
}
for (sp1=symtab; sp1<nextsym; sp1++) /* handle .align in .text */
	if (sp1->dest== -1 && (displ = sp1->value & (unsigned)(sp1->tsfear)))
		jxxxbump(sp1,sp1->tsfear -= displ-1);
}

short njxxx;
struct symtab *lastnam;
struct tshdr *rovjxxx /* = {symtab-1}*/ ;

remjxxx() {/* remove jxxx entries from symbol table */
register struct symtab *sp1,*sp2;
for (sp1=symtab; sp1<nextsym; sp1++) {
	if ((((struct tshdr *)sp1)->tsflag&0xFF)<0xFE) continue;
	sp2=sp1; /* found first */
	for (;;) {
		register char *cp1,*cp2;
		while ((((struct tshdr *)sp1)->tsflag&0xFF)>=0xFE) sp1++;
		if (sp1>=nextsym) {nextsym=sp2; return;}
		*sp2 = *sp1;
		sp2++; sp1++;
	}
} njxxx=0;
}

ijxout(op,ap,nact) struct arg *ap; {/* handle jxxx instructions */
register struct arg *aplast;
aplast=ap+nact-1;
if (passno!=2) {/* record the jxxx in a special symbol table entry */
	register struct tshdr *ts2;
	putins(op,ap,nact); /* assume minimal length */
	ts2=(struct tshdr *)(symalloc()); ts2->tsflag=0xFF;
	ts2->tsfear=3; if (op==JBR) ts2->tsfear=1;
	ts2->tsline=lineno;
	if (lastnam==0) yyerror("jxxx destination not a label");
	ts2->dest=lastnam-symtab; ts2->type=dotp->xtype;
	ts2->index=dotp-usedot; ts2->value=dotp->xvalue; njxxx++;
} else {/* pass2, resolve */
	register long displ; register struct exp *xp; register struct tshdr *sp;
	/* forward search for special symbol table entry */
	while (((++rovjxxx)->tsflag&0xFF)<0xFE);
	xp=aplast->xp;
	if (rovjxxx->tsfear==TUNNEL) {
		sp= &symtab[rovjxxx->dest]; xp->xvalue=sp->value;
		if (1==sp->tsfear) xp->xvalue -= 2; /* brw */
	}
	if (rovjxxx->tsbump==0) putins(op,ap,nact);
	else {
		if (op!=JBR) {
			displ=xp->xvalue; xp->xvalue=rovjxxx->value+rovjxxx->tsfear;
			putins(op^1,ap,nact); xp->xvalue=displ;
		}
		putins(BRW,aplast,1);
	}
}}

jalign(xp) register struct exp *xp; {
	if (xp->xtype != XABS || 0>xp->xvalue || xp->xvalue>16) {
		yyerror("Illegal `align' argument"); return;
	}
	flushfield(NBPW/4);
	if (dotp<(usedot+NLOC)) {/* .align in .text */
		if (passno!=2) {/* record in symbol table entry */
			register struct tshdr *ts2;
			ts2=(struct tshdr *)(symalloc()); ts2->tsflag=0xFF;
			ts2->tsfear=(1<<xp->xvalue)-1; ts2->tsline=lineno;
			ts2->dest= -1; ts2->type=dotp->xtype;
			ts2->index=dotp-usedot; ts2->value=dotp->xvalue;
			njxxx++; return;
		} else while (((++rovjxxx)->tsflag&0xFF)<0xFE);
	}
	while (dotp->xvalue & ((1<<xp->xvalue)-1)) outb(0);
}

insout(op, ap, nact)
struct arg *ap;
{
	int jxxflg;

	op &= 0xFF; if (0>(jxxflg=nact)) nact= -nact;
	if (passno!=2) {
		register struct arg *ap2; register struct instab *ip; int i,nexp;
		ip=itab[op]; nexp=ip->nargs;
		if (nact<nexp) yyerror("Too few arguments");
		if (nact>nexp) {yyerror("Too many arguments"); nact=nexp;}
		for (ap2=ap+nact, i=nact; --i>=0;) argcompat(--ap2,ip->argtyp[i],i);
	}
	if (jxxflg<0) ijxout(op,ap,nact);
	else putins(op, ap, nact);
}

argcompat(act, exp, i)
struct arg *act;
int exp,i;
{
	register at,atm;

	at = act->atype; atm=at&AMASK;

	if ((exp&ACCA) && (atm==AREG)) {
		yyerror("arg %d, addressing a register",i); return;}
	if ((exp&ACCW) && (atm==AIMM) && !(at&ASTAR)) {
		yyerror("arg %d, modifying a constant",i); return;}
	if (at&AINDX) {
		if (act->areg2==017) {
			yyerror("arg %d, PC used as index",i); return;}
		if (atm==AREG) {
			yyerror("arg %d, indexing the register file",i); return;}
		if (atm==AIMM) {
			yyerror("arg %d, indexing a constant",i); return;}
		if (((atm==ADECR) || (atm==AINCR)) && (act->areg1==act->areg2)) {
			yyerror("arg %d, indexing with modified register",i); return;}
	}
}

int d124 = {4};
int len124[] = {0,LEN1,LEN2,0,LEN4};
char mod124[] = {0,0x00,0x20,0,0x40};

putins(op, ap, n)
register struct arg *ap;
{
/* op had better be positive */
	register struct exp *xp;
	register int a; int i,xtrab;

	if (passno!=2) {
		dotp->xvalue += n+1;	/* 1 for the opcode, at least 1 per arg */
		for (i=0; i<n; i++,ap++) {/* some args take more than 1 byte */
			a=ap->atype;
			if (a&AINDX) dotp->xvalue++;
			switch (a&~(AINDX|ASTAR)) {
				case AEXP: {
					a=itab[op]->argtyp[i];
					if (a==ACCB+TYPB) break;
					if (a==ACCB+TYPW) {dotp->xvalue++; break;}
					dotp->xvalue += d124; break;
				}
				case ADISP: {
					xp=ap->xp;
					if ((xp->xtype&XTYPE)!=XABS || xp->xtype&XFORW)
						{dotp->xvalue += d124; break;}
					if (xp->xvalue==0 && !(a&ASTAR)) break;
					dotp->xvalue++;
					if ((xp->xvalue<-128) || (xp->xvalue>127)) dotp->xvalue++;
					if ((xp->xvalue<-32768) || (xp->xvalue>32767))
						dotp->xvalue += 2;
					break;
				}
				case AIMM: {
					if (ap->atype&ASTAR) a=TYPL;
					else {
						xp=ap->xp;
						if ((xp->xtype&XTYPE)==XABS && !(xp->xtype&XFORW)
							&& xp->xvalue>=0 && xp->xvalue<=63) break;
						a=itab[op]->argtyp[i];
						if (a&ACCA) a=TYPL; else a &= TYPMASK;
					}
					switch (a) {
						case TYPD: case TYPF:
							if (xp->yvalue==0 &&
							   (xp->xvalue & 0x000043f0)==xp->xvalue) break;
							if (a==TYPF) dotp->xvalue -= 4;
						case TYPQ: dotp->xvalue += 4;
						case TYPL: dotp->xvalue += 2;
						case TYPW: dotp->xvalue++;
						case TYPB: dotp->xvalue++;
					}
				}
			}
		}
		return;
	}
	/* pass2 here */
	outb(op); /* the opcode */
	for (i=0; i<n; i++,ap++) {/* now for the arguments */
		a=ap->atype; xp=ap->xp; xtrab=0;
		if (a&AINDX) {outb(0x40 | ap->areg2); a &= ~AINDX;}
		if (a&ASTAR) {ap->areg1 |= 0x10; a &= ~ASTAR;}
		switch (a) {
			case AREG:  ap->areg1 |= 0x50; break; /* %r */
			case ABASE: ap->areg1 |= 0x60; break; /* (%r) */
			case ADECR: ap->areg1 |= 0x70; break; /* -(%r) */
			case AINCR: ap->areg1 |= 0x80; break; /* (%r)+ */
			case AEXP: {/* expr */
				a=itab[op]->argtyp[i];
				if (a==ACCB+TYPB) {
					ap->areg1=a=xp->xvalue - dotp->xvalue -1;
					if (a<-128 || a>127) yyerror("Branch too far"); break;
				}
				if (a==ACCB+TYPW) {
					ap->areg1=a=xp->xvalue -= dotp->xvalue +2; xp->xtype=XABS;
					if (a<-32768 || a>32767) yyerror("Branch too far");
					xp->xvalue = a>>8; xtrab=LEN1; break;
				}
				/* reduces to expr(pc) mode */
				ap->areg1 |= (0xAF+mod124[d124]); xtrab=len124[d124]+PCREL; break;
			}
			case ADISP: {/* expr(%r) */
				ap->areg1 |= 0xA0;
				if ((xp->xtype&XTYPE)!=XABS || xp->xtype&XFORW)
					{ap->areg1 += mod124[d124]; xtrab=len124[d124]; break;}
				if (xp->xvalue==0 && !(ap->areg1&0x10)) {ap->areg1 ^= 0xC0; break;}
				xtrab=LEN1;
				if ((xp->xvalue<-128) || (xp->xvalue>127))
					{ap->areg1 += 0x20; xtrab=LEN2;}
				if ((xp->xvalue<-32768) || (xp->xvalue>32767))
					{ap->areg1 += 0x20; xtrab=LEN4;}
				break;
			}
			case AIMM: { /* $expr */
				if (ap->atype&ASTAR) a=TYPL;
				else {
					if ((xp->xtype&XTYPE)==XABS && !(xp->xtype&XFORW) &&
						xp->xvalue>=0 && xp->xvalue<=63) {ap->areg1=xp->xvalue; break;}
					a=itab[op]->argtyp[i];
					if (a&ACCA) a=TYPL; else a &= TYPMASK;
				}
				ap->areg1 |= 0x8F;
				switch (a) {
					case TYPD: case TYPF:
						if (xp->yvalue==0 &&
						   (xp->xvalue & 0x000043f0)==xp->xvalue) {
							ap->areg1 = xp->xvalue>>4; break;
						}
						if (a==TYPF) {xtrab = LEN4; break;}
					case TYPQ: xtrab = LEN8; break;
					case TYPL: xtrab = LEN4; break;
					case TYPW: xtrab = LEN2; break;
					case TYPB: xtrab = LEN1; break;
				}
			}
		}
		outb(ap->areg1);/* first byte to describe arg */
		if (xtrab) outrel(&xp->xvalue,xtrab,xp->xtype,xp->xname);
	}
}

get2(f)
register FILE *f;
{
	short r;
	register char *p;
	r = 0;
	p = (char *)&r;
	*p++ = getc(f);
	*p = getc(f);
	return(r);
}

put2(v, f)
register FILE *f;
short v;
{
	register char *p;
	short lv;

	p = (char *)&lv;
	lv = v;
	putc(*p++, f);
	putc(*p, f);
}

setindices() {
	register int i;
	register struct symtab *sp;
	
	i = 0;
	for (sp=symtab; sp<nextsym; sp++) {
		if ((((struct tshdr *)sp)->tsflag&0xFF)<0xFE) {
			sp->index = i++;
		}
	}
}
