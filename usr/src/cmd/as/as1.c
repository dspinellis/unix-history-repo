#
char AS1[] = "@(#)as1.c 1.21 79/03/21 22:27:27";	/* sccs ident */
#include <stdio.h>
#include <signal.h>
#include <a.out.h>
#include "as.h"
#include "as.yh"

int	curlen;
int	lineno	= 1;
long	datbase;
struct	symtab *nextsym,*rovjxxx;
char *endcore;
struct	hdr hdr = {
	0410, 0, 0, 0, 0, 0, 0, 0,
};

#ifndef vax
	struct {short hiword; short loword;}; /* stupid fp-11 */
#endif

writel(p,n,f) long *p; FILE *f; {
#ifdef vax
	fwrite(p,sizeof(*p),n,f);
#else
	while (n--) {
		fwrite(&(*p).loword,2,1,f);
		fwrite(&(*p).hiword,2,1,f);
		p++;
	}
#endif
}

symwrite(p,n,f) struct symtab *p; FILE *f; {
#ifdef vax
	while (n--) {
		fwrite(&(p->name[0]), sizeof(symtab[0].name), 1, f);
		fwrite(p->ptype ? &(p->ptype) : &(p->type),
			sizeof(symtab[0].type), 1, f);
	 	fwrite(&(p->other), sizeof(symtab[0].other), 1, f);
	 	fwrite(&(p->desc), sizeof(symtab[0].desc), 1, f);
	 	fwrite(&(p->value), sizeof(symtab[0].value), 1, f);
		p++;
	}
#else
	while (n--) {
		fwrite(p,sizeof(symtab[0])-sizeof(p->value),1,f);
		writel(&(p->value),1,f); p++;
	}
#endif
}

char	*tmpn1;
char	*tmpn2;
char	*tmpn3;
struct	exp	usedot[NLOC+NLOC];
FILE	*usefile[NLOC+NLOC];
FILE	*rusefile[NLOC+NLOC];
extern char _sibuf[BUFSIZ],_sobuf[BUFSIZ];
extern short njxxx;
extern int d124;

main(argc, argv)
char **argv;
{
	int symcmp();
	register struct instab *ip;
	register struct symtab *sp;
	int c;
	long v;
	register struct symtab **hp;
	char *outfile = "a.out";
	int infound = 0;
	int delexit();

	while (argc > 1) {
		if (argv[1][0]=='-' && argv[1][1]=='o') {
			if (argc <3) {
				yyerror("-o what?");
				exit(1);
			}
			outfile = argv[2];
			argc -= 2;
			argv += 2;
			continue;
		}
		if (argv[1][0]=='-' && argv[1][1]=='d') {
			d124=argv[1][2]-'0';
			if (d124!=1 && d124!=2 && d124!=4) {
				yyerror("-d[124] only"); exit(1);
			}
			argc--; argv++; continue;
		}
		if (infound) {
			yyerror(">1 argument.");
			exit(1);
		}
		infound++;
		if (freopen(argv[1], "r", stdin) == NULL) {
			yyerror("as: Can't open %s\n", argv[1]);
			exit(2);
		}
		setbuf(stdin,_sibuf);
		argc--;
		argv++;
	}
	nextsym=symtab=endcore=sbrk(0); rovjxxx= nextsym-1;
	c = -1;
	for (ip=instab; ip->name[0]!=0; ip++) {
		register char *p1, *p2;
		for (p1=ip->name,p2=yytext; p2<yytext+NCPS;)
			*p2++ = *p1++;
		*p2++ = 0;
		usrname = 0;
		hp = lookup(0);
		if (*hp==NULL) {
			*hp = ip;
			if (ip->tag!=(INSTn-256) && ip->tag!=(INST0-256) && ip->tag!=0)
				continue; /* was pseudo-op */
/*			c++;
/*			if (c>=NINST) {
/*				yyerror("Instruction table overflow");
/*				continue;
/*			}
*/
			itab[ip->opcode&0xFF] = ip; /* given opcode, find instruction */
		}
	}
	for (c=0; c<NLOC; c++) {
		usedot[c].xtype = XTEXT;
		usedot[c+NLOC].xtype = XDATA;
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, delexit);
	tmpn1 = mktemp("/tmp/asXXXXX");
	tmpfil = fopen(tmpn1, "w");
	if (tmpfil==NULL) {
		yyerror("Bad temp1file");
		delexit();
	}
	setbuf(tmpfil,_sobuf);
	yyparse();
	if (anyerrs)
		delexit();
	jxxxfix();
	stabfix();
	fclose(tmpfil);
	tmpfil = fopen(tmpn1, "r");
	if (tmpfil==NULL) {
		yyerror("Bad tmp1file (r)");
		delexit();
	}
	setbuf(tmpfil,_sibuf);
	/* round and assign text segment origins */
	tsize = 0;
	for (c=0; c<NLOC; c++) {
		v = round(usedot[c].xvalue, FW);
		usedot[c].xvalue = tsize;
		tsize += v;
	}
	/* round and assign data segment origins */
	datbase = round(tsize, PAGRND);
	for (c=0; c<NLOC; c++) {
		v = round(usedot[NLOC+c].xvalue, FW);
		usedot[NLOC+c].xvalue = datbase+dsize;
		dsize += v;
	}
	hdr.bsize = dsize;
	/* assign final values to symbols */
	for (sp=symtab; sp<nextsym; sp++) {
		if ((sp->name[0]&0xFF)>=0xFE) continue; /* ignore jxxx entries */
		if ((sp->type&XTYPE)==XUNDEF)
			sp->type = XXTRN+XUNDEF;
		else if ((sp->type&XTYPE)==XDATA)
			sp->value += usedot[sp->index].xvalue;
		else if ((sp->type&XTYPE)==XTEXT)
			sp->value += usedot[sp->index].xvalue;
		else if ((sp->type&XTYPE)==XBSS) {
			long bs;
			bs = sp->value;
			sp->value = hdr.bsize + datbase;
			hdr.bsize += bs;
		}
	}
	hdr.bsize -= dsize;
	tmpn2 = mktemp("/tmp/aaatXXXXX");
	txtfil = fopen(outfile, "w");
	if (txtfil==NULL) {
		yyerror("Cannot create %s", outfile);
		delexit();
	}
	setbuf(txtfil,_sobuf);
	usefile[0] = txtfil;
	tmpn3 = mktemp("/tmp/abatXXXXX");
	relfil = fopen(tmpn3, "w");
	if (relfil==NULL) {
		yyerror("temp file can't be opened");
		delexit();
	}
	rusefile[0] = relfil;
	hdr.tsize = tsize;
	hdr.dsize = dsize;
	hdr.ssize = 
		(sizeof(symtab[0])-
		(sizeof(symtab[0].tag)+sizeof(symtab[0].index)+
			sizeof(symtab[0].ptype)))*
		(nextsym-symtab-njxxx);
	/* hdr.trsize, hdr.drsize set by outrel */
	writel(&hdr,8,txtfil);
	tsize = 0;
	dsize = 0;
	lineno = 1;
	dotp = &usedot[0];
	passno = 2;
/*
	for (sp=symtab; sp<nextsym; sp++)
		sp->index = 20000;
*/
	setindices();
	yyparse();
	/* round csects to FW */
	for (c=0; c<NLOC; c++) {
		if (usefile[c]) {
			txtfil=usefile[c]; dotp= &usedot[c];
			while (usedot[c].xvalue&FW) outb(0);
			if (c>0)
				fclose(usefile[c]);
			fclose(rusefile[c]);
		}
		if (usefile[NLOC+c]) {
			txtfil = usefile[NLOC+c]; dotp= &usedot[c+NLOC];
			relfil = rusefile[NLOC+c];
			while (usedot[c+NLOC].xvalue&FW) outb(0);
			fclose(txtfil);
			fclose(relfil);
		}
	}
	txtfil = usefile[0];
	/* append csect text onto text for csect 0 */
	for (c=1; c<NLOC+NLOC; c++) {
		register ch;
		if (usefile[c]) {
			tmpn2[TMPC] = c+'a';
			relfil = fopen(tmpn2, "r");
			if (relfil==NULL) {
				yyerror("cannot reopen temp");
				continue;
			}
			while ((ch = getc(relfil))>=0)
				putc(ch, txtfil);
			fclose(relfil);
		}
	}
	/* append relocation info onto text */
	for (c=0; c<NLOC+NLOC; c++) {
		register ch;
		if (rusefile[c]) {
			tmpn3[TMPC] = c+'a';
			relfil = fopen(tmpn3, "r");
			if (relfil==NULL) {
				yyerror("cannot reopen temp");
				continue;
			}
			while ((ch = getc(relfil))>=0)
				putc(ch, txtfil);
			fclose(relfil);
		}
	}
	remjxxx();
/*
	qsort(symtab, nextsym-symtab, sizeof(symtab[0]), symcmp);
*/
	for (sp=symtab; sp<nextsym; sp++) {
		sp->type &= ~XFORW;
		sp->index = 0;
	}
	symwrite(symtab, nextsym-symtab, txtfil);
	fseek(txtfil,0L,0); /* a little half-passedness here to get rsize correct */
	writel(&hdr,8,txtfil);
	delete();
	if (anyerrs==0 && orgwarn)
		yyerror("Caution: absolute origins.\n");
	exit(anyerrs!=0);
}


delexit()
{
	delete();
	exit(1);
}

delete()
{
	register c;

	if (tmpn1)
		unlink(tmpn1);
	for (c=0; c<NLOC+NLOC; c++) {
		if (tmpn2) {
			tmpn2[TMPC] = c+'a';
			unlink(tmpn2);
		}
		if (tmpn3) {
			tmpn3[TMPC] = c+'a';
			unlink(tmpn3);
		}
	}
}

struct symtab *
symalloc() {
if((char *)++nextsym>=endcore) {
	register int *p;
	if(-1==sbrk(200*sizeof(*symtab)))
		{yyerror("Memory overflow"); delexit();}
	p=endcore += 200*sizeof(*symtab); while (p>(char *)nextsym) *--p=0;
}
/*if(++nextsym-symtab>NSYM) yyerror("Symbol table overflow");
*/
return(nextsym-1);
}

struct symtab **
lookup(instflg)
{
	register int ihash; int un;
	register struct symtab **hp;
	register char *p1, *p2;
	static struct symtab *local;

	if (lclname!=0) {
		lclname=0;
		*(hp= &local) = symalloc();
		p1=yytext; p2=(*hp)->name;
		while (*p2++ = *p1++);
		return(hp);
	}
	un = usrname;
	usrname = 1;
	ihash = 0;
	p1 = yytext;
	while (*p1) {
		ihash += ihash + *p1++;
	}
	ihash &= 077777;
	while (p1<yytext+NCPS)
		*p1++ = 0;
	hp = &hshtab[ihash%NHASH];
	while (*hp) {
		if (hp>=symtab && (*hp)->ptype) {
			printf("IGNORING LOOKUP %s %x %x %x\n",
			(*hp)->name, (*hp)->ptype, (*hp)->type, (*hp)->type & STABTYPS);
			goto no;
			}
		p2 = (*hp)->name;
		for (p1=yytext; p1<yytext+NCPS;)
			if (*p1++ != *p2++)
				goto no;
/*		if (un == ((*hp)->tag==0)) */
			return(hp);
	no:
		if (++hp >= &hshtab[NHASH])
			hp = hshtab;
	}
	if(++hshused >= NHASH) {
		yyerror("Symbol table overflow");
		delexit();
	}
	if (instflg) {
		*hp = symalloc();
		for (p1=yytext,p2=(*hp)->name; p1<yytext+NCPS;)
			*p2++ = *p1++;
	}
	return(hp);
}

outb(val) {dotp->xvalue++; if (passno==2) fwrite(&val,1,1,txtfil);}

int reflen[] = {0,0,1,1,2,2,4,4,8,8};

outrel(pval,reftype,reltype,xsym)
long *pval; register int reftype,reltype; struct symtab *xsym; {
/* reftype: PCREL or not, plus length LEN1, LEN2, LEN4, LEN8
/* reltype: csect ("segment") number (XTEXT, XDATA, ...) associated with 'val'
/* xsym: symbol table pointer
*/
long ts; char tc;
long tl;
short t;
if (passno!=2) {
	dotp->xvalue += reflen[reftype]; return;
}
if (bitoff&07) yyerror("Padding error");
reltype &= ~XFORW;
if (reltype==XUNDEF) yyerror("Undefined reference");
if (reltype!=XABS || reftype&PCREL) {
	/* write the address portion of a relocation datum */
	if (dotp>= &usedot[NLOC]) {
		hdr.drsize += sizeof(dotp->xvalue) + 3 + sizeof tc;
		tl=dotp->xvalue-datbase; writel(&tl,1,relfil);
	} else {
		hdr.trsize += sizeof(dotp->xvalue) + 3 + sizeof tc;
		writel(&dotp->xvalue,1,relfil);
	}
	/* write the properties portion of a relocation datum */
	if (reltype==XXTRN+XUNDEF) {
		setindex(xsym);
		ts=(xsym->index);
		tc = (XXTRN<<3)|(reftype-LEN1);
	} else if ((reltype&XTYPE)==XUNDEFO) {
		setindex(xsym);
		ts=(xsym->index);
		tc = ((XXTRN+2)<<3)|(reftype-LEN1);
	} else  {
		ts=(reltype);
		tc = (reftype-LEN1);
	}
	fwrite((char *)&ts, 3, 1, relfil);
	fwrite(&tc, sizeof(tc), 1, relfil);
}
/* write the raw ("unrelocated") value to the text file */
t=reflen[reftype]; dotp->xvalue += t;
if (reftype&PCREL) *pval -= dotp->xvalue;
#ifdef vax
	fwrite(pval,1,t,txtfil);
#else
	if (t>2) {
		fwrite(&((*pval).loword),1,2,txtfil);
		fwrite(&((*pval).hiword),1,t-2,txtfil);
	} else fwrite(&((*pval).loword),1,t,txtfil);
#endif
}

setindex(xsym)
register struct symtab *xsym;
{
	return;
/*	
	if (xsym->index == 20000)
		xsym->index = gindex++;
	if (gindex >= 1023)
		yyerror("Too many external symbols");
*/
}

flushfield(n)
{
	if (bitoff==0) return;
	n=((bitoff+n-1)/n)*n;
	while (n>0) {outb((int)bitfield); bitfield >>= 8; n -= 8;}
	bitoff=0; bitfield=0;
}

putflt(fp, s)
double *fp;
{fwrite(fp,1,s,txtfil);}

symcmp(p, q)
register struct symtab *p, *q;
{
	if (p->index < q->index)
		return(-1);
	if (p->index > q->index)
		return(1);
	if (p->value < q->value)
		return(-1);
	if (p->value > q->value)
		return(1);
	return(0);
}

stabfix() {
	register struct symtab *sp;
	register struct symtab *p;
	
	for(sp=symtab; sp<nextsym; sp++) {
		if(sp->ptype && (sp->type & 0200)) {
			p = sp->value;
			sp->value = p->value;
			sp->index = p->index;
			sp->type = p->type;
/*
printf("STABFIX: %s (old %s) to %d offsets %d %d\n", sp->name, p->name, sp->value, sp, p);
*/
/*
			sp->ptype = (sp->ptype & STABTYPS) | (p->type & N_EXT);
*/
		}
	}
}
