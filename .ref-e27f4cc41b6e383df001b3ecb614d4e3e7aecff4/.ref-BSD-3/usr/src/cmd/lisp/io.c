#include "global.h"
#include <stdio.h>
#include <ctype.h>
#include "chars.h"

struct readtable {
char	ctable[132];
} initread = {
/*	^@ nul	^A soh	^B stx	^C etx	^D eot	^E eng	^F ack	^G bel  */
	VERR,	VERR,	VERR,	VERR,	VERR,	VERR,	VERR,	VERR,
/*	^H bs	^I ht	^J nl	^K vt	^L np	^M cr	^N so	^O si	*/
	VCHAR,	VSEP,	VSEP,	VSEP,	VSEP,	VSEP,	VERR,	VERR,
/*	^P dle	^Q dc1	^R dc2	^S dc3	^T dc4	^U nak	^V syn	^W etb	*/
	VERR,	VERR,	VERR,	VERR,	VERR,	VERR,	VERR,	VERR,
/*	^X can	^Y em	^Z sub	^[ esc	^\ fs	^] gs	^^ rs	^_ us	*/
	VERR,	VERR,	VERR,	VSEP,	VERR,	VERR,	VERR,	VERR,
/*	sp	!	"	#	$	%	&	'	*/
	VSEP,	VCHAR,	VDQ,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VSQ,
/*	(	)	*	+	,	-	.	/	*/
	VLPARA,	VRPARA,	VCHAR,	VSIGN,	VCHAR,	VSIGN,	VPERD,	VCHAR,
/*	0	1	2	3	4	5	6	7	*/
	VNUM,	VNUM,	VNUM,	VNUM,	VNUM,	VNUM,	VNUM,	VNUM,
/*	8	9	:	;	<	=	>	?	*/
	VNUM,	VNUM,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,
/*	@	A	B	C	D	E	F	G	*/
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,
/*	H	I	J	K	L	M	N	O	*/
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,
/*	P	Q	R	S	T	U	V	W	*/
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,
/*	X	Y	Z	[	\	]	^	_	*/
	VCHAR,	VCHAR,	VCHAR,	VLBRCK,	VESC,	VRBRCK,	VCHAR,	VCHAR,
/*	`	a	b	c	d	e	f	g	*/
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,
/*	h	i	j	k	l	m	n	o	*/
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,
/*	p	q	r	s	t	u	v	w	*/
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,
/*	x	y	z	{	|	}	~	del	*/
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VEOF,
/*	unused	unused	Xesc	Xdqc					*/
	0,	0,	'\\',	'"'
};

char *ctable = initread.ctable;
lispval atomval;	/* external varaible containing atom returned
			   from internal atom reading routine */
lispval protect();
lispval unprotect();
lispval readrx(); lispval readr(); lispval readry();
int keywait;
static int dbqflag;
static int macflag;
static int splflag;
static int mantisfl = 0;
lispval	lastrtab;	/* external variable designating current reader
			   table */
static char baddot1[]=
"Bad reader construction: (. <something>)\nShould be (nil . <something>)\n";
static char baddot2[]=
"Bad reader construction: (<something> .)\n\
Should be (<something> . <something>), assumed to be (<something>)";
static char baddot3[]=
"Bad reader construction: (<something> . <something> not followed by )";

#include "chkrtab.h"
/* readr ****************************************************************/
/* returns a s-expression read in from the port specified as the first	*/
/* argument.  Handles superbrackets, reader macros.			*/
lispval
readr(useport)
FILE *useport;
{
	register lispval handy = Vreadtable->clb;

	chkrtab(handy);
	rbktf = FALSE;
	rdrport = (FILE *) useport;
	if(useport==stdin)
		keywait = TRUE;	
	handy = readrx(Iratom());
	if(useport==stdin)
		keywait = FALSE;
	return(handy);

}


/* readrx **************************************************************/
/* returns a s-expression beginning with the syntax code of an atom	*/
/* passed in the first	*/
/* argument.  Does the actual work for readr, including list, dotted	*/
/* pair, and quoted atom detection					*/
lispval
readrx(code)
register int code;
{
	register lispval work;
	register lispval *current;
	register struct argent *result;
	register struct argent *lbot, *np;
	int inlbkt = FALSE;
	lispval errorh();

top:
	switch(code)
	{
	case TLBKT:
		inlbkt = TRUE;
	case TLPARA:
		result = np;
		current = (lispval *)np;
		np++->val = nil; /*protect(nil);*/
		for(EVER) {
			switch(code = Iratom())
			{
			case TRPARA:
				if(rbktf && inlbkt)
					rbktf = FALSE;
				return(result->val);
			default:
				atomval = readrx(code);
			case TSCA:
				np++->val=atomval;
				*current = work = newdot();
				work->car = atomval;
				np--;
				current = (lispval *) &(work->cdr);
				break;
			case TSPL:
				macrox(); /* input and output in atomval */
				*current = atomval;
				while(*current!=nil) {
					if(TYPE(*current)!=DTPR)
						errorh(Vermisc,"Non-list returned from splicing macro",nil,FALSE,7,*current);
					current=(lispval *)&((*current)->cdr);
				}
				break;
			case TPERD:
				if(result->val==nil) {
					work = result->val=newdot();
					current = (lispval *) &(work->cdr);
					fprintf(stderr,baddot1);
				}
				code = Iratom();
				if(code==TRPARA) {
					return(errorh(Vermisc,baddot2,nil,TRUE,58,result->val));
				}
				*current = readrx(code);
				if((code = Iratom())!=TRPARA) {
					errorh(Vermisc,baddot3,nil,TRUE,59,result->val,atomval);
				}
				if(rbktf && inlbkt)
					rbktf = FALSE;
				return(result->val);
			case TEOF:
				clearerr(rdrport);
				error("Premature end of file.", FALSE);
			}
			if(rbktf) {
				if(inlbkt)
					rbktf = FALSE;
				return(result->val);
			}
		}
	case TSCA:
		return(atomval);
	case TEOF:
		return(eofa);
	case TMAC:
		macrox();
		return(atomval);
	case TSPL:
		macrox();
		if((work = atomval)!=nil) {
			if(TYPE(work)==DTPR && work->cdr==nil)
				return(work->car);
			else
				errorh(Vermisc,
"Improper value returned from splicing macro at top-level",nil,FALSE,9,work);
		}
		code = Iratom();
		goto top;
		/* return(readrx(Iratom())); */
	case TSQ:
		result = np;
		protect(newdot());
		(work = result->val)->car = quota;
		work = work->cdr = newdot();
		work->car = readrx(Iratom());
		return(result->val);
	default:
		return(error("Readlist error",FALSE));
	}
}
macrox()
{
	lispval Lapply();

	snpand(0);
	lbot = np;
	protect(Iget(atomval,macro));
	protect(nil);
	atomval = Lapply();
	return;
}



/* ratomr ***************************************************************/
/* this routine returns a pointer to an atom read in from the port given*/
/* by the first argument						*/
lispval
ratomr(useport)
register FILE	*useport;
{
	rdrport = useport;
	switch(Iratom())
	{
	case TEOF:
		return(eofa);
	case TSQ:
	case TRPARA:
	case TLPARA:
	case TLBKT:
	case TPERD:
		strbuf[1]=0;
		return(getatom());
	default:
		return(atomval);
	}
}
Iratom()
{
	register FILE	*useport = rdrport;
	register char	c, marker, *name;
	extern lispval finatom(), calcnum(), getnum();
	char	positv = TRUE;
	int code;
	int strflag = FALSE;

	name = strbuf;

again:	c = getc(useport) & 0177;
	*name = c;

	switch(ctable[c] & 0377) {

	default:	goto again;

	case VNUM:

	case VSIGN:	*name++ = c;
			atomval = (getnum(name));
			return(TSCA);

	case VESC:
			dbqflag = TRUE;
			*name++ = getc(useport) & 0177;
			atomval = (finatom(name));
			return(TSCA);
			
	case VCHAR:
			*name++ = c;
			atomval = (finatom(name));
			return(TSCA);

	case VLPARA:	return(TLPARA);

	case VRPARA:	return(TRPARA);

	case VPERD:	c = peekc(useport);
			if(VNUM!=ctable[c])
				return(TPERD);
			*name++ = '.';
			mantisfl = 1;
			atomval = (getnum(name));
			return(TSCA);

	case VLBRCK:	return(TLBKT);

	case VRBRCK:	rbktf = TRUE;
			return(TRPARA);

	case VEOF:	/*printf("returning eof atom\n");*/
			return(TEOF);

	case VSQ:	return(TSQ);

	case VSD:	strflag = TRUE;
	case VDQ:	name = strbuf;
			marker = c;
			while ((c = getc(useport)) != marker) {

				if(VESC==ctable[c]) c = getc(useport);
				*name++ = c;
				if (name >= endstrb)
					error("ATOM TOO LONG",FALSE);
				if (feof(useport)) {
					clearerr(useport);
					error("EOF ecountered while reading atom", FALSE);
				}
			}
			*name = NULL_CHAR;
			if(strflag)
				atomval = (lispval) inewstr(strbuf);
			else
				atomval = (getatom(name));
			return(TSCA);

	case VERR:	if (c == '\0') goto same;	/* null pname */
			fprintf(stderr,"%c (%o): ",c,(int) c);
			error("ILLEGAL CHARACTER IN ATOM",TRUE);

	case VSPL:
		code = TSPL;
		goto same;
	case VMAC:
		code = TMAC;
		goto same;
	case VSCA:
		code = TSCA;
	same:
		strbuf[0] = c;
		strbuf[1] = 0;
		atomval = (getatom());
		return(code);
	}
}

#define push();	if(name==endstrb) error("Int too long",FALSE); else *name++=c;
#define next()	(stats = ctable[c=getc(useport) & 0177])

lispval
getnum(name)
register char *name;
{
	register char c;
	register lispval result;
	register FILE *useport=rdrport;
	char  stats;
	double realno;
	extern lispval finatom(), calcnum(), newdoub(), dopow();

	if(mantisfl) {
		mantisfl = 0;
		next();
		goto mantissa;
	}
	while(VNUM==next()) {
		push();		/* recognize [0-9]*, in "ex" parlance */
	}
	if(stats==VPERD) {
		push();		/* continue */ 
	} else if(stats & SEPMASK) {
		ungetc(c,useport);
		return(calcnum(strbuf,name,ibase->clb->i));
	} else if(c=='^') {
		push();
		return(dopow(name,ibase->clb->i));
	} else if(c=='_') {
		push();
		return(dopow(name,2));
	} else{
		ungetc(c,useport);
		return(finatom(name));
	}
				/* at this point we have [0-9]*\. , which might
				   be a decimal int or the leading part of a
				   float				*/
	if(next()!=VNUM) {
		if(c=='e' || c=='E' || c=='d' ||c=='D')
			goto expt;
		else if(c=='^') {
			push();
			return(dopow(name,ibase->clb->i));
		} else if(c=='_') {
			push();
			return(dopow(name,2));
		} else {
				/* Here we have 1.x where x not num, not sep */
				/* Here we have decimal int. NOT FORTRAN! */
			ungetc(c,useport);
			return(calcnum(strbuf,name-1,10));
		}
	}
mantissa:
	do {
		push();
	} while (VNUM==next());
				/* Here we have [0-9]*\.[0-9]* */
	if(stats & SEPMASK)
		goto last;
	else if(c!='e' && c!='E' && c!='d' && c!='D') {
		ungetc(c,useport);
		goto verylast;
	}
expt:	push();
	next();
	if(c=='+' || c =='-') {
		push();
		next();
	}
	while (VNUM==stats) {
		push();
		next();
	}
last:	ungetc(c,useport);
	if(! (stats & SEPMASK) )
		return(finatom(name));

verylast:
	*name=0;
	sscanf(strbuf,"%F",&realno);
	(result = newdoub())->r = realno;
	return(result);
}

lispval
dopow(part2,base)
lispval base;
char *part2;
{
	register char *name = part2;
	register char c;
	register FILE *useport = rdrport;
	register int power;
	register struct argent *lbot, *np;
	char stats;
	char *end1 = part2 - 1; lispval Ltimes();

	while(VNUM==next()) {
		push();
	}
	if(c!='.') {
		ungetc(c,useport);
	}
	if(c!='.' && !(stats & SEPMASK)) {
		return(finatom(name));
	}
	lbot = np;
	np++->val = inewint(base);
	/* calculate "mantissa"*/
	if(*end1=='.')
		np++->val = calcnum(strbuf,end1-1,10);
	else
		np++->val = calcnum(strbuf,end1,ibase->clb->i);

	/* calculate exponent */
	if(c=='.')
		power = calcnum(part2,name,10)->i;
	else
		power = calcnum(part2,name,ibase->clb->i)->i;
	while(power-- > 0)
		lbot[1].val = Ltimes();
	return(lbot[1].val);
}
	

lispval
calcnum(strbuf,name,base)
char *name;
char *strbuf;
{
	register char *p;
	register lispval result, temp;
	int negflag = 0;

	temp = rdrsdot;			/* initialize sdot cell */
	temp->CDR = nil;
	temp->i	  = 0;
	p = strbuf;
	if(*p=='+') p++;
	else if(*p=='-') {negflag = 1; p++;}
	*name = 0;
	if(p>=name) return(getatom());

	for(;p < name; p++)
		dmlad(temp,base,*p-'0');
	if(negflag)
		dmlad(temp,-1,0);

	if(temp->CDR==0) {
		result = inewint(temp->i);
		return(result);
	} else {
		(result = newsdot())->i = temp->i;
		result->CDR = temp->CDR;
		temp->CDR = 0;
	}
	return(result);
}
lispval
finatom(name)
register char *name;
{
	extern int uctolc;
	register FILE *useport = rdrport;
	register char c, stats;
	register char *savenm;
	savenm = name - 1;	/* remember start of name */
	while(!(next()&SEPMASK)) {

		if(stats == VESC) c = getc(useport) & 0177;
		*name++=c;
		if (name >= endstrb)
			error("ATOM TOO LONG",FALSE);
	}
	*name = NULL_CHAR;
	ungetc(c,useport);
	if (uctolc) for(; *savenm ; savenm++) 
			if( isupper(*savenm) ) *savenm = tolower(*savenm);
	return(getatom());
}

/* printr ***************************************************************/
/* prints the first argument onto the port specified by the second 	*/
printr(a,useport)
register lispval a;
register FILE *useport;
{
		register lispval temp;
		char strflag = 0;
		char Idqc = 0;


val_loop:
	if( ! VALID(a) )
		{
		error("BAD LISP DATA ENCOUNTERED BY PRINTR",TRUE);
		a = badst;
		}

	switch (TYPE(a))	{


	case UNBO:	fputs("<UNBOUND>",useport);
			break;

	case VALUE:	fputs("(ptr to)",useport);
			a = a->l;
			goto val_loop;

	case INT:	fprintf(useport,"%d",a->i);
			break;

	case DOUB:	fprintf(useport,"%0.16G",a->r);
			break;

	case PORT:	fputs("port",useport);
			break;

	case ARRAY:	fputs("array[",useport);
			printr(a->length,useport);
			fputs("]",useport);
			break;

	case BCD:	fprintf(useport,"#%X-",a->entry);
			printr(a->discipline,useport);
			break;

	case SDOT:	pbignum(a,useport);
			break;

	case DTPR:	if(a->car==quota && a->cdr!=nil 
			    && a->cdr->cdr==nil) {
				putc('\'',useport);
				printr(a->cdr->car,useport);
				break;
			}
			putc('(',useport);
	morelist:	printr(a->car,useport);
			if ((a = a->cdr) != nil)
				{
				putc(' ',useport);
				if (TYPE(a) == DTPR) goto morelist;
				fputs(". ",useport);
				printr(a,useport);
				}
			fputc(')',useport);
			break;

	case STRNG:	strflag = TRUE;
			Idqc = Xsdc;

	case ATOM:	{
			char	*front, *temp; int clean;
			temp = front = (strflag ? ((char *) a) : a->pname);
			if(Idqc==0) Idqc = Xdqc;

			if(Idqc) {
				clean = *temp;
				if (*temp == '-') temp++;
				clean = clean && (ctable[*temp] != VNUM);
				while (clean && *temp)
					clean = (!(ctable[*temp++] & QUTMASK));
				if (clean)
					fputs(front,useport);
				else	 {
					putc(Idqc,useport);
					for(temp=front;*temp;temp++) {
						if(  *temp==Idqc
						  || ctable[*temp] == VESC)
							putc(Xesc,useport);
						putc(*temp,useport);
					}
					putc(Idqc,useport);
				}

			}  else {
				register char *cp = front;

				if(ctable[*cp]==VNUM)
					putc(Xesc,useport);
				for(; *cp; cp++) {
					if(ctable[*cp]& QUTMASK)
						putc(Xesc,useport);
					putc(*cp,useport);
				}
			
			}
					
		}
	}
}

/* dmpport ****************************************************************/
/* outputs buffer indicated by first argument whether full or not	*/
dmpport(useport)
register lispval useport;
	{
	fflush(useport);
}

/*  protect and unprot moved to eval.c  (whr)  */
