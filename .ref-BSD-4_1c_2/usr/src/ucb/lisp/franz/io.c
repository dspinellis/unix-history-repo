#ifndef lint
static char *rcsid =
   "$Header: /na/franz/franz/RCS/io.c,v 1.2 83/02/14 20:30:13 jkf Exp $";
#endif

/*					-[Mon Feb 14 20:17:35 1983 by jkf]-
 * 	io.c				$Locker:  $
 * input output functions
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include "global.h"
#include <ctype.h>
#include "chars.h"
#include "chkrtab.h"

struct readtable {
unsigned char	ctable[132];
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
	VSEP,	VCHAR,	VSD,	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VSQ,
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
	VCHAR,	VCHAR,	VCHAR,	VCHAR,	VDQ,	VCHAR,	VCHAR,	VERR,
/*	unused	Xsdc	Xesc	Xdqc					*/
	0,	'"',	'\\',	'|'
};

extern unsigned char *ctable;
lispval atomval;	/* external varaible containing atom returned
			   from internal atom reading routine */
lispval readrx(); lispval readr(); lispval readry();
int keywait;
int plevel = -1;	/* contains maximum list recursion count	*/
int plength = -1;   /* maximum number of list elements printed	*/
static int dbqflag;
static int mantisfl = 0;
extern lispval	lastrtab;	/* external variable designating current reader
			   table */
static char baddot1[]=
"Bad reader construction: (. <something>)\nShould be (nil . <something>)\n";
static char baddot2[]=
"Bad reader construction: (<something> .)\n\
Should be (<something> . <something>), assumed to be (<something>)";
static char baddot3[]=
"Bad reader construction: (<something> . <something> not followed by )";

/* readr ****************************************************************/
/* returns a s-expression read in from the port specified as the first	*/
/* argument.  Handles superbrackets, reader macros.			*/
lispval
readr(useport)
FILE *useport;
{
	register lispval handy = Vreadtable->a.clb;

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
	int inlbkt = FALSE;
	lispval errorh();
	Savestack(4); /* ???not necessary because np explicitly restored if
	  changed */

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
				goto out;
			default:
				atomval = readrx(code);
			case TSCA:
				np++->val=atomval;
				*current = work = newdot();
				work->d.car = atomval;
				np--;
				current = (lispval *) &(work->d.cdr);
				break;
			case TINF:
				imacrox(result->val,TRUE);
				work = atomval;
				result->val = work->d.car;
				current = (lispval *) & (result->val);
				goto mcom;
			case TSPL:
				macrox(); /* input and output in atomval */
				*current = atomval;
			mcom:
				while(*current!=nil) {
					if(TYPE(*current)!=DTPR)
						errorh1(Vermisc,"Non-list returned from splicing macro",nil,FALSE,7,*current);
					current=(lispval *)&((*current)->d.cdr);
				}
				break;
			case TPERD:
				if(result->val==nil) {
					work = result->val=newdot();
					current = (lispval *) &(work->d.cdr);
					fprintf(stderr,baddot1);
				}
				code = Iratom();
				if(code==TRPARA) {
					result->val = errorh1(Vermisc,baddot2,nil,TRUE,58,result->val);
					goto out;
				}
				*current = readrx(code);
				/* there is the possibility that the expression
				   following the dot is terminated with a "]"
				   and thus needs no closing lparens to follow
				*/
				if(!rbktf && ((code = Iratom()))!=TRPARA) {
					errorh2(Vermisc,baddot3,nil,TRUE,59,result->val,atomval);
				}
				if(rbktf && inlbkt)
					rbktf = FALSE;
				goto out;
			case TEOF:
				errorh1(Vermisc,"Premature end of file after ", 
							  nil,FALSE,0,result->val);
			}
			if(rbktf) {
				if(inlbkt)
					rbktf = FALSE;
				goto out;
			}
		}
	case TSCA:
		Restorestack();
		return(atomval);
	case TEOF:
		Restorestack();
		return(eofa);
	case TMAC:
		macrox();
		Restorestack();
		return(atomval);
	case TINF:
		imacrox(nil,FALSE);
		work = atomval;
		if(work==nil) { code = Iratom(); goto top;}
		work = work->d.car;
	        Restorestack();
		if(work->d.cdr==nil)
		    return(work->d.car);
		else
		    return(work);
	case TSPL:
		macrox();
		if((work = atomval)!=nil) {
			if(TYPE(work)==DTPR && work->d.cdr==nil) {
				Restorestack();
				return(work->d.car);
			} else {
				errorh1(Vermisc,
"Improper value returned from splicing macro at top-level",nil,FALSE,9,work);
			}
		}
		code = Iratom();
		goto top;
		/* return(readrx(Iratom())); */
	case TSQ:
		result = np;
		protect(newdot());
		(work = result->val)->d.car = quota;
		work = work->d.cdr = newdot();
		work->d.car = readrx(Iratom());
		goto out;

	case TRPARA:
		Restorestack();
		return(errorh(Vermisc,
		    "read: read a right paren when expecting an s-expression",
		    nil,FALSE,0));
	case TPERD:
		Restorestack();
		return(errorh(Vermisc,
		    "read: read a period when expecting an s-expression",
		    nil,FALSE,0));
		    
	/* should never get here, we should have covered all cases above */
	default:
		Restorestack();
		return(errorh1(Vermisc,"Readlist error,  code ",nil,FALSE,0,inewint((long)code)));
	}
out:
	work = result->val;
	np = result;
	Restorestack();
	return(work);
}
macrox()
{
    	FILE *svport;
	lispval handy, Lapply();

	Savestack(0);
	svport = rdrport;	/* save from possible changing */
	lbot = np;
	protect(handy=Iget(atomval,lastrtab));
	if (handy == nil)
	{
	    errorh1(Vermisc,"read: can't find the character macro for ",nil,
	    		FALSE,0,atomval);
	}
	protect(nil);
	atomval = Lapply();
	chkrtab(Vreadtable->a.clb);	/* the macro could have changed
					   the readtable
					 */
	rdrport = svport;	/* restore old value */
	Restorestack();
	return;
}
imacrox(current,inlist)
register lispval current;
{
    	FILE *svport;
	register lispval work;
	lispval Lapply(), handy;

	Savestack(2);
	svport = rdrport;	/* save from possible changing */
	if(inlist)
	{
	    protect(handy = newdot());
	    handy->d.car = current;
	    for(work = handy->d.car; (TYPE(work->d.cdr))==DTPR; )
	    	work = work->d.cdr;
            handy->d.cdr = work;
	}
	else handy = current;
	
	lbot = np;
	protect(Iget(atomval,lastrtab));
	protect(handy);
	atomval = Lfuncal();
	chkrtab(Vreadtable->a.clb);	/* the macro could have changed
					   the readtable
					 */
	rdrport = svport;	/* restore old value */
	Restorestack();
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
	int code, cc;
	int strflag = FALSE;

	name = strbuf;

again:	cc = getc(useport);
	if(cc==EOF)
	{
	    clearerr(useport);
	    return(TEOF);
	}
	c = cc & 0177;
	*name = c;

	switch(synclass(ctable[c])) {

	default:	goto again;

	case synclass(VNUM):

	case synclass(VSIGN):	*name++ = c;
			atomval = (getnum(name));
			return(TSCA);

	case synclass(VESC):
			dbqflag = TRUE;
			*name++ = getc(useport) & 0177;
			atomval = (finatom(name));
			return(TSCA);
			
	case synclass(VCHAR):
			*name++ = c;
			atomval = (finatom(name));
			return(TSCA);

	case synclass(VLPARA):	return(TLPARA);

	case synclass(VRPARA):	return(TRPARA);

	case synclass(VPERD):	marker = peekc(useport) & 0177;
			if(synclass(VNUM)!=synclass(ctable[marker]))
			{  if(SEPMASK & ctable[marker])
				return(TPERD);
			   else { *name++ = c;	/* this period begins an atm */
				  atomval = finatom(name);
				  return(TSCA);
			   }
			}
			*name++ = '.';
			mantisfl = 1;
			atomval = (getnum(name));
			return(TSCA);

	case synclass(VLBRCK):	return(TLBKT);

	case synclass(VRBRCK):	rbktf = TRUE;
			return(TRPARA);

	case synclass(VSQ):	return(TSQ);

	case synclass(VSD):	strflag = TRUE;
	case synclass(VDQ):	name = strbuf;
			marker = c;
			while ((c = getc(useport)) != marker) {

				if(synclass(VESC)==synclass(ctable[c]))
					c = getc(useport) & 0177;
				*name++ = c;
				if (name >= endstrb)
					atomtoolong();
				if (feof(useport)) {
					clearerr(useport);
					error("EOF encountered while reading atom", FALSE);
				}
			}
			*name = NULL_CHAR;
			if(strflag)
				atomval = (lispval) pinewstr(strbuf);
			else
				atomval = (getatom());
			return(TSCA);

	case synclass(VERR):	if (c == '\0') 
			{
			  fprintf(stderr,"[read: null read and ignored]\n");
			  goto again;	/* null pname */
			}
			fprintf(stderr,"%c (%o): ",c,(int) c);
			error("ILLEGAL CHARACTER IN ATOM",TRUE);

	case synclass(VSINF):
		code = TINF;
		goto same;
	case synclass(VSSPL):
		code = TSPL;
		goto same;
	case synclass(VSMAC):
		code = TMAC;
	same:
		marker = peekc(rdrport);
		if(! (SEPMASK & ctable[marker]) ) {
		    *name++ = c;  /* this is not a macro */
		    atomval = (finatom(name));
		    return(TSCA);
		}
		goto simple;
	case synclass(VINF):
		code = TINF;
		goto simple;
	case synclass(VSCA):
		code = TSCA;
		goto simple;
	case synclass(VSPL):
		code = TSPL;
		goto simple;
	case synclass(VMAC):
		code = TMAC;
	simple:
		strbuf[0] = c;
		strbuf[1] = 0;
		atomval = (getatom());
		return(code);
	}
}
static char toobig[]="Number exceeds parse buffer";

#define push();	if(name==endstrb) error(toobig,FALSE); else *name++=c;
#define next() (((cc=getc(useport))!=EOF)?(stats = ctable[c = cc &0177]):\
					((c=0),(saweof = 1),(stats = SEPMASK)))

lispval
getnum(name)
register char *name;
{
	unsigned char c;
	register lispval result;
	register FILE *useport=rdrport;
	unsigned char  stats;
	int sawdigit = 0, saweof = 0,cc;
	char *exploc = (char *) 0;
	double realno;
	extern lispval finatom(), calcnum(), newdoub(), dopow();

	if(mantisfl) {
		mantisfl = 0;
		next();
		goto mantissa;
	}
	if(VNUM==ctable[*(unsigned char*)(name-1)]) sawdigit = 1;
	while(VNUM==next()) {
		push();		/* recognize [0-9]*, in "ex" parlance */
		sawdigit = 1;
	}
	if(c=='.') {
		push();		/* continue */ 
	} else if(stats & SEPMASK) {
		if(!saweof)ungetc((int)c,useport);
		return(calcnum(strbuf,name,(int)ibase->a.clb->i));
	} else if(c=='^') {
		push();
		return(dopow(name,(int)ibase->a.clb->i));
	} else if(c=='_') {
		if(sawdigit)	/* _ must be preceeded by a digit */
		{
		    push();
		    return(dopow(name,2));
		}
		else goto backout;
	} else if(c=='e' || c=='E' || c=='d' ||c=='D') {
		if(sawdigit) goto expt;
		else goto backout;
	} else {
	backout:
		ungetc((int)c,useport);
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
			return(dopow(name,(int)ibase->a.clb->i));
		} else if(c=='_') {
			push();
			return(dopow(name,2));
		} else {
				/* Here we have 1.x where x not num, not sep */
				/* Here we have decimal int. NOT FORTRAN! */
			if(!saweof)ungetc((int)c,useport);
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
		if(!saweof)ungetc((int)c,useport);
		goto verylast;
	}
expt:
	exploc = name;  /* remember location of exponent character */
	push();
	next();
	if(c=='+' || c =='-') {
		push();
		next();
	}
	while (VNUM==stats) {
		push();
		next();
	}
last:	if(!saweof)ungetc((int)c,useport);
	if(! (stats & SEPMASK) )
		return(finatom(name));

verylast:
 	/* scanf requires that the exponent be 'e' */
	if(exploc != (char *) 0 ) *exploc = 'e';
	*name=0;
	sscanf(strbuf,"%F",&realno);
	(result = newdoub())->r = realno;
	return(result);
}

lispval
dopow(part2,base)
register char *part2;
{
	register char *name = part2;
	register FILE *useport = rdrport;
	register int power;
	lispval work;
	unsigned char stats,c;
	int cc, saweof = 0;
	char *end1 = part2 - 1; lispval Ltimes();
	Savestack(4);

	while(VNUM==next()) {
		push();
	}
	if(c!='.') {
		if(!saweof)ungetc((int)c,useport);
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
		np++->val = calcnum(strbuf,end1,(int)ibase->a.clb->i);

	/* calculate exponent */
	if(c=='.')
		power = calcnum(part2,name,10)->i;
	else
		power = calcnum(part2,name,(int)ibase->a.clb->i)->i;
	while(power-- > 0)
		lbot[1].val = Ltimes();
	work = lbot[1].val;
	Restorestack();
	return(work);
}
	

lispval
calcnum(strbuf,name,base)
register char *name;
char *strbuf;
{
	register char *p;
	register lispval result, temp;
	int negflag = 0;

	result = temp = newsdot();		/* initialize sdot cell */
	protect(temp);
	p = strbuf;
	if(*p=='+') p++;
	else if(*p=='-') {negflag = 1; p++;}
	*name = 0;
	if(p>=name) return(getatom());

	for(;p < name; p++)
		dmlad(temp,(long)base,(long)*p-'0');
	if(negflag)
		dmlad(temp,-1L,0L);

	if(temp->s.CDR==0) {
		result = inewint(temp->i);
		pruneb(np[-1].val);
	}
	np--;
	return(result);
}
lispval
finatom(name)
register char *name;
{
	extern int uctolc;
	register FILE *useport = rdrport;
	unsigned char c, stats;
	int cc, saweof = 0;
	register char *savenm;
	savenm = name - 1;	/* remember start of name */
	while(!(next()&SEPMASK)) {

		if(synclass(stats) == synclass(VESC)) c = getc(useport) & 0177;
		*name++=c;
		if (name >= endstrb)
			atomtoolong();	/* error message */
	}
	*name = NULL_CHAR;
	if(!saweof)ungetc((int)c,useport);
	if (uctolc) for(; *savenm ; savenm++) 
			if( isupper(*savenm) ) *savenm = tolower(*savenm);
	return(getatom());
}

atomtoolong()
{
    /*
     * the string buffer contains an string which is too long 
     * we print an error message and include part of the atom
     */

    strbuf[50] = '\0';		/* truncate string */
    errorh1(Vermisc,"String or symbol print name too long, it begins with ",
    		nil,0,FALSE,getatom());
}
    
/* printr ***************************************************************/
/* prints the first argument onto the port specified by the second 	*/

/*
 * Last modified Mar 21, 1980 for hunks
 */

printr(a,useport)
register lispval a;
register FILE *useport;
{
	register hsize, i;
	char strflag = 0;
	char Idqc = 0;
	int curplength = plength;
	lispval Istsrch();
	lispval debugmode;

val_loop:
	if(! VALID(a)) {
	    debugmode = Istsrch(matom("debugging"))->d.cdr->d.cdr->d.cdr;
	    if(debugmode != nil) {
		printf("<printr:bad lisp data: 0x%x>\n",a);
 		error("Bad lisp data encountered by printr", FALSE); 
	    } else {
		a = badst;
		printf("<printr:bad lisp data: 0x%x>",a);
		return;
	    }
	}

	switch (TYPE(a))
	{


	case UNBO:	fputs("<UNBOUND>",useport);
			break;

	case VALUE:	fputs("(ptr to)",useport);
			a = a->l;
			goto val_loop;

	case INT:	fprintf(useport,"%d",a->i);
			break;

	case DOUB:	{  char buf[64];
			   lfltpr(buf,a->r);
			   fputs(buf,useport);
			}
			break;

	case PORT:	{ lispval  cp;
			  if((cp = ioname[PN(a->p)]) == nil)
			     fputs("%$unopenedport",useport);
			  else fprintf(useport,"%%%s",cp);
			}
			break;

	case HUNK2:
	case HUNK4:
	case HUNK8:
	case HUNK16:
	case HUNK32:
	case HUNK64:
	case HUNK128:
			if(plevel == 0) 
			{   
			     fputs("%",useport);
			     break;
			}
			hsize = 2 << HUNKSIZE(a);
			fputs("{", useport);
			plevel--;
			printr(a->h.hunk[0], useport);
			curplength--;
			for (i=1; i < hsize; i++)
			{
			    if (a->h.hunk[i] == hunkfree)
				break;
			    if (curplength-- == 0)
			    {
			    	fputs(" ...",useport); 
				break;
			    }
			    else
			    {
			        fputs(" ", useport);
				printr(a->h.hunk[i], useport);
			    }
			}
			fputs("}", useport);
			plevel++;
			break;
			
	case VECTOR:
		 	if((a->v.vector[VPropOff] != nil)
			  && (TYPE(a->v.vector[VPropOff]) == ATOM) )
			  fprintf(useport,"%s[%d]",
			  	    a->v.vector[VPropOff]->a.pname,
				    a->vl.vectorl[VSizeOff]);
		        else
			  fprintf(useport,"vector[%d]",
						 a->vl.vectorl[VSizeOff]);
			break;
	case VECTORI:
			fprintf(useport,"vectori[%d]",
						a->vl.vectorl[VSizeOff]);
			break;

	case ARRAY:	fputs("array[",useport);
			printr(a->ar.length,useport);
			fputs("]",useport);
			break;

	case BCD:	fprintf(useport,"#%X-",a->bcd.start);
			printr(a->bcd.discipline,useport);
			break;

	case OTHER:	fprintf(useport,"#Other-%X",a);
			break;

	case SDOT:	pbignum(a,useport);
			break;

	case DTPR:	if(plevel==0)
			{
			     fputs("&",useport);
			     break;
			}
			plevel--;
			if(a->d.car==quota && a->d.cdr!=nil 
			    && a->d.cdr->d.cdr==nil) {
				putc('\'',useport);
				printr(a->d.cdr->d.car,useport);
				plevel++;
				break;
			}
			putc('(',useport);
			curplength--;
	morelist:	printr(a->d.car,useport);
			if ((a = a->d.cdr) != nil)
				{
				if(curplength-- == 0)
				{
				    fputs(" ...",useport);
				    goto out;
				}
				putc(' ',useport);
				if (TYPE(a) == DTPR) goto morelist;
				fputs(". ",useport);
				printr(a,useport);
				}
		out:
			fputc(')',useport);
			plevel++;
			break;

	case STRNG:	strflag = TRUE;
			Idqc = Xsdc;

	case ATOM:	{
			char	*front, *temp, first; int clean;
			temp = front = (strflag ? ((char *) a) : a->a.pname);
			if(Idqc==0) Idqc = Xdqc;

			if(Idqc) {
				clean = first = *temp;
				first &= 0177;
				switch(QUTMASK & ctable[first]) {
				case QWNFRST:
				case QALWAYS:
					clean = 0; break;
				case QWNUNIQ:
					if(temp[1]==0) clean = 0;
				}
				if (first=='-'||first=='+') temp++;
				if(synclass(ctable[*temp])==VNUM) clean = 0;
				while (clean && *temp) {
					if((ctable[*temp++]&QUTMASK)==QALWAYS)
						clean = 0;
				}
				if (clean && !strflag)
					fputs(front,useport);
				else	 {
					putc(Idqc,useport);
					for(temp=front;*temp;temp++) {
						if(  *temp==Idqc
						  || (synclass(ctable[*temp])) == CESC)
							putc(Xesc,useport);
						putc(*temp,useport);
					}
					putc(Idqc,useport);
				}

			}  else {
				register char *cp = front;
				int handy = ctable[*cp & 0177];

				if(synclass(handy)==CNUM)
					putc(Xesc,useport);
				else switch(handy & QUTMASK) {
				case QWNUNIQ:
					if(cp[1]==0) putc(Xesc,useport);
					break;
				case QWNFRST:
				case QALWAYS:
					putc(Xesc,useport);
				}
				for(; *cp; cp++) {
					if((ctable[*cp]& QUTMASK)==QALWAYS)
						putc(Xesc,useport);
					putc(*cp,useport);
				}
			}
		}
	}
}

lfltpr(buf,val)		/* lisp floating point printer */
char *buf;
double val;
{
	register char *cp1; char *sprintf();

	sprintf(buf,(char *)Vfloatformat->a.clb,val);
	for(cp1 = buf; *cp1; cp1++)
		if(*cp1=='.'|| *cp1=='E' || *cp1 == 'e') return;

	/* if we are here, there was no dot, so the number was
	   an integer.  Furthermore, cp1 already points to the 
	   end of the string. */

	*cp1++ = '.';
	*cp1++ = '0';
	*cp1++ = 0;
}
	

/* dmpport ****************************************************************/
/* outputs buffer indicated by first argument whether full or not	*/

dmpport(useport)
FILE *useport;
{
	fflush(useport);
}

/*  protect and unprot moved to eval.c  (whr)  */
