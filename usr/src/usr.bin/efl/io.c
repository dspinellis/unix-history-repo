#include <ctype.h>

#include "defs"

static int lastfmtchar;
static int writeop;
static int needcomma;


ptr mkiost(kwd,unit,list)
int kwd;
ptr unit;
ptr list;
{
register ptr p;

if(unit!=NULL && unit->vtype!=TYINT)
	{
	execerr("I/O unit must be an integer", "");
	return(NULL);
	}
p = allexpblock();
p->tag = TIOSTAT;
p->vtype = TYINT;
p->iokwd = kwd;
p->iounit = unit;
p->iolist = list;

return(p);
}




struct iogroup *mkiogroup(list, format, dop)
ptr list;
char *format;
ptr dop;
{
register struct iogroup *p;

p = ALLOC(iogroup);
p->tag = TIOGROUP;
p->doptr = dop;
p->iofmt = format;
p->ioitems = list;
return(p);
}

ptr exio(iostp, errhandle)
struct iostblock *iostp;
int errhandle;
{
ptr unit, list;
int fmtlabel, errlabel, endlabel, jumplabel;
ptr errval;
int fmtio;

if(iostp == NULL)
	return( errnode() );
unit = iostp->iounit;
list = iostp->iolist;

/* kwd=	0  binary input 	2  formatted input
	1  binary output	3  formatted output
*/

writeop = iostp->iokwd & 01;
if( fmtio = (iostp->iokwd & 02) )
	fmtlabel = nextlab() ;
frexpblock(iostp);

errval = 0;
endlabel = 0;
if(errhandle)
	{
	switch(tailor.errmode)
		{
		default:
			execerr("no error handling ", "");
			return( errnode() );

		case IOERRIBM:	/* ibm: err=, end= */
			jumplabel = nextlab();
			break;

		case IOERRFORT77:	/* New Fortran Standard: iostat= */
			break;

		}
	errval = gent(TYINT, PNULL);
	}
if(unit)
	unit = simple(RVAL, unit);
else	unit = mkint(writeop ? tailor.ftnout : tailor.ftnin);

if(unit->tag!=TCONST && (unit->tag!=TNAME || unit->vsubs!=0))
	unit = simple(LVAL, mknode(TASGNOP,OPASGN,gent(TYINT,PNULL),unit));

simlist(list);

exlab(0);
putic(ICKEYWORD, (writeop ? FWRITE : FREAD) );
putic(ICOP, OPLPAR);
prexpr(unit);
frexpr(unit);

if( fmtio )
	{
	putic(ICOP, OPCOMMA);
	putic(ICLABEL, fmtlabel);
	}

if(errhandle) switch(tailor.errmode)
	{
	case IOERRIBM:
		putic(ICOP,OPCOMMA);
		putsii(ICCONST, "err =");
		putic(ICLABEL, errlabel = nextlab() );
		if(!writeop)
			{
			putic(ICOP,OPCOMMA);
			putsii(ICCONST, "end =");
			putic(ICLABEL, endlabel = nextlab() );
			}
		break;

	case IOERRFORT77:
		putic(ICOP,OPCOMMA);
		putsii(ICCONST, "iostat =");
		putname(errval);
		break;
	}

putic(ICOP,OPRPAR);
putic(ICBLANK, 1);

needcomma = NO;
doiolist(list);
if(fmtio)
	{
	exlab(fmtlabel);
	putic(ICKEYWORD, FFORMAT);
	putic(ICOP, OPLPAR);
	lastfmtchar = '(';
	doformat(1, list);
	putic(ICOP, OPRPAR);
	}
friolist(list);

if(errhandle && tailor.errmode==IOERRIBM)
	{
	exasgn(cpexpr(errval), OPASGN, mkint(0) );
	exgoto(jumplabel);
	exlab(errlabel);
	exasgn(cpexpr(errval), OPASGN, mkint(1) );
	if(endlabel)
		{
		exgoto(jumplabel);
		exlab(endlabel);
		exasgn(cpexpr(errval), OPASGN,
			mknode(TNEGOP,OPMINUS,mkint(1),PNULL) );
		}
	exlab(jumplabel);
	}

return( errval );
}

doiolist(list)
ptr list;
{
register ptr p, q;
register struct doblock *dop;
for(p = list ; p ; p = p->nextp)
	{
	switch( (q = p->datap) ->tag)
		{
		case TIOGROUP:
			if(dop = q->doptr)
				{
				if(needcomma)
					putic(ICOP, OPCOMMA);
				putic(ICOP, OPLPAR);
				needcomma = NO;
				}
			doiolist(q->ioitems);
			if(dop)
				{
				putic(ICOP,OPCOMMA);
				prexpr(dop->dovar);
				putic(ICOP, OPEQUALS);
				prexpr(dop->dopar[0]);
				putic(ICOP, OPCOMMA);
				prexpr(dop->dopar[1]);
				if(dop->dopar[2])
					{
					putic(ICOP, OPCOMMA);
					prexpr(dop->dopar[2]);
					}
				putic(ICOP, OPRPAR);
				needcomma = YES;
				}
			break;

		case TIOITEM:
			if(q->ioexpr)
				{
				if(needcomma)
					putic(ICOP, OPCOMMA);
				prexpr(q->ioexpr);
				needcomma = YES;
				}
			break;

		default:
			badtag("doiolist", q->tag);
		}
	}
}

doformat(nrep, list)
int nrep;
ptr list;
{
register ptr p, q;
int k;
ptr arrsize();

if(nrep > 1)
	{
	fmtnum(nrep);
	fmtop(OPLPAR);
	}

for(p = list ; p ; p = p->nextp)
	switch( (q = p->datap) ->tag)
		{
		case TIOGROUP:
			if(q->iofmt)
				prfmt(q->nrep, q->iofmt);
			else	{
				doformat(q->nrep>0 ? q->nrep :
					(q->doptr ? repfac(q->doptr) : 1),
					q->ioitems);
				}
			break;

		case TIOITEM:
			if(q->iofmt == NULL)
				break;

			if(q->nrep==0 && q->ioexpr && q->ioexpr->vdim)
				{
				if( ! isicon(arrsize(q->ioexpr), &k) )
					execerr("io of adjustable array", "");
				else
					prfmt(k, q->iofmt);
				}
			else
				prfmt(q->nrep, q->iofmt);
		}
if(nrep > 1)
	fmtop(OPRPAR);
}

fmtop(op)
register int op;
{
register c;

c = (op==OPLPAR ? '(' : (op==OPRPAR ? ')' : 'x') );
fmtcom(c);
putic(ICOP, op);
lastfmtchar = c;
}




fmtnum(k)
int k;
{
fmtcom('1');
prexpr( mkint(k) );
lastfmtchar = ',';	/* prevent further comma after factor*/
}








/* separate formats with comma unless already a slash*/
fmtcom(c)
int c;
{
if( c!='/' && c!=')' && lastfmtchar!='/' && lastfmtchar!='(' && lastfmtchar!=',' )
	{
	putic(ICOP, OPCOMMA);
	lastfmtchar = ',';
	}
}

prfmt(nrep, str)
int nrep;
char *str;
{
char fmt[20];
register int k, k0, k1, k2;
register char *t;

fmtcom(nrep>1 ? '1' : str[0]);

if(nrep > 1)
	{
	fmtnum(nrep);
	fmtop(OPLPAR);
	}

switch(str[0])
	{
	case 'd':
	case 'e':
	case 'g':
		if(writeop)
			{
			putsii(ICCONST, "1p");
			break;
			}
	
	case 'f':
		putsii(ICCONST, "0p");
		break;
		
	case 'c':
		k = convci(str+1);
		k0 = tailor.ftnchwd;
		k1 = k / k0;
		k2 = k % k0;
		if(k1>0 && k2>0)
			sprintf(fmt, "(%da%d,a%d)",k1,k0,k2);
		else if(k1>1)
			sprintf(fmt, "(%da%d)", k1, k0);
		else	sprintf(fmt, "a%d", k);
		putsii(ICCONST, fmt);
		lastfmtchar = 'f';	/* last char isnt operator */
		goto close;

	default:
		break;
	}
putsii(ICCONST,str);
/* if the format is an nH, act as if it ended with a non-operator character */
if( isdigit(str[0]) )
	{
	for(t = str+1 ; isdigit(*t) ; ++t);
		;
	if(*t=='h' || *t=='H')
		{
		lastfmtchar = 'f';
		goto close;
		}
	}
lastfmtchar = str[ strlen(str)-1 ];

close:
	if(nrep > 1)
		fmtop(OPRPAR);
}

friolist(list)
ptr list;
{
register ptr p, q;
register struct doblock *dop;

for(p = list; p; p = p->nextp)
	{
	switch ( (q = p->datap) ->tag)
		{
		case TIOGROUP:
			if(dop = q->doptr)
				{
				frexpr(dop->dovar);
				frexpr(dop->dopar[0]);
				frexpr(dop->dopar[1]);
				if(dop->dopar[2])
					frexpr(dop->dopar[2]);
				cfree(dop);
				}
			friolist(q->ioitems);
			break;

		case TIOITEM:
			if(q->ioexpr)
				frexpr(q->ioexpr);
			break;

		default:
			badtag("friolist", q->tag);
		}
	if(q->iofmt)
		cfree(q->iofmt);
	cfree(q);
	}
frchain( &list );
}

simlist(p)
register ptr p;
{
register ptr q, ep;
struct iogroup *enloop();

for( ; p ; p = p->nextp)
	switch( (q = p->datap) ->tag )
		{
		case TIOGROUP:
			simlist(q->ioitems);
			break;

		case TIOITEM:
			if(ep = q->ioexpr)
				{
				/* if element is a subaggregate, need
				   an implied do loop */
				if( (ep->voffset || ep->vsubs) &&
				    (ep->vdim || ep->vtypep) )
					p->datap = enloop(q);
				else
					q->ioexpr = simple(LVAL,ep);
				}
			break;

		default:
			badtag("ioblock", q->tag);
		}
}




/* replace an aggregate by an implied do loop of elements */

struct iogroup *enloop(p)
struct ioitem *p;
{
register struct doblock *dop;
struct iogroup *gp;
ptr np, q, v, arrsize(), mkioitem();
int nrep, k, nwd;

q = p->ioexpr;
np = arrsize(q);
if( ! isicon(np, &nrep) )
	nrep = 0;

if(q->vtype == TYCHAR)
	{
	nwd = ceil(conval(q->vtypep), tailor.ftnchwd);
	if(nwd != 1)
		np = simple(LVAL, mknode(TAROP,OPSTAR,np,mkint(nwd)));
	}
else
	nwd = 0;

if( isicon(np, &k) && k==1)
	return(p);

dop = ALLOC(doblock);
dop->tag = TDOBLOCK;

dop->dovar = v = gent(TYINT, PNULL);
dop->dopar[0] = mkint(1);
dop->dopar[1] = simple(SUBVAL, np);
dop->dopar[2] = NULL;

q = simple(LVAL, q);
if(q->vsubs == NULL)
	q->vsubs = mknode(TLIST,0, mkchain(cpexpr(v),CHNULL), PNULL);
else
	q->vsubs->leftp->datap = simple(SUBVAL, mknode(TAROP,OPPLUS, cpexpr(v),
		     mknode(TAROP,OPMINUS,q->vsubs->leftp->datap,mkint(1))));
q->vdim = NULL;
gp = mkiogroup( mkchain(mkioitem(q,CNULL), CHNULL), p->iofmt, dop);
gp->nrep = nrep;
cfree(p);
return(gp);
}

ptr mkformat(letter, n1, n2)
char letter;
register ptr n1, n2;
{
char f[20], *fp, *s;
int k;

if(letter == 's')
	{
	if(n1)
		{
		k = conval(n1);
		frexpr(n1);
		}
	else	k = 1;

	for(fp = f; k-->0 ; )
		*fp++ = '/';
	*fp = '\0';
	return( copys(f) );
	}

f[0] = letter;
fp = f+1;

if(n1)	{
	n1 = simple(RVAL,n1);
	if(n1->tag==TCONST && n1->vtype==TYINT)
		{
		for(s = n1->leftp ; *s; )
			*fp++ = *s++;
		}
	else	execerr("bad format component %s", n1->leftp);
	frexpr(n1);
	}

if(n2)	{
	if(n2->tag==TCONST && n2->vtype==TYINT)
		{
		*fp++ = '.';
		for(s = n2->leftp ; *s; )
			*fp++ = *s++;
		}
	else	execerr("bad format component %s", n2->leftp);
	frexpr(n2);
	}

if( letter == 'x' )
	{
	if(n1 == 0)
		*fp++ = '1';
	fp[0] = 'x';
	fp[1] = '\0';
	return( copys(f+1) );
	}
else	{
	*fp = '\0';
	return( copys(f) );
	}
}

ptr mkioitem(e,f)
register ptr e;
char *f;
{
register ptr p;
char fmt[10];
ptr gentemp();

p = ALLOC(ioitem);
p->tag = TIOITEM;
if(e!=NULL && e->tag==TCONST)
	if(e->vtype==TYCHAR && (f==0 || (f[0]=='c' && f[1]=='\0') ))
		{
		p->ioexpr = 0;
		sprintf(msg, "%dh%s", strlen(e->leftp), e->leftp);
		p->iofmt = copys(msg);
		frexpr(e);
		return(p);
		}
	else	e = mknode(TASGNOP,OPASGN,gentemp(e),e);

if(e && e->vtype==TYCHAR && f && f[0]=='c' && f[1]=='\0')
	f = NULL;
if(f == NULL)
	{
	switch(e->vtype)
		{
		case TYINT:
		case TYREAL:
		case TYLREAL:
		case TYCOMPLEX:
		case TYLOG:
			f = copys( tailor.dfltfmt[e->vtype] );
			break;

		case TYCHAR:
			if(e->vtypep->tag != TCONST)
				{
				execerr("no adjustable character formats", "");
				f = 0;
				}
			else	{
				sprintf(fmt, "c%s", e->vtypep->leftp);
				f = copys(fmt);
				}
			break;

		default:
			execerr("cannot do I/O on structures", "");
			f = 0;
			break;
		}
	}

p->ioexpr = e;
p->iofmt = f;
return(p);
}



ptr arrsize(p)
ptr p;
{
register ptr b;
ptr f, q;

q = mkint(1);

if(b = p->vdim)
    for(b = b->datap ; b ; b = b->nextp)
	{
	if(b->upperb == 0) continue;
	f = cpexpr(b->upperb);
	if(b->lowerb)
		f = mknode(TAROP,OPPLUS,f,
			mknode(TAROP,OPMINUS,mkint(1),cpexpr(b->lowerb)));
	q = simple(RVAL, mknode(TAROP,OPSTAR,q,f));
	}
return(q);
}




repfac(dop)
register struct doblock *dop;
{
int m1, m2, m3;

m3 = 1;
if( isicon(dop->dopar[0],&m1) &&  isicon(dop->dopar[1],&m2) &&
  (dop->dopar[2]==NULL || isicon(dop->dopar[2],&m3)) )
	{
	if(m3 > 0)
		return(1 + (m2-m1)/m3);
	}
else	execerr("nonconstant implied do", "");
return(1);
}



ioop(s)
char *s;
{
if( equals(s, "backspace") )
	return(FBACKSPACE);
if( equals(s, "rewind") )
	return(FREWIND);
if( equals(s, "endfile") )
	return(FENDFILE);
return(0);
}




ptr exioop(p, errcheck)
register struct exprblock *p;
int errcheck;
{
register ptr q, t;

if( (q = p->rightp)==NULL || (q = q->leftp)==NULL  )
	{
	execerr("bad I/O operation", "");
	return(NULL);
	}
q = simple(LVAL, cpexpr(q->datap) );

exlab(0);
putic(ICKEYWORD, ioop(p->leftp->sthead->namep));

if(errcheck)
	{
	if(tailor.errmode != IOERRFORT77)
		{
		execerr("cannot test value of IOOP without ftn77", "");
		return( errnode() );
		}
	putic(ICOP, OPLPAR);
	prexpr(q);
	putic(ICOP, OPCOMMA);
	putsii(ICCONST, "iostat =");
	prexpr(cpexpr( t = gent(TYINT,PNULL)));
	putic(ICOP, OPRPAR);
	return( t );
	}
else	{
	putic(ICBLANK, 1);
	prexpr(q);
	}
}
