#include "defs"

exlab(n)
register int n;
{
if(n==0 && thisexec->labelno && !(thisexec->labused))
	{
	thisexec->labused = 1;
	n = thisexec->labelno;
	}

if(!prevbg || n!=0)  /* avoid empty statement */
	{
	if(comments && !afterif) putcomment();
	putic(ICBEGIN, n);
	putic(ICINDENT, ctllevel);
	if(n != 0)
		if(stnos[n] != 0)
			fatal("statement number changed");
		else	stnos[n] = ( nxtstno += tailor.deltastno) ;
	TEST fprintf(diagfile, "LABEL %d\n", n);
	thisexec->nftnst++;
	afterif = 0;
	}
}


exgoto(n)
int n;
{
exlab(0);
exgo1(n);
}

exgoind(n)
int n;
{
exlab(0);
putic(ICKEYWORD,FGOTO);
putic(ICINDPTR,n);
TEST fprintf(diagfile, "goto indirect %o\n", n);
}



exgo1(n)
int n;
{
putic(ICKEYWORD,FGOTO);
putic(ICLABEL,n);
TEST fprintf(diagfile, "goto %d\n", n);
}


excompgoto(labs,index)
ptr labs;
register ptr index;
{
register int first;
register ptr p;

index = simple(LVAL,index);
if(tailor.ftn77)
	exlab(0);
else
	{
	int ncases = 0;
	for(p = labs ; p ; p = p->nextp)
		++ncases;
	exif1( mknode(TLOGOP, OPAND,
		mknode(TRELOP,OPGT, cpexpr(index), mkint(0)),
		mknode(TRELOP,OPLE, cpexpr(index), mkint(ncases)) ));
	}

putic(ICKEYWORD, FGOTO);
putic(ICOP,OPLPAR);

first = 1;
for(p = labs ; p ; p = p->nextp)
	{
	if(first)   first = 0;
	else   putic(ICOP,OPCOMMA);
	putic(ICLABEL,p->datap);
	}
putic(ICOP,OPRPAR);
frchain(&labs);

putic(ICOP,OPCOMMA);
prexpr(index);
frexpr(index);
TEST fprintf(diagfile, "computed goto\n");
}




excall(p)
register ptr p;
{
register ptr q1, q2, q3;
ptr mkholl(), exioop();

if(p->tag==TNAME || p->tag==TFTNBLOCK)
	p = mkcall(p, PNULL);

if(p->tag == TERROR)
	{
	frexpr(p);
	return;
	}
if(p->tag != TCALL)
	badtag("excall", p->tag);

q1 = p->leftp;
q2 = (q1->tag==TFTNBLOCK ? q1 : q1->sthead->varp);
if(q2->vtype!=TYUNDEFINED && q2->vtype!=TYSUBR)
	{
	dclerr("attempt to use a variable as a subroutine", p->sthead->namep);
	frexpr(p);
	return;
	}
q1->vtype = q2->vtype = TYSUBR;
if(q1->vdcldone==0)
	dclit(q1);

if(q1->tag == TNAME)
	{
	if( equals(q2->sthead->namep, "stop") )
		{
		exlab(0);
		putic(ICKEYWORD, FSTOP);
		TEST fprintf(diagfile,"stop ");
		if( (q1 = p->rightp) && (q1 = q1->leftp) )
			prexpr( simple(RVAL, q1->datap) );
		goto done;
		}
	if( ioop(q2->sthead->namep) )
		{
		exioop(p,NO);
		goto done;
		}
	}

p = simple(RVAL,p);
exlab(0);
putic(ICKEYWORD,FCALL);
TEST fprintf(diagfile, "call ");
/* replace character constant arguments with holleriths */
if( (q1=p->rightp) && tailor.hollincall)
	for(q1 = q1->leftp ; q1 ; q1 = q1->nextp)
		if( (q2 = q1->datap)->tag==TCONST
		    && q2->vtype==TYCHAR)
			{
			q2->vtype = TYHOLLERITH;
			frexpr(q2->vtypep);
			q2->vtypep = 0;
			q2->leftp = mkholl(q3 = q2->leftp);
			cfree(q3);
			}
prexpr( p );

done:	frexpr(p);
}




ptr mkholl(p)
register char *p;
{
register char *q, *t, *s;
int n;

n = strlen(p);
q = convic(n);
s = t = calloc(n + 2 + strlen(q) , 1);
while(*q)
	*t++ = *q++;
*t++ = 'h';
while(*t++ = *p++ )
	;
return(s);
}


ptr ifthen()
{
ptr p;
ptr addexec();

p = addexec();
thisexec->brnchend = 0;
if(thisexec->nftnst == 0)
	{
	exlab(0);
	putic(ICKEYWORD,FCONTINUE);
	thisexec->nftnst = 1;
	}
if(thisexec->nftnst>1 || thisexec->labeled || thisexec->uniffable )
	{
	if(thisctl->breaklab == 0)
		thisctl->breaklab = nextlab();
	indifs[thisctl->indifn] = thisctl->breaklab;
	}
else	thisctl->breaklab = 0;
return(p);
}



exasgn(l,o,r)
ptr l;
int o;
ptr r;
{
exlab(0);
if(l->vdcldone == 0)
	dclit(l);
frexpr( simple(LVAL , mknode(TASGNOP,o,l,r)) );
}

exretn(p)
ptr p;
{
if(p)
	{
	if(procname && procname->vtype && procname->vtype!=TYCHAR &&
	  (procname->vtype!=TYLCOMPLEX || tailor.lngcxtype!=NULL) )
		{
		if(p->tag!=TNAME || p->sthead!=procname->sthead)
			exasgn( cpexpr(procname) , OPASGN, p);
		}
	else execerr("can only return values in a function", PNULL);
	}
else if(procname && procname->vtype)
	 warn("function return without data value");
exlab(0);
putic(ICKEYWORD, FRETURN);

TEST {fprintf(diagfile, "exec: return( " );  prexpr(p);  fprintf(diagfile, ")\n" );  }
}


exnull()
{
if(thisexec->labelno && !(thisexec->labused) )
	{
	exlab(0);
	putic(ICKEYWORD,FCONTINUE);
	}
}




exbrk(opnext,levskip,btype)
int opnext;
ptr levskip;
int btype;
{

if(opnext && (btype==STSWITCH || btype==STPROC))
	execerr("illegal next", PNULL);
else if(!opnext && btype==STPROC)
	exretn(PNULL);
else  brknxtlab(opnext,levskip,btype);
TEST fprintf(diagfile, "exec: %s\n", (opnext ? "next" : "exit"));

}



exif(e)
register ptr e;
{
int tag;

if( (tag = e->tag)==TERROR || e->vtype!=TYLOG)
	{
	frexpr(e);
	e = mkconst(TYLOG, ".true.");
	if(tag != TERROR)
		execerr("non-logical conditional expression in if", PNULL);
	}
TEST fprintf(diagfile, "exif called\n");
e = simple(RVAL,e);
exlab(0);
putic(ICKEYWORD,FIF2);
indifs[thisctl->indifn = nextindif()] = 0;
putic(ICINDPTR, thisctl->indifn);
putic(ICOP,OPLPAR);
prexpr(e);
putic(ICOP,OPRPAR);
putic(ICMARK,0);
putic(ICOP,OPLPAR);
prexpr(e = simple(RVAL, mknode(TNOTOP,OPNOT,e,PNULL)));
putic(ICOP,OPRPAR);
putic(ICMARK,0);
afterif = 1;
frexpr(e);
}


exifgo(e,l)
ptr e;
int l;
{
exlab(0);
exif1(e);
exgo1(l);
}


exif1(e)
register ptr e;
{
e = simple(RVAL,e);
exlab(0);
putic(ICKEYWORD,FIF1);
putic(ICOP,OPLPAR);
TEST fprintf(diagfile, "if1 ");
prexpr( e );
frexpr(e);
putic(ICOP,OPRPAR);
putic(ICBLANK, 1);
}







brkcase()
{
ptr bgnexec();

if(ncases==0 /* && thisexec->prevexec->brnchend==0 */ )
	{
	exbrk(0, PNULL, 0);
	addexec();
	bgnexec();
	}
ncases = 1;
}


brknxtlab(opnext, levp, btype)
int opnext;
ptr levp;
int btype;
{
register ptr p;
int levskip;

levskip = ( levp ? convci(levp->leftp) : 1);
if(levskip <= 0)
	{
	execerr("illegal break count %d", levskip);
	return;
	}

for(p = thisctl ; p!=0 ; p = p->prevctl)
	if( (btype==0 || p->subtype==btype) &&
	    p->subtype!=STIF && p->subtype!=STPROC &&
	    (!opnext || p->subtype!=STSWITCH) )
		if(--levskip == 0) break;

if(p == 0)
	{
	execerr("invalid break/next", PNULL);
	return;
	}

if(p->subtype==STREPEAT && opnext)
	exgoind(p->indifn);
else if(opnext)
	exgoto(p->nextlab);
else	{
	if(p->breaklab == 0)
		p->breaklab = nextlab();
	exgoto(p->breaklab);
	}
}



ptr doloop(p1,p2,p3)
ptr p1;
ptr p2;
ptr p3;
{
register ptr p, q;
register int i;
int val[3];

p = ALLOC(doblock);
p->tag = TDOBLOCK;

if(p1->tag!=TASGNOP || p1->subtype!=OPASGN || p1->leftp->tag!=TNAME)
	{
	p->dovar = gent(TYINT, PNULL);
	p->dopar[0] = p1;
	}
else	{
	p->dovar = p1->leftp;
	p->dopar[0] = p1->rightp;
	frexpblock(p1);
	}
if(p2 == 0)
	{
	p->dopar[1] = p->dopar[0];
	p->dopar[0] = mkint(1);
	}
else	p->dopar[1] = p2;
p->dopar[2] = p3;

for(i = 0; i<3 ; ++i)
	{
	if(q = p->dopar[i])
		{
		if( (q->tag==TNAME || q->tag==TTEMP) &&
		   (q->vsubs || q->voffset) )
			p->dopar[i] = simple(RVAL,mknode(TASGNOP,0,
				gent(TYINT,PNULL), q));
		else
			p->dopar[i] = simple(LVAL, coerce(TYINT, q) );

		if(isicon(p->dopar[i], &val[i]))
			{
			if(val[i] <= 0)
				execerr("do parameter out of range", PNULL);
			}
		else	val[i] = -1;
		}
	}

if(val[0]>0 && val[1]>0 && val[0]>val[1])
	execerr("do parameters out of order", PNULL);
return(p);
}
