#include "defs"


static char mess[ ] = "inconsistent attributes";

attatt(a1 , a2)
register struct atblock *a1, *a2;
{
#define MERGE1(x) {if(a1->x==0) a1->x = a2->x; else if(a2->x!=0 && a1->x!=a2->x) dclerr(mess,"x"+2); }

MERGE1(attype);
MERGE1(attypep);
MERGE1(atprec);
MERGE1(atclass);
MERGE1(atext);
MERGE1(atcommon);
MERGE1(atdim);

if(a1->atprec!=0 && (a1->attype==TYREAL || a1->attype==TYCOMPLEX) )
	a1->attype += (TYLREAL-TYREAL);

cfree(a2);
}



attvars(a , v)
register struct atblock * a;
register chainp v;
{
register chainp p;

for(p=v; p!=0 ; p = p->nextp)
	attvr1(a, p->datap);

if(a->attype == TYFIELD)
	cfree(a->attypep);
else if(a->attype == TYCHAR)
	frexpr(a->attypep);

cfree(a);
}

#define MERGE(x,y) {if(v->y==0) v->y = a->x; else if(a->x!=0 && a->x!=v->y) dclerr(mess,"x"+2); }





attvr1(a, v)
register struct atblock * a;
register struct varblock * v;
{
register chainp p;

if(v->vdcldone)
	{
	dclerr("attempt to declare variable after use", v->sthead->namep);
	return;
	}
v->vdclstart = 1;
if(v->vclass == CLMOS)
	dclerr("attempt to redefine structure member", v->sthead->namep);
if (v->vdim == 0)
	v->vdim = a->atdim;
else if(!eqdim(a->atdim, v->vdim))
	dclerr("inconsistent dimensions", v->sthead->namep);
if(v->vprec == 0)
	v->vprec = a->atprec;

MERGE(attype,vtype);

if(v->vtypep == 0)
	{
	if(a->attypep != 0)
		if(a->attype == TYFIELD)
			{
			v->vtypep = ALLOC(fieldspec);
			cpblock(a->attypep, v->vtypep, sizeof(struct fieldspec));
			}
		else if(a->attype == TYCHAR)
			v->vtypep = cpexpr(a->attypep);
		else	v->vtypep = a->attypep;
	else if(a->attypep!=0 && a->attypep!=v->vtypep)
		dclerr("inconsistent attributes", "typep");
	}

if(v->vprec!=0 && (v->vtype==TYREAL || v->vtype==TYCOMPLEX) )
	v->vtype += (TYLREAL-TYREAL);

if(a->atcommon)
	if(v->vclass !=  0)
		dclerr("common variable already in common, argument list, or external",
			v->sthead->namep);
	else	{
		if(blklevel != a->atcommon->blklevel)
			dclerr("inconsistent common block usage", "");
		for(p = &(a->atcommon->comchain) ; p->nextp!=0 ; p = p->nextp) ;
		p->nextp = mkchain(v, PNULL);
	}

if(a->atext!=0 && v->vext==0)
	{
	v->vext = 1;
	extname(v);
	}
else if(a->atclass == CLVALUE)
	if(v->vclass==CLARG || v->vclass==CLVALUE)
		v->vclass = CLVALUE;
	else dclerr("cannot value a non-argument variable",v->sthead->namep);
else  MERGE(atclass,vclass);
if(v->vclass==CLCOMMON || v->vclass==CLVALUE || v->vclass==CLAUTO)
	setvproc(v, PROCNO);
}





eqdim(a,b)
register ptr a, b;
{
if(a==0 || b==0 || a==b)  return(1);

a = a->datap;
b = b->datap;

while(a!=0 && b!=0)
	{
	if(!eqexpr(a->lowerb,b->lowerb) || !eqexpr(a->upperb,b->upperb))
		return(0);

	a = a->nextp;
	b = b->nextp;
	}

return( a == b );
}


eqexpr(a,b)
register ptr a, b;
{
if(a==b) return(1);
if(a==0 || b==0) return(0);
if(a->tag!=b->tag || a->subtype!=b->subtype)
	return(0);

switch(a->tag)
	{
case TCONST:
	return( equals(a->leftp, b->leftp) );

case TNAME:
	return( a->sthead ==  b->sthead );

case TLIST:
	a = a->leftp;
	b = b->leftp;

	while(a!=0 && b!=0)
		{
		if(!eqexpr(a->datap,b->datap))
			return(0);
		a = a->nextp;
		b = b->nextp;
		}
	return( a == b );

case TAROP:
case TASGNOP:
case TLOGOP:
case TRELOP:
case TCALL:
case TREPOP:
	return(eqexpr(a->leftp,b->leftp) && eqexpr(a->rightp,b->rightp));

case TNOTOP:
case TNEGOP:
	return(eqexpr(a->leftp,b->leftp));

default:
	badtag("eqexpr", a->tag);
	}
/* NOTREACHED */
}



setimpl(type, c1, c2)
int type;
register int c1, c2;
{
register int i;

if(c1<'a' || c2<c1 || c2>'z')
	dclerr("bad implicit range", CNULL);
else if(type==TYUNDEFINED || type>TYLCOMPLEX)
	dclerr("bad type in implicit statement", CNULL);
else
	for(i = c1 ; i<=c2 ; ++i)
		impltype[i-'a'] = type;
}

doinits(p)
register ptr p;
{
register ptr q;

for( ; p ; p = p->nextp)
	if( (q = p->datap)->vinit)
		{
		mkinit(q, q->vinit);
		q->vinit = 0;
		}
}




mkinit(v, e)
register ptr v;
register ptr e;
{
if(v->vdcldone == 0)
	dclit(v);

swii(idfile);

if(v->vtype!=TYCHAR && v->vtypep)
	dclerr("structure initialization", v->sthead->namep);
else if(v->vdim==NULL || v->vsubs!=NULL)
	{
	if(e->tag==TLIST && (v->vtype==TYCOMPLEX || v->vtype==TYLCOMPLEX) )
		e = compconst(e);
	valinit(v, e);
	}
else
	arrinit(v,e);

swii(icfile);

frexpr(e);
}





valinit(v, e)
register ptr v;
register ptr e;
{
static char buf[4] = "1hX";
int vt;

vt = v->vtype;
/*check for special case of one-character initialization of
  non-character datum
*/
if(vt==TYCHAR || e->vtype!=TYCHAR || !isconst(e) || strlen(e->leftp)!=1)
	{
	e = simple(RVAL, coerce(vt,e) );
	if(e->tag == TERROR)
		return;
	if( ! isconst(e) )
		{
		dclerr("nonconstant initializer", v->sthead->namep);
		return;
		}
	}
if(vt == TYCHAR)
	{
	charinit(v, e->leftp);
	return;
	}
prexpr( simple(LVAL,v) );
putic(ICOP,OPSLASH);
if(e->vtype != TYCHAR)
	prexpr(e);
else if(strlen(e->leftp) == 1)
	{
	buf[2] = e->leftp[0];
	putsii(ICCONST, buf);
	}
else	dclerr("character initialization of nonchar", v->sthead->namep);
putic(ICOP,OPSLASH);
putic(ICMARK,0);
}



arrinit(v, e)
register ptr v;
register ptr e;
{
struct exprblock *listinit(), *firstelt(), *nextelt();
ptr arrsize();

if(e->tag!=TLIST && e->tag!=TREPOP)
	e = mknode(TREPOP, 0, arrsize(v), e);
if( listinit(v, firstelt(v), e) )
	warn("too few initializers");
if(v->vsubs)
	{
	frexpr(v->vsubs);
	v->vsubs = NULL;
	}
}



struct exprblock *listinit(v, subs, e)
register struct varblock *v;
struct exprblock *subs;
register ptr e;
{
struct varblock *vt;
register chainp p;
int n;
struct varblock *subscript();
struct exprblock *nextelt();

switch(e->tag)
	{
	case TLIST:
		for(p = e->leftp; p; p = p->nextp)
			{
			if(subs == NULL)
				goto toomany;
			subs = listinit(v, subs, p->datap);
			}
		return(subs);

	case TREPOP:
		if( ! isicon(e->leftp, &n) )
			{
			dclerr("nonconstant repetition factor");
			return(subs);
			}
		while(--n >= 0)
			{
			if(subs == NULL)
				goto toomany;
			subs = listinit(v, subs, e->rightp);
			}
		return(subs);

	default:
		if(subs == NULL)
			goto toomany;
		vt = subscript(cpexpr(v), cpexpr(subs));
		valinit(vt, e);
		frexpr(vt);
		return( nextelt(v,subs) );

	}

toomany:
	dclerr("too many initializers", NULL);
	return(NULL);
}




charinit(v,e)
ptr v;
char *e;
{
register char *bp;
char buf[50];
register int i, j;
int nwd, nch;

v = cpexpr(v);
if(v->vsubs == 0)
	v->vsubs = mknode(TLIST,0, mkchain(mkint(1),CHNULL), PNULL);

nwd = ceil( nch = conval(v->vtypep) , tailor.ftnchwd);
sprintf(buf,"%dh", tailor.ftnchwd);
for(bp = buf ; *bp ; ++bp )
	;


for(i = 0; i<nwd ; ++i)
	{
	if(i > 0) v->vsubs->leftp->datap = 
		mknode(TAROP,OPPLUS, v->vsubs->leftp->datap, mkint(1));
	prexpr( v = simple(LVAL,v) );

	for(j = 0 ; j<tailor.ftnchwd && *e!='\0' && nch-->0 ; )
		bp[j++] = *e++;
	while(j < tailor.ftnchwd)
		{
		bp[j++] = ' ';
		nch--;
		}
	bp[j] = '\0';

	putic(ICOP,OPSLASH);
	putsii(ICCONST, buf);
	putic(ICOP,OPSLASH);
	putic(ICMARK,0);
	}

frexpr(v);
}







struct exprblock *firstelt(v)
register struct varblock *v;
{
register struct dimblock *b;
register chainp s;
ptr t;
int junk;

if(v->vdim==NULL || v->vsubs!=NULL)
	fatal("firstelt: bad argument");
s = NULL;
for(b = v->vdim->datap ; b; b = b->nextp)
	{
	t = (b->lowerb ? cpexpr(b->lowerb) : mkint(1) );
	s = hookup(s, mkchain(t,CHNULL) );
	if(!isicon(b->upperb,&junk) || (b->lowerb && !isicon(b->lowerb,&junk)) )
		dclerr("attempt to initialize adjustable array",
			v->sthead->namep);
	}
return( mknode(TLIST, 0, s, PNULL) );
}




struct exprblock *nextelt(v,subs)
struct varblock *v;
struct exprblock *subs;
{
register struct dimblock *b;
register chainp *s;
int sv;

if(v == NULL)
	return(NULL);

b = v->vdim->datap;
s = subs->leftp;

while(b && s)
	{
	sv = conval(s->datap);
	frexpr(s->datap);
	if( sv < conval(b->upperb) )
		{
		s->datap =mkint(sv+1);
		return(subs);
		}
	s->datap = (b->lowerb ? cpexpr(b->lowerb) : mkint(1) );

	b = b->nextp;
	s = s->nextp;
	}

if(b || s)
	fatal("nextelt: bad subscript count");
return(NULL);
}
