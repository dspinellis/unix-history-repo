#include "defs"

struct varblock *subscript(v,s)
register ptr v,s;
{
ptr p;
register ptr q;
ptr bounds, subs;
int size, align, mask;

if(v->tag == TERROR)
	goto ret;
if(v->tag!=TNAME && v->tag!=TTEMP)
	badtag("subscript", v->tag);
if(s->tag == TERROR)
	{
	v->vsubs = 0;
	goto ret;
	}

if(s->tag != TLIST)
	badtag("subscript", s->tag);
sizalign(v, &size, &align, &mask);
if(bounds = v->vdim)
	bounds = bounds->datap;
subs = s->leftp;

while ( bounds && subs)
	{
	if(bounds->lowerb)
		{
		p = mknode(TAROP,OPMINUS,mkint(1),cpexpr(bounds->lowerb));
		subs->datap = mknode(TAROP,OPPLUS, subs->datap, p);
		}
	bounds = bounds->nextp;
	subs = subs->nextp;
	}
v->vdim = 0;
if(bounds || subs)
	{
	exprerr("subscript and bounds of different length", CNULL);
	v->vsubs = 0;
	goto ret;
	}

if(v->vsubs)
	{ /* special case of subscripted type element */
	if(s->leftp==0 || s->leftp->nextp!=0)
		{
		exprerr("not exactly one subscript on type member", CNULL);
		v->vsubs = 0;
		goto ret;
		}
	q = mknode(TAROP,OPMINUS,s->leftp->datap, mkint(1) );
	q = mknode(TAROP,OPSTAR, mkint(size), q);
	if(v->voffset)
		v->voffset = mknode(TAROP,OPPLUS,v->voffset, q);
	else	v->voffset = q;
	goto ret;
	}

v->vsubs = s;

if(v->vtype==TYCHAR || v->vtype==TYSTRUCT ||
	(v->vtype==TYLCOMPLEX && tailor.lngcxtype==NULL) )
	{ /* add an initial unit subscript */
	s->leftp = mkchain(mkint(1), s->leftp);
	}

else	{   /* add to offset, set first subscript to 1 */
	q = mknode(TAROP,OPMINUS,s->leftp->datap, mkint(1) );
	q = mknode(TAROP,OPSTAR, mkint(size), q);
	if(v->voffset)
		v->voffset = mknode(TAROP,OPPLUS,v->voffset, q);
	else	v->voffset = q;

	s->leftp->datap = mkint(1);
	}
ret:
	return(v);
}





ptr strucelt(var, subelt)
register ptr var;
ptr subelt;
{
register ptr p, q;

if(var->tag == TERROR)
	return(var);
if(var->vtype!=TYSTRUCT || var->vtypep==0 || var->vdim!=0)
	{
	exprerr("attempt to find a member in an array or non-structure", CNULL);
	return(errnode());
	}
for(p = var->vtypep->strdesc ; p ; p = p->nextp)
	if(subelt == p->datap->sthead) break;
if(p == 0)
	{
	exprerr("%s is not in structure\n", subelt->namep);
	return(errnode());
	}
q = p->datap;
var->vdim = q->vdim;
var->vtypep = q->vtypep;
if(q->voffset)
	if(var->voffset)
		var->voffset = mknode(TAROP,OPPLUS,var->voffset,cpexpr(q->voffset));
	else	{
		var->voffset = cpexpr(q->voffset);
		}
if( (var->vtype = q->vtype) != TYSTRUCT)
	convtype(var);
return(var);
}



convtype(p)
register ptr p;
{
register int i, k;
ptr mksub1();

switch(p->vtype)
	{
	case TYFIELD:
	case TYINT:
	case TYCHAR:
	case TYREAL:
	case TYLREAL:
	case TYCOMPLEX:
	case TYLOG:
		k = eflftn[p->vtype];
		break;

	default:
		fatal("convtype: impossible type");
	}

for(i=0; i<NFTNTYPES; ++i)
	if(i != k) p->vbase[i] = 0;
	else if(p->vbase[i]==0)
		{
		exprerr("illegal combination of array and dot",CNULL);
		mvexpr(errnode(), p);
		return;
		}

if(p->vsubs == 0)
	p->vsubs = mksub1();

}



fixsubs(p)
register ptr p;
{
ptr q, *firstsub;
int size,align,mask;

if(p->voffset)
	{
	firstsub = &(p->vsubs->leftp->datap);
	sizalign(p, &size,&align,&mask);
	if(p->vtype == TYCHAR)
		size = tailor.ftnsize[FTNINT];

	q = mknode(TAROP,OPSLASH,p->voffset,mkint(size));
	*firstsub = mknode(TAROP,OPPLUS, q, *firstsub);
	p->voffset = 0;
	}
}
