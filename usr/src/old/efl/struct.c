#include "defs"

offsets(s)
register ptr s;
{
register ptr p, q;
ptr t;
ptr prevp;
int n;
int size, align, mask, nelt;
double rshift;

s->stralign = 1;
s->strsize = 0;
s->basetypes = 0;
prevp = 0;
rshift = 0;

for(p = s->strdesc ; p ; p = p->nextp)
	{
	q = p->datap;
	if(q->vclass != 0)
		dclerr("attempt to give storage class to mos",
			q->namep);
	else  q->vclass = CLMOS;
	if(q->vtype == TYUNDEFINED)
		impldecl(q);

	sizalign(q, &size, &align, &mask);
	s->strsize = evenoff(s->strsize, align);
	q->voffset = mkint(s->strsize);
	/* sloppy formula */
	nelt = 1;
	if(t = q->vdim)
	    for(t = t->datap ; t ; t = t->nextp)
		{
		if(t->upperb == 0) continue;
		n = conval(t->upperb);
		if(t->lowerb)
			n -= conval(t->lowerb)-1;
		nelt *= n;
		}
	if(q->vtype==TYFIELD && q->vdim==0 &&
	     (n=conval(q->vtypep->frange))*rshift<=fieldmax && rshift>0)
		{
		prevp->vtypep->fanymore = 1;
		q->vtypep->frshift = mkint( (int) rshift );
		rshift *= n;
		cfree(q->voffset);
		q->voffset = mkint(s->strsize - tailor.ftnsize[FTNINT]);
		}
	else	{
		if(q->vdim!=0 && q->vtype==TYFIELD)
			q->vtype = TYINT;
		rshift = (q->vtype==TYFIELD ? n : 0);
		s->strsize +=  nelt * evenoff(size,align);
		s->stralign = lcm(s->stralign, align);
		s->basetypes |= mask;
		}
	prevp = q;
	}
}


lcm(a,b)
int a,b;
{
int ab, c;

if( (ab = a*b) == 0) return(0);

while(b)
	{
	c = a%b;
	a = b;
	b = c;
	}

return(ab/a);
}





sizalign(p, s, a, m)
register ptr p;
int *s;
int *a;
int *m;
{
register int k, t;

if(p->tag == TERROR)
	return;
if(p->tag!=TNAME && p->tag!=TTEMP && p->tag!=TFTNBLOCK)
	badtag("sizalign", p->tag);
switch(t = p->vtype)
	{
	case TYFIELD:
	case TYINT:
	case TYREAL:
	case TYLREAL:
	case TYCOMPLEX:
	case TYLOG:
		k = eflftn[t];
		*s = tailor.ftnsize[k];
		*a = tailor.ftnalign[k];
		*m = ftnmask[k];
		return;

	case TYLCOMPLEX:
		if(tailor.lngcxtype)
			{
			k = FTNDCOMPLEX;
			*s = tailor.ftnsize[FTNDCOMPLEX];
			}
		else
			{
			k = FTNDOUBLE;
			*s = 2*tailor.ftnsize[k];
			}
		*a = tailor.ftnalign[k];
		*m = ftnmask[k];
		return;

	case TYSTRUCT:
		*s = p->vtypep->strsize;
		*a = p->vtypep->stralign;
		*m = p->vtypep->basetypes;
		return;

	case TYCHAR:
		*s = tailor.ftnsize[FTNINT] *
			ceil(conval(p->vtypep), tailor.ftnchwd);
		*a = tailor.ftnalign[FTNINT];
		*m = ftnmask[FTNINT];
		return;

	case TYSUBR:
		*s = 1;
		*a = 1;
		*m = 1;
		dclerr("subroutine name as variable", p->sthead->namep);
		return;

	default:
		fatal1("sizalign: invalid type %d", t);
	}
}



evenoff(a,b)	/* round a up to a multiple of b */
int a,b;
{
return(b * ceil(a,b));
}


ceil(a,b)
int a,b;
{
return( (a+b-1)/b );
}




ptr esizeof(type, typep, dim)
register int type;
register ptr typep;
ptr dim;
{
register int k;

switch(type)
	{
	case TYFIELD:
	case TYINT:
	case TYREAL:
	case TYLREAL:
	case TYCOMPLEX:
	case TYLCOMPLEX:
	case TYLOG:
		k = tailor.ftnsize[ eflftn[type] ];
		break;

	case TYSTRUCT:
		k = typep->strsize;
		break;

	case TYCHAR:
		k = tailor.ftnsize[FTNINT] * ceil(conval(typep), tailor.ftnchwd);
		break;

	default:
		exprerr("invalid sizeof", "");
		k = 0;
	}
/* debug version.  should multiply by dimension */
return( mkint(k) );
}



ptr elenof(type, typep, dim)
register int type;
register ptr typep;
ptr dim;
{
if(type == TYCHAR)
	return( mkint( conval(typep) ) );
exprerr("invalid lengthof", "");
return( mkint(0) );
/* debug version.  should multiply by dimension */
}
