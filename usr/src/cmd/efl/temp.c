#include "defs"

ptr gentemp(t)
ptr t;
{
register ptr oldp;
register ptr p;
register ptr q;
int ttype;
ptr ttypep, tdim;

/* search the temporary list for a matching type */

ttype = t->vtype;
ttypep = t->vtypep;
tdim = t->vdim;

for(oldp = &tempvarlist ; p = oldp->nextp ; oldp = p)
	if( (q = p->datap) && (q->vtype == ttype) &&
	  (q->vtypep == ttypep) && eqdim(q->vdim,tdim) )
		{
		oldp->nextp = p->nextp;
		break;
		}

if(p == PNULL)
	{
	q = allexpblock();
	q->tag = TTEMP;
	q->subtype = t->subtype;
	q->vtype = ttype;
	q->vclass = t->vclass;
	q->vtypep = ( ttypep ? cpexpr(ttypep) : PNULL);
	q->vdim = tdim;
	mkftnp(q);	/* assign fortran types */

	p = mkchain(q, CHNULL);
	p->datap = q;
	}

p->nextp = thisexec->temps;
thisexec->temps = p;

return( cpexpr(q) );
/* need a copy of the block for the temporary list and another for use */
}


ptr gent(t,tp)  /* make a temporary of type t, typepointer tp */
int t;
ptr tp;
{
static struct varblock model;

model.vtype = t;
model.vtypep = tp;

return( gentemp(&model) );
}
