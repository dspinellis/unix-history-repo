#include "defs"



ptr mkfield(q)
register ptr q;
{
register ptr p;

if(!instruct)
	goto err;
else if(q->upperb == 0)
	dclerr("must have upper bound in field spcification", "");
else
	{
	p = ALLOC(fieldspec);
	p->tag = TFIELD;
	if(q->lowerb)
		{
		p->flbound = q->lowerb;
		p->frange = mknode(TAROP,OPPLUS,mknode(TAROP,OPMINUS,
				q->upperb, cpexpr(q->lowerb)),
				mkconst(TYINT,"1") );
		}
	else	{
		p->flbound = mkconst(TYINT,"1");
		p->frange = q->upperb;
		}
	p->frange = simple(RVAL,p->frange);
	if(p->frange->tag != TCONST)
		{
		dclerr("field range must be constant", "");
		cfree(p);
		goto err;
		}
	cfree(q);
	return(p);
	}

err:
	cfree(q);
	return( errnode() );
}





ptr extrfield(p)
register ptr p;
{
register ptr t;

t = p->vtypep;
p->vtype = TYINT;
p->vtypep = 0;

if(t->frshift)
	p = mknode(TAROP,OPSLASH, p, cpexpr(t->frshift));
if(t->fanymore)
	p = mkcall(builtin(TYINT, "mod"), arg2(p, cpexpr(t->frange)) );
p = mknode(TAROP,OPPLUS, p, cpexpr(t->flbound));
return(p);
}




ptr setfield(e)
ptr e;
{
ptr lp, rp;
register ptr f, p;
int subt;

lp = cpexpr(e->leftp);
rp = e->rightp;
subt = e->subtype;
f = lp->vtypep;
lp->vtype = TYINT;
lp->vtypep = 0;

if(subt==OPPLUS || subt==OPMINUS)
	{
	if(f->frshift)
		rp = mknode(TAROP,OPSTAR,rp,cpexpr(f->frshift));
	}
else	{
	if(subt != OPASGN)
		{
		rp = mknode(TAROP,subt, extrfield(cpexpr(e->leftp)), rp);
		subt = OPASGN;
		}
	rp = coerce(TYINT,rp);
	if(f->flbound)
		rp = simple(RVAL, mknode(TAROP,OPMINUS,rp,cpexpr(f->flbound)) );
	
	if(f->frshift==0)
		{
		if(f->fanymore)
			{
			p = mknode(TAROP,OPSLASH,cpexpr(lp),cpexpr(f->frange));
			p->needpar = YES;
			p = mknode(TAROP,OPSTAR,cpexpr(f->frange),p);
			rp = mknode(TAROP,OPPLUS,p,rp);
			}
		}
	else if(f->fanymore==0)
		{
		rp = mknode(TAROP,OPSTAR,cpexpr(f->frshift),rp);
		p = mkcall(builtin(TYINT,"mod"),
			arg2(cpexpr(lp),cpexpr(f->frshift)) );
		rp = mknode(TAROP,OPPLUS, p,rp);
		}
	else	{
		p = mknode(TAROP,OPSLASH,cpexpr(lp),cpexpr(f->frshift));
		p = mkcall(builtin(TYINT,"mod"), 
			arg2(p, cpexpr(f->frange)) );
		if( rp->tag!=TCONST || !equals(rp->leftp, "0") )
			p = mknode(TAROP,OPMINUS, p, rp);
		rp = mknode(TAROP,OPSTAR, cpexpr(f->frshift), p);
		rp = mknode(TAROP,OPMINUS, cpexpr(lp), rp);
		}
	}
frexpr( simple(LVAL, mknode(TASGNOP,subt,lp,rp) ));
return(extrfield(e->leftp));
}
