#include "defs"

/* ROUTINES RELATED TO EQUIVALENCE CLASS PROCESSING */

/* called at end of declarations section to process chains
   created by EQUIVALENCE statements
 */
doequiv()
{
register int i;
int inequiv, comno, ovarno;
ftnint comoffset, offset, leng, iarrlen(), lmin(), lmax();
register struct equivblock *p;
register struct eqvchain *q;
struct primblock *itemp;
register struct nameblock *np;
expptr offp, suboffset();
int ns, nsubs();
chainp cp;

for(i = 0 ; i < nequiv ; ++i)
	{
	p = &eqvclass[i];
	p->eqvbottom = p->eqvtop = 0;
	comno = -1;

	for(q = p->equivs ; q ; q = q->nextp)
		{
		itemp = q->eqvitem;
		vardcl(np = itemp->namep);
		if(itemp->argsp || itemp->fcharp)
			{
			if(np->vdim!=NULL && np->vdim->ndim>1 &&
			   nsubs(itemp->argsp)==1 )
				{
				if(! ftn66flag)
					warn("1-dim subscript in EQUIVALENCE");
				cp = NULL;
				ns = np->vdim->ndim;
				while(--ns > 0)
					cp = mkchain( ICON(1), cp);
				itemp->argsp->listp->nextp = cp;
				}
			offp = suboffset(itemp);
			}
		else	offp = ICON(0);
		if(ISICON(offp))
			offset = q->eqvoffset = offp->const.ci;
		else	{
			dclerr("nonconstant subscript in equivalence ", np);
			np = NULL;
			goto endit;
			}
		if( (leng = iarrlen(np)) < 0)
			{
			dclerr("adjustable in equivalence", np);
			np = NULL;
			goto endit;
			}
		p->eqvbottom = lmin(p->eqvbottom, -offset);
		p->eqvtop = lmax(p->eqvtop, leng-offset);

		switch(np->vstg)
			{
			case STGUNKNOWN:
			case STGBSS:
			case STGEQUIV:
				break;

			case STGCOMMON:
				comno = np->vardesc.varno;
				comoffset = np->voffset + offset;
				break;

			default:
				dclerr("bad storage class in equivalence", np);
				np = NULL;
				goto endit;
			}
	endit:
		frexpr(offp);
		q->eqvitem = np;
		}

	if(comno >= 0)
		eqvcommon(p, comno, comoffset);
	else  for(q = p->equivs ; q ; q = q->nextp)
		{
		if(np = q->eqvitem)
			{
			inequiv = NO;
			if(np->vstg==STGEQUIV)
				if( (ovarno = np->vardesc.varno) == i)
					{
					if(np->voffset + q->eqvoffset != 0)
						dclerr("inconsistent equivalence", np);
					}
				else	{
					offset = np->voffset;
					inequiv = YES;
					}

			np->vstg = STGEQUIV;
			np->vardesc.varno = i;
			np->voffset = - q->eqvoffset;

			if(inequiv)
				eqveqv(i, ovarno, q->eqvoffset + offset);
			}
		}
	}

for(i = 0 ; i < nequiv ; ++i)
	{
	p = & eqvclass[i];
	if(p->eqvbottom!=0 || p->eqvtop!=0)
		{
		for(q = p->equivs ; q; q = q->nextp)
			{
			np = q->eqvitem;
			np->voffset -= p->eqvbottom;
			if(np->voffset % typealign[np->vtype] != 0)
				dclerr("bad alignment forced by equivalence", np);
			}
		p->eqvtop -= p->eqvbottom;
		p->eqvbottom = 0;
		}
	freqchain(p);
	}
}





/* put equivalence chain p at common block comno + comoffset */

LOCAL eqvcommon(p, comno, comoffset)
struct equivblock *p;
int comno;
ftnint comoffset;
{
int ovarno;
ftnint k, offq;
register struct nameblock *np;
register struct eqvchain *q;

if(comoffset + p->eqvbottom < 0)
	{
	err1("attempt to extend common %s backward",
		nounder(XL, extsymtab[comno].extname) );
	freqchain(p);
	return;
	}

if( (k = comoffset + p->eqvtop) > extsymtab[comno].extleng)
	extsymtab[comno].extleng = k;

for(q = p->equivs ; q ; q = q->nextp)
	if(np = q->eqvitem)
		{
		switch(np->vstg)
			{
			case STGUNKNOWN:
			case STGBSS:
				np->vstg = STGCOMMON;
				np->vardesc.varno = comno;
				np->voffset = comoffset - q->eqvoffset;
				break;

			case STGEQUIV:
				ovarno = np->vardesc.varno;
				offq = comoffset - q->eqvoffset - np->voffset;
				np->vstg = STGCOMMON;
				np->vardesc.varno = comno;
				np->voffset = comoffset - q->eqvoffset;
				if(ovarno != (p - eqvclass))
					eqvcommon(&eqvclass[ovarno], comno, offq);
				break;

			case STGCOMMON:
				if(comno != np->vardesc.varno ||
				   comoffset != np->voffset+q->eqvoffset)
					dclerr("inconsistent common usage", np);
				break;


			default:
				fatal1("eqvcommon: impossible vstg %d", np->vstg);
			}
		}

freqchain(p);
p->eqvbottom = p->eqvtop = 0;
}


/* put all items on ovarno chain on front of nvarno chain
 * adjust offsets of ovarno elements and top and bottom of nvarno chain
 */

LOCAL eqveqv(nvarno, ovarno, delta)
int ovarno, nvarno;
ftnint delta;
{
register struct equivblock *p0, *p;
register struct nameblock *np;
struct eqvchain *q, *q1;

p0 = eqvclass + nvarno;
p = eqvclass + ovarno;
p0->eqvbottom = lmin(p0->eqvbottom, p->eqvbottom - delta);
p0->eqvtop = lmax(p0->eqvtop, p->eqvtop - delta);
p->eqvbottom = p->eqvtop = 0;

for(q = p->equivs ; q ; q = q1)
	{
	q1 = q->nextp;
	if( (np = q->eqvitem) && np->vardesc.varno==ovarno)
		{
		q->nextp = p0->equivs;
		p0->equivs = q;
		q->eqvoffset -= delta;
		np->vardesc.varno = nvarno;
		np->voffset -= delta;
		}
	else	free(q);
	}
p->equivs = NULL;
}




LOCAL freqchain(p)
register struct equivblock *p;
{
register struct eqvchain *q, *oq;

for(q = p->equivs ; q ; q = oq)
	{
	oq = q->nextp;
	free(q);
	}
p->equivs = NULL;
}





LOCAL nsubs(p)
register struct listblock *p;
{
register int n;
register chainp q;

n = 0;
if(p)
	for(q = p->listp ; q ; q = q->nextp)
		++n;

return(n);
}
