#include "defs"


cleanst()
{
register ptr p, q;
ptr pjunk;
int i;
register struct stentry *s;
struct stentry **hp;

TEST fprintf(diagfile, "\n");

clcomm();

for(hp = hashtab ; hp<hashend ; ++hp)
    while( s = *hp )
	{
	if( q = s->varp )
		{
		if( q->blklevel > 0 )
			{
			TEST fprintf(diagfile, "remove %s from st\n", s->namep);
			switch(q->tag)
				{
				case TNAME:
					frvar(q);
					break;

				case TSTRUCT:
					frtype(q);
					break;

				case TDEFINE:
					frdef(q);
					break;

				case TLABEL:
					cfree(q);
					break;

				default:
					sprintf(msg, "cleanst: illegal entry tag %d, ptr %o, name %s.",
						q->tag, q, s->namep);
					fatal(msg);
				}
			}
		else if( q->tag == TNAME )
			{
			q->vdcldone = 0;
			q->vnamedone = 0;
			q->vextbase = 0;
			for(i = 0 ; i<NFTNTYPES ; ++i)
				q->vbase[i] = 0;
			}
		}
	if(s->blklevel > 0)
		name(s->namep,-1);
	else	break;
	}

for(p = gonelist ; p ; p = p->nextp)
	frvar(p->datap);
frchain(&gonelist);

if(hidlist) fatal("cleanst: hidlist not empty");
for(p = hidlist ; p ; p = p->nextp)
	frvar(p->datap);
frchain(&hidlist);

for(p = tempvarlist ; p ; p = p->nextp)
	frvar(p->datap);
frchain(&tempvarlist);

for(p = temptypelist ; p ; p = p->nextp)
	if(p->datap->blklevel > 0)
		frtype(p->datap);
frchain(&temptypelist);

q = &arrays;
for(p = arrays ; p ; p = q->nextp)
	if(p->datap == 0)
		{
		q->nextp = p->nextp;
		p->nextp = 0;
		pjunk = p;
		frchain(&pjunk);
		}
	else	q = p;
}



frvar(p)
register ptr p;
{
register ptr q, qn;

if(p==0) return;

switch(p->tag)
	{
	case TSTRUCT:
		frtype(p);
		return;

	case TDEFINE:
		frdef(p);
		return;

	case TNAME:
	case TTEMP:
		if(q = p->vdim)
		    for(q = q->datap ; q ; q = qn)
			{
			if(q->lowerb) frexpr(q->lowerb);
			frexpr(q->upperb);
			qn = q->nextp;
			cfree(q);
			}
		
		if(p->vdim)
			p->vdim->datap = 0;
		if(p->vtype == TYCHAR)
			frexpr(p->vtypep);
		frexpblock(p);
		return;

	default:
		badtag("frvar",p->tag);
	}
}


frtype(p)
register ptr p;
{
register ptr q;

if(p==0 || p->tag!=TSTRUCT)
	fatal("frtype: bad argument");
for(q = p->strdesc ; q; q = q->nextp)
	frvar(q->datap);
frchain( &(p->strdesc) );
cfree(p);
}



frdef(p)
ptr p;
{
cfree(p->valp);
cfree(p);
}



frexpr(p)
register ptr p;
{
register ptr q;

if(p == 0) return;

switch(p->tag)
	{
	case TAROP:
	case TRELOP:
	case TLOGOP:
	case TASGNOP:
	case TREPOP:
	case TCALL:
		frexpr(p->rightp);


	case TNOTOP:
	case TNEGOP:
		frexpr(p->leftp);
		break;

	case TCONST:
		cfree(p->leftp);
		if(p->vtype == TYCHAR)
			frexpr(p->vtypep);
		if(p->rightp)
			cfree(p->rightp);
		break;

	case TLIST:
		for(q = p->leftp ; q ; q = q->nextp)
			frexpr(q->datap);
		frchain( &(p->leftp) );
		break;

	case TTEMP:
	case TNAME:
	case TFTNBLOCK:
		if(p->vsubs)
			frexpr(p->vsubs);
		if(p->voffset)
			frexpr(p->voffset);

	case TERROR:
/*debug*/ case TIOSTAT:
		break;

	default:
		badtag("frexpr", p->tag);
	}
frexpblock(p);
}




clcomm()	/* clean up common lists */
{
ptr p, oldp, q;

for(oldp = &commonlist ; p = oldp->nextp ;  )
	{
	q = p->datap;

	if(q->blklevel > 0)
		{
		frchain( &(q->comchain) );
		cfree(q);
		oldp->nextp = p->nextp;
		cfree(p);
		}
	else   oldp = p;
	}
}
