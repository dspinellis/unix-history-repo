#include "defs"

#define DOCOMMON 1
#define NOCOMMON 0

dclgen()
{
register ptr p, q;
ptr q1;
chainp *y, z;
register struct stentry *s;
struct stentry **hp;
int first;
int i, j;
extern char *types[];
char *sp;

/*   print procedure statement and argument list */

for(p = prevcomments ; p ; p = p->nextp)
	{
	sp = p->datap;
	fprintf(codefile, "%s\n", sp+1);
	cfree(sp);
	}
frchain(&prevcomments);

if(tailor.procheader)
	fprintf(codefile, "%s\n", tailor.procheader);

if(procname)
	{
	p2str("      ");
	if(procname->vtype==TYSUBR || procname->vtype==TYUNDEFINED)
		p2key(FSUBROUTINE);
	else	{
		p2str(types[procname->vtype]);
		p2key(FFUNCTION);
		}

	p2str(procname->sthead->namep);
	}
else if(procclass == PRBLOCK)
	{
	p2stmt(0);
	p2key(FBLOCKDATA);
	}
else	{
	p2str("c  main program");
	if(tailor.ftnsys == CRAY)
		{
		p2stmt(0);
		p2key(FPROGRAM);
		}
	}

if(thisargs)
	{
	p2str( "(" );
	first = 1;

	for(p = thisargs ; p ; p = p->nextp)
		if( (q=p->datap)->vextbase)
			{
			if(first) first = 0;
			else p2str(", ");
			p2str(ftnames[q->vextbase]);
			}
		else	for(i=0 ; i<NFTNTYPES ; ++i)
				if(j = q->vbase[i])
					{
					if(first) first = 0;
					else p2str( ", " );
					p2str(ftnames[j]);
					}
	p2str( ")" );
	}

/* first put out declarations of variables that are used as
   adjustable dimensions
*/

y = 0;
z = & y;
for(hp = hashtab ; hp<hashend; ++hp)
	if( *hp && (q = (*hp)->varp) )
		if(q->tag==TNAME && q->vadjdim && q!=procname)
			z = z->nextp = mkchain(q,CHNULL);

dclchain(y, NOCOMMON);
frchain(&y);

/* then declare the rest of the arguments */
z = & y;
for(p = thisargs ; p ; p = p->nextp)
	if(p->datap->vadjdim == 0)
		z = z->nextp = mkchain(p->datap,CHNULL);
dclchain(y, NOCOMMON);
frchain(&y);
frchain(&thisargs);


/* now put out declarations for common blocks */
for(p = commonlist ; p ; p = p->nextp)
	prcomm(p->datap);

TEST fprintf(diagfile, "\nend of common declarations");
z = &y;

/* next the other variables that are in the symbol table */

for(hp = hashtab ; hp<hashend ; ++hp)
	if( *hp && (q = (*hp)->varp) )
		if(q->tag==TNAME && q->vadjdim==0 && q->vclass!=CLCOMMON &&
		    q->vclass!=CLARG && q!=procname &&
		    (tailor.dclintrinsics || q->vproc!=PROCINTRINSIC) )
			z = z->nextp = mkchain(q,CHNULL);

dclchain(y, NOCOMMON);
frchain(&y);

TEST fprintf(diagfile, "\nend of symbol table, start of gonelist");

/* now declare variables that are no longer in the symbol table */

dclchain(gonelist, NOCOMMON);

TEST fprintf(diagfile, "\nbeginning of hidlist");
dclchain(hidlist, NOCOMMON);

dclchain(tempvarlist, NOCOMMON);


/* finally put out equivalence statements that are generated 
   because of structure and character variables
*/
for(p = genequivs; p ; p = p->nextp)
	{
	q = p->datap;
	p2stmt(0);
	first = 1;
	p2key(FEQUIVALENCE);
	p2str( "(" );
	for(i=0; i<NFTNTYPES; ++i)
		if(q->vbase[i])
			{
			if(first) first = 0;
			else p2str( ", " );
			p2str(ftnames[ q->vbase[i] ]);
			p2str( "(1" );
			if(q1 = q->vdim)
				for(q1 = q1->datap; q1 ; q1 = q1->nextp)
					p2str( ",1" );
			p2str( ")" );
			}
	p2str( ")" );
	}
frchain(&genequivs);
}




prcomm(p)
register ptr p;
{
register int first;
register ptr q;

p2stmt(0);
p2key(FCOMMON);
p2str( "/" );
p2str(p->comname);
p2str("/ ");
first = 1;
for(q = p->comchain ; q; q = q->nextp)
	{
	if(first) first=0;
	else p2str(", ");
	prname(q->datap);
	}
dclchain(p->comchain, DOCOMMON);
}



prname(p)
register ptr p;
{
register int i;

switch(p->tag)
	{
	case TCONST:
		p2str(p->leftp);
		return;

	case TNAME:
		if( ! p->vdcldone )
			if(p->blklevel == 1)
				dclit(p);
			else	mkftnp(p);
		for(i=0; i<NFTNTYPES ; ++i)
			if(p->vbase[i])
				{
				p2str(ftnames[p->vbase[i]]);
				return;
				}
		fatal1("prname: no fortran types for name %s",
			p->sthead->namep);

	case TFTNBLOCK:
		for(i=0; i<NFTNTYPES ; ++i)
			if(p->vbase[i])
				{
				p2str(ftnames[p->vbase[i]]);
				return;
				}
		return;

	default:
		badtag("prname", p->tag);
	}
}




dclchain(chp, okcom)
ptr chp;
int okcom;
{
extern char *ftntypes[];
register ptr pn, p;
register int i;
int first, nline;
ptr q,v;
int ntypes;
int size,align,mask;
int subval;

nline = 0;
for(pn = chp ; pn ; pn = pn->nextp)
	{
	p = pn->datap;
	if( (p->tag==TNAME || p->tag==TTEMP) && p->vext!=0)
		{
		if(nline%NAMESPERLINE == 0)
			{
			p2stmt(0);
			p2key(FEXTERNAL);
			}
		else	p2str(", ");
		++nline;
		p2str(ftnames[p->vextbase]);
		}
	}


for(pn = chp ; pn ; pn = pn->nextp)
	{
	p = pn->datap;
	if( (p->tag==TNAME || p->tag==TTEMP) &&
	    p->vtype==TYSTRUCT && p->vclass!=CLARG)
		{
		ntypes = 0;
		for(i=0; i<NFTNTYPES; ++i)
			if(p->vbase[i])
				++ntypes;
		if(ntypes > 1)
			genequivs = mkchain(p, genequivs);
		}
	}

for(i=0; i<NFTNTYPES; ++i)
	{
	nline = 0;
	for(pn = chp; pn ; pn = pn->nextp)
		{
		p = pn->datap;
		if( (p->tag==TNAME || p->tag==TTEMP) &&
		    p->vtype!=TYSUBR && p->vbase[i]!=0 &&
		    (okcom || p->vclass!=CLCOMMON) )
			{
			if(nline%NAMESPERLINE == 0)
				{
				p2stmt(0);
				p2str(ftntypes[i]);
				}
			else	p2str( ", " );
			++nline;
			p2str(ftnames[p->vbase[i]]);
			first = -1;
		
			if(p->vtype==TYCHAR || p->vtype==TYSTRUCT ||
			   (p->vtype==TYLCOMPLEX && tailor.lngcxtype==NULL))
				{
				p2str( "(" );
				sizalign(p, &size,&align,&mask);
				p2int( size/tailor.ftnsize[i] );
				first = 0;
				}
			else if(p->vdim)
				{
				p2str( "(" );
				first = 1;
				}
			if(first >=0)
				{
				if(q = p->vdim)
				    for(q = q->datap ; q ; q = q->nextp)
					{
					if(q->upperb == 0)
						{
						q->upperb = mkint(1);
						if(q->lowerb)
							{
							frexpr(q->lowerb);
							q->lowerb = 0;
							}
						}
					else if(q->lowerb)
						{
						v = fold( mknode(TAROP,OPMINUS,
							mkint(1),cpexpr(q->lowerb)) );
						v = fold( mknode(TAROP,OPPLUS,
							cpexpr(q->upperb),v) );
						q->lowerb = 0;
						q->upperb = v;
						}
					if(first) first = 0;
					else p2str( ", " );
					v = q->upperb = simple(RVAL,q->upperb);
					if( (v->tag==TNAME && v->vclass==CLARG) ||
					    (isicon(v,&subval) && subval>0) )
						prname(v);
					else	dclerr("invalid array bound",
						p->sthead->namep);
					}
				p2str( ")" );
				}
			}
		}
	}
}
