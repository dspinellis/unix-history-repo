#include "defs"


hide(p)
ptr p;
{
warn1("Name %s hidden by a new declaration", p->namep);
hidlist = mkchain(p->varp, hidlist);
p->varp = 0;
++nhid[blklevel];
}



/*  remove all symbol table entries in terminated block,
    revive old hidden names
*/
unhide()
{
chainp p;
register ptr q;
register ptr v;
register struct stentry *s;
struct stentry **hp;

for(hp = hashtab ; hp<hashend ; ++hp)
	if(s = *hp)
		{
		if( (v = s->varp) && v->blklevel == blklevel)
			{
			if(v->tag==TLABEL)
				if(blklevel <= 1)
					{
					if(v->labdefined==0)
						laberr("%s never defined",
							v->sthead->namep);
					s->varp = 0;
					}
				else	{ /* move label out a level */
					if(v->labdefined)
						v->labinacc = 1;
					v->blklevel--;
					++ndecl[blklevel-1];
					}
			else	{
				if(v->tag == TNAME)
					{
					TEST fprintf(diagfile,"gone(%s) level %d\n",
						s->namep, blklevel);
					gonelist = mkchain(s->varp, gonelist);
					}

				else if(v->tag!=TSTRUCT)
					{
					++ndecl[blklevel];
					if(v->tag==TDEFINE)
						frdef(v);
					}
				s->varp = 0;
				}
			--ndecl[blklevel];
			}
		}

for( p=hidlist  ;  p && ((v = (q=p->datap)->sthead)->varp==NULL) ; p=hidlist )
	{
	v->varp = q;
	v->tag = q->tag;
	v->subtype = q->subtype;
	if(v->blklevel > q->blklevel)
		v->blklevel = q->blklevel;
	hidlist = p->nextp;
	p->nextp = CHNULL;
	frchain(&p);
	--nhid[blklevel];
TEST fprintf(diagfile, "unhide(%s), blklevel %d\n", v->namep, v->blklevel);
	}
if(ndecl[blklevel] != 0)
	{
	sprintf(msg, "%d declarations leftover at block level %d",
		ndecl[blklevel], blklevel);
	fatal(msg);
	}
if(nhid[blklevel] != 0)
	fatal("leftover hidden variables");
}




ptr bgnexec()
{
register ptr p;

p = allexcblock();
p->tag = TEXEC;
p->prevexec = thisexec;
if(thisexec && thisexec->copylab)
	{
	p->labelno = thisexec->labelno;
	p->labused = thisexec->labused;
	thisexec->labelno = 0;
	}
thisexec = p;
return(p);
}


ptr addexec()
{
register ptr p;
register ptr q;

q = thisexec;
p = q->prevexec;

if(q->temps)
	tempvarlist = hookup(q->temps, tempvarlist);

p->brnchend = q->brnchend;
p->nftnst += q->nftnst;
p->labeled |= q->labeled;
p->uniffable |= q->uniffable;

if(q->labelno && !(q->labused))
	{
	if(q->nxtlabno)
		exnull();
	else q->nxtlabno = q->labelno;
	}

thisexec = p;

if(q->nxtlabno)
	{
	if(p->labelno && !(p->labused))
		exnull();
	p->labelno = q->nxtlabno;
	p->labused = 0;
	}

frexcblock(q);
return(p);
}



pushctl(t,vp)
int t;
register ptr vp;
{
register ptr q;
ptr p;
int junk;

q = allexcblock();
q->tag = TCONTROL;
q->subtype = t;
q->loopvar = vp;
q->prevctl = thisctl;
thisctl = q;

switch(t)
	{
	case STSWITCH:
		q->xlab = nextlab();
		q->nextlab = 0;
		exgoto(q->xlab);
		ncases = -1;
		break;

	case STFOR:
		exlab(0);
		q->nextlab = nextlab();
		q->xlab = nextlab();
		break;

	case STWHILE:
		q->nextlab = thislab();
		if(vp)
			exifgo( mknode(TNOTOP,OPNOT,vp,PNULL),
				q->breaklab = nextlab() );
		else	thisexec->copylab = 1;
		break;

	case STREPEAT:
		exnull();
		q->xlab = thislab();
		thisexec->copylab = 1;
		junk = nextindif();
		indifs[junk] = 0;
		q->indifn = junk;
		indifs[q->indifn] = q->xlab;
		break;

	case STDO:
		q->nextlab = nextlab();
		exlab(0);
		putic(ICKEYWORD,FDO);
		putic(ICLABEL, q->nextlab);
		putic(ICBLANK, 1);
		p = mknode(TASGNOP,OPASGN,vp->dovar,vp->dopar[0]);
		prexpr(p);
		frexpr(p);
		putic(ICOP, OPCOMMA);
		prexpr(vp->dopar[1]);
		frexpr(vp->dopar[1]);
		if(vp->dopar[2])
			{
			putic(ICOP, OPCOMMA);
			prexpr(vp->dopar[2]);
			frexpr(vp->dopar[2]);
			}
		cfree(vp);
		break;

	case STIF:
		exif(vp);
		thisexec->nftnst = 0;
		break;

	default:
		fatal1("pushctl: invalid control block type %d", t);
	}

++ctllevel;
}



popctl()
{
register ptr p;
ptr newp;
chainp q;
int first, deflabno, blab, cmin, cmax, range, caseval, optcase;
int labp[MAXSWITCH];

if(thisctl == 0)
	fatal("empty control stack popped");

switch(thisctl->subtype)
	{
	case STSWITCH:
/*		if(thisexec->brnchend == 0)	*/
			{
			if(thisctl->breaklab == 0)
				thisctl->breaklab = nextlab();
			exgoto(thisctl->breaklab);
			}
		exlab(thisctl->xlab);
		deflabno = 0;
		first = YES;
		optcase = (thisctl->loopvar->vtype == TYINT);

		for(p=thisctl->loopctl ; p!=0 ; p = p->nextcase)
			if(p->labdefined == 0)
				{
				laberr("undefined case label", CNULL);
				optcase = NO;
				}
			else if(p->casexpr == 0)
				deflabno = p->labelno;
			else if( isicon(p->casexpr, &caseval))
				{
				if(first)
					{
					first = NO;
					cmin = cmax = caseval;
					}
				else	{
					if(caseval < cmin)
						cmin = caseval;
					if(caseval > cmax)
						cmax = caseval;
					}
				++ncases;
				}
			else	optcase = NO;

		range = cmax - cmin + 1;
		if(optcase && ncases>2 && range<2*ncases && range<MAXSWITCH)
			{
			register int i;
			for(i=0; i<range ; ++i)
				labp[i] = 0;
			for(p=thisctl->loopctl ; p!=0 ; p = p->nextcase)
				if(p->labdefined && p->casexpr)
					{
					isicon(p->casexpr, &caseval);
					frexpr(p->casexpr);
					labp[caseval-cmin] = p->labelno;
					}
			
			q = CHNULL;
			blab = (deflabno ? deflabno : thisctl->breaklab);
			for(i=range-1 ; i>=0 ; --i)
				q = mkchain(labp[i] ? labp[i] : blab, q);
			excompgoto(q, mknode(TAROP,OPPLUS, mkint(1-cmin),
				     cpexpr(thisctl->loopvar) ));
			}
		else	{
			for(p=thisctl->loopctl ; p!=0 ; p = p->nextcase)
				if(p->labdefined && p->casexpr)
					exifgo( mknode(TRELOP,OPEQ,
					   cpexpr(thisctl->loopvar),p->casexpr),
					   p->labelno);
			}
		if(deflabno)
			exgoto(deflabno);

		for(p = thisctl->loopctl ; p; p = newp)
			{
			newp = p->nextcase;
			cfree(p);
			}
		thisctl->loopctl = NULL;
		break;

	case STFOR:
		exgoto(thisctl->nextlab);
		break;

	case STWHILE:
		exgoto(thisctl->nextlab);
		break;

	case STREPEAT:
		break;

	case STDO:
		exnull();
		exlab(thisctl->nextlab);
		putic(ICKEYWORD,FCONTINUE);
		break;

	case STIF:
		break;

	case STPROC:
		break;

	default:
		fatal1("popctl: invalid control block type %d",
			thisctl->subtype);
	}

if(thisctl->breaklab != 0)
	thisexec->nxtlabno = thisctl->breaklab;
p = thisctl->prevctl;
frexcblock(thisctl);
thisctl = p;
--ctllevel;
}
