#include "defs"

impldecl(p)
register ptr p;
{
extern char *types[];
register ptr q;
int n;

if(p->vtype==TYSUBR) return;
if(p->tag == TCALL)
	{
	impldecl(p->leftp);
	p->vtype = p->leftp->vtype;
	p->vtypep = p->leftp->vtypep;
	return;
	}

if(inbound)
	n = TYINT;
else	{
	n = impltype[p->sthead->namep[0] - 'a' ];
	if(n==TYREAL && p->vprec!=0)
		n = TYLREAL;
	sprintf(msg,  "%s implicitly typed %s",p->sthead->namep, types[n]);
	warn(msg);
	}
q = p->sthead->varp;
p->vtype = q->vtype = n;
if(p->blklevel>1 && p->vdclstart==0)
	{
	p->blklevel = q->blklevel = p->sthead->blklevel = 1;
	p->vdclstart = q->vdclstart = 1;
	--ndecl[blklevel];
	++ndecl[1];
	}
}



extname(p)
register ptr p;
{
register int i;
register char *q, *s;

/*	if(p->vclass == CLARG) return;	*/
if(p->vextbase) return;
q = p->sthead->namep;
setvproc(p, PROCYES);

/* external names are automatically at block level 1 */

if( (i =p->blklevel) >1)
	{
	p->sthead->blklevel = 1;
	p->blklevel = 1;
	p->sthead->varp->blklevel = 1;
	++ndecl[1];
	--ndecl[i];
	}

if(p->vclass!=CLUNDEFINED && p->vclass!=CLARG)
	{
	dclerr("illegal class for procedure", q);
	return;
	}
if(p->vclass!=CLARG && strlen(q)>XL)
	{
	if(! ioop(q) )
		dclerr("procedure name too long", q);
	return;
	}
if(lookftn(q) > 0)
	dclerr("procedure name already used", q);
else	{
	for(i=0 ; i<NFTNTYPES ; ++i)
		if(p->vbase[i]) break;
	if(i < NFTNTYPES)
		p->vextbase = p->vbase[i];
	else	p->vextbase = nxtftn();

	if(p->vext==0 || p->vclass!=CLARG)
		for(s = ftnames[ p->vextbase ]; *s++ = *q++ ; ) ; 
	return;
	}
}



dclit(p)
register ptr p;
{
register ptr q;

if(p->tag == TERROR)
	return;

q = p->sthead->varp;

if(p->tag == TCALL)
	{
	dclit(p->leftp);
	if( ioop(p->leftp->sthead->namep) )
		p->leftp->vtype = TYLOG;
	p->vtype = p->leftp->vtype;
	p->vtypep = p->leftp->vtypep;
	return;
	}

if(q->vdcldone == 0)
	mkftnp(q);
if(p != q)
	cpblock(q,p, sizeof(struct exprblock));
}


mkftnp(p)
register ptr p;
{
int i,k;
if(inbound || p->vdcldone) return;
if(p == 0)
	fatal("mkftnp: zero argument");
if(p->tag!=TNAME && p->tag!=TTEMP)
	badtag("mkftnp", p->tag);

if(p->vtype == TYUNDEFINED)
	if(p->vextbase)
		return;
	else	impldecl(p);
p->vdcldone = 1;

switch(p->vtype)
	{
	case TYCHAR:
	case TYINT:
	case TYREAL:
	case TYLREAL:
	case TYLOG:
	case TYCOMPLEX:
	case TYLCOMPLEX:
		p->vbase[ eflftn[p->vtype] ] = nxtftn();
		break;

	case TYSTRUCT:
		k = p->vtypep->basetypes;
		for(i=0; i<NFTNTYPES ; ++i)
			if(k & ftnmask[i])
				p->vbase[i] = nxtftn();
		break;

	case TYSUBR:
		break;

	default:
		fatal1("invalid type for %s", p->sthead->namep);
		break;
	}
}


namegen()
{
register ptr p;
register struct stentry **hp;
register int i;

for(hp = hashtab ; hp<hashend ; ++hp)
	if(*hp && (p = (*hp)->varp) )
		if(p->tag == TNAME)
			mkft(p);

for(p = gonelist ; p ; p = p->nextp)
	mkft(p->datap);

for(p = hidlist ; p ; p = p->nextp)
	if(p->datap->tag == TNAME)  mkft(p->datap);

for(p = tempvarlist ; p ; p = p->nextp)
	mkft(p->datap);

TEST fprintf(diagfile, "Fortran names:\n");
TEST for(i=1; i<=nftnames ; ++i)  fprintf(diagfile, "%s\n", ftnames[i]);
}


mkft(p)
register ptr p;
{
int i;
register char *s, *t;

if(p->vnamedone)
	return;

if(p->vdcldone==0 && p!=procname)
	{
	if(p->vext && p->vtype==TYUNDEFINED)
		p->vtype = TYSUBR;
	else if(p->vextbase==0 && p->vadjdim==0 && p->vclass!=CLCOMMON)
		warn1("%s never used", p->sthead->namep);
	mkftnp(p);
	}

if(p->vextbase)
	mkftname(p->vextbase, p->sthead->namep);

for(i=0; i<NFTNTYPES ; ++i)
	if(p->vbase[i] != 0)
	if(p!=procname && p->vextbase!=0)
		{
		s = ftnames[p->vextbase];
		t = ftnames[p->vbase[i]];
		while(*t++ = *s++ )
			;
		}
	else if(p->sthead)
		mkftname(p->vbase[i], p->sthead->namep);
	else
		mkftname(p->vbase[i], CHNULL);
p->vnamedone = 1;
}





mkftname(n,s)
int n;
char *s;
{
int i, j;
register int k;
char fn[7];
register char *c1, *c2;

if(ftnames[n][0] != '\0')  return;

if(s==0 || *s=='\0')
	s = "temp";
else if(*s == '_')
	++s;
k = strlen(s);

for(i=0; i<k && i<(XL/2) ; ++i)
	fn[i] = s[i];
if(k > XL)
	{
	s += (k-XL);
	k = XL;
	}

for( ; i<k ; ++i)
	fn[i] = s[i];
fn[i] = '\0';

if( lookftn(fn) )
	{
	if(k < XL)
		++k;
	fn[k] = '\0';
	c1 = fn + k-1;
	for(*c1 = '1' ; *c1 <= '9' ; *c1 += 1)
		if(lookftn(fn) == 0)
			goto nameok;

	if(k < XL)
		++k;
	fn[k] = '\0';
	c1 = fn + k-2;
	c2 = c1 + 1;

	for(*c1 = '1' ; *c1 <= '9' ; *c1 += 1)
		for(*c2 = '0' ; *c2 <= '9' ; *c2 += 1)
			if(lookftn(fn) == 0)
				goto nameok;
	fatal1("mkftname: cannot generate fortran name for %s", s);
	}

nameok:
for(j=0; j<=k ; ++j)
	ftnames[n][j] = fn[j];
}



nxtftn()
{
if( ++nftnames < MAXFTNAMES)
	{
	ftnames[nftnames][0] = '\0';
	return(nftnames);
	}

fatal("too many Fortran names generated");
/* NOTREACHED */
}



lookftn(s)
char *s;
{
register int i;

for(i=1 ; i<=nftnames ; ++i)
	if(equals(ftnames[i],s))  return(i);
return(0);
}



ptr mkftnblock(type, name)
int type;
char *name;
{
register struct varblock *p;
register int k;

p = allexpblock();
p->tag = TFTNBLOCK;
p->vtype = type;
p->vdcldone = 1;

if( (k = lookftn(name)) == 0)
	{
	k = nxtftn();
	strcpy(ftnames[k], name);
	}
p->vbase[ eflftn[type] ] = k;
p->vextbase = k;
return(p);
}
