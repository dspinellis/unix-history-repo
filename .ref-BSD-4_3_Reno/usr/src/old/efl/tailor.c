#include "defs"


setopt(p,q)
char *p;
char *q;
{
int qval;
qval = (q!=NULL) && ( equals(q, "yes") || equals(q, "on") );

if(equals(p,"debug")) dbgopt = 1;
else if(equals(p,"ndebug")) dbgopt = 0;
else if(equals(p,"pfort")) langopt = 0;
else if(equals(p,"ratfor")) langopt = 1;
else if(equals(p,"efl")) langopt = 2;
else if(equals(p,"dots"))
	dotsopt = qval;
else if(equals(p,"ioerror"))
	{
	if(equals(q,"none"))
		tailor.errmode = IOERRNONE;
	else if(equals(q,"ibm"))
		tailor.errmode = IOERRIBM;
	else if(equals(q,"fortran77"))
		tailor.errmode = IOERRFORT77;
	else execerr("unknown ioerror option %s", q);
	}
else if(equals(p, "system"))
	{
	register struct system *sysp;
	for(sysp = systab ; sysp->sysname ; ++sysp)
		if( equals(q, sysp->sysname) )
			break;

	if(sysp->sysname)
		tailinit(sysp);
	else
		execerr("unknown system %s", q);
	}
else if(equals(p, "continue"))
		tailor.ftncontnu = equals(q, "column1");
else if(equals(p, "procheader"))
	tailor.procheader = (q ? copys(q) : 0);
else if(equals(p, "hollincall"))
	tailor.hollincall = qval;
else if(equals(p, "longcomplextype"))
	{
	tailor.lngcxtype = (q ? copys(q) : CNULL);
	if(qval)
		eflftn[TYLCOMPLEX] = FTNDCOMPLEX;
	}
else if(equals(p, "longcomplexprefix"))
	tailor.lngcxprefix = (q ? copys(q) : CNULL);
else if(equals(p, "fortran77"))
	{
	if(tailor.ftn77 = (q==NULL || qval) )
		tailor.errmode = IOERRFORT77;
	else if(tailor.errmode == IOERRFORT77)
		tailor.errmode = IOERRNONE;
	}

else if( !tailop(p,q) )
	execerr("unknown option %s", p);

if(langopt==2)
	setdot(dotsopt);
else if(langopt==1)
	setdot(1);
}




tailinit(sysp)
register struct system *sysp;
{
register int sysf = sysp->sysno;
tailor.ftncontnu = (sysf==UNIX);
tailor.ftnsys = sysf;
tailor.ftnin = 5;
tailor.ftnout = 6;
tailor.errmode = (sysf==UNIX ? IOERRFORT77 : IOERRIBM);
tailor.charcomp = 2;
tailor.hollincall = YES;
tailor.deltastno = 1;
tailor.dclintrinsics = YES;

tailsize(sysp->chperwd);
tailfmt(sysp->idig, sysp->rdig, sysp->ddig);
}





tailsize(wordsize)
int wordsize;
{
int i;

tailor.ftnchwd = wordsize;
tailor.ftnsize[FTNINT] = wordsize;
tailor.ftnsize[FTNREAL] = wordsize;
tailor.ftnsize[FTNLOG] = wordsize;
tailor.ftnsize[FTNCOMPLEX] = 2*wordsize;
tailor.ftnsize[FTNDOUBLE] = 2*wordsize;
tailor.ftnsize[FTNDCOMPLEX] = 2*wordsize;

for(i = 0 ; i<NFTNTYPES ; ++i)
	tailor.ftnalign[i] = tailor.ftnsize[i];
}




tailfmt(idig, rdig, ddig)
int idig, rdig, ddig;
{
sprintf(msg, "i%d", idig);
tailor.dfltfmt[TYINT] = copys(msg);

sprintf(msg, "e%d.%d", rdig+8, rdig);
tailor.dfltfmt[TYREAL] = copys(msg);

sprintf(msg, "d%d.%d", ddig+8, ddig);
tailor.dfltfmt[TYLREAL] = copys(msg);

sprintf(msg, "1h(,1p%s,2h, ,%s,1h)",
	tailor.dfltfmt[TYREAL], tailor.dfltfmt[TYREAL]);
tailor.dfltfmt[TYCOMPLEX] = copys(msg);

sprintf(msg, "1h(,1p%s,2h, ,%s,1h)",
	tailor.dfltfmt[TYLREAL], tailor.dfltfmt[TYLREAL]);
tailor.dfltfmt[TYLCOMPLEX] = copys(msg);

tailor.dfltfmt[TYLOG] = "l2";
}




tailop(n,v)
char *n, *v;
{
int val;
struct itable { char *optn; int *ioptloc; } *ip;
struct ctable { char *optn; char **coptloc; } *cp;
static struct ctable formats[ ] =  {
	"iformat",	&tailor.dfltfmt[TYINT],
	"rformat",	&tailor.dfltfmt[TYREAL],
	"dformat",	&tailor.dfltfmt[TYLREAL],
	"zformat",	&tailor.dfltfmt[TYCOMPLEX],
	"zdformat",	&tailor.dfltfmt[TYLCOMPLEX],
	"lformat",	&tailor.dfltfmt[TYLOG],
	0, 0  };

static struct itable ints[ ] = {
	"ftnin",	&tailor.ftnin,
	"ftnout",	&tailor.ftnout,
	"charperint",  &tailor.ftnchwd,
	"charcomp",	&tailor.charcomp,
	"deltastno",	&tailor.deltastno,
	"dclintrinsics",	&tailor.dclintrinsics,
	"isize",	&tailor.ftnsize[FTNINT],
	"rsize",	&tailor.ftnsize[FTNREAL],
	"dsize",	&tailor.ftnsize[FTNDOUBLE],
	"lsize",	&tailor.ftnsize[FTNLOG],
	"zsize",	&tailor.ftnsize[FTNCOMPLEX],
	"ialign",	&tailor.ftnalign[FTNINT],
	"ralign",	&tailor.ftnalign[FTNREAL],
	"dalign",	&tailor.ftnalign[FTNDOUBLE],
	"lalign",	&tailor.ftnalign[FTNLOG],
	"zalign",	&tailor.ftnalign[FTNCOMPLEX],
	0, 0 };

for(cp = formats; cp->optn ; ++cp)
	if(equals(n, cp->optn))
		{
		*(cp->coptloc) = copys(v);
		return(1);
		}

for(ip = ints ; ip->optn ; ++ip)
	if(equals(n, ip->optn))
		{
		if( equals(v, "yes") || equals(v, "on") )
			val = 1;
		else if( equals(v, "no") || equals(v, "off") )
			val = 0;
		else	val = convci(v);
		*(ip->ioptloc) = val;
		return(1);
		}

return(0);
}
