#include "defs"


FILEP infile	= { stdin };
FILEP diagfile	= { stderr };

FILEP textfile;
FILEP asmfile;
FILEP initfile;
long int headoffset;

char token[200];
int toklen;
int lineno;
char *infname;
int needkwd;
struct Labelblock *thislabel	= NULL;
flag nowarnflag	= NO;
flag ftn66flag	= NO;
flag no66flag	= NO;
flag noextflag	= NO;
flag profileflag	= NO;
flag optimflag	= NO;
flag shiftcase	= YES;
flag undeftype	= NO;
flag shortsubs	= YES;
flag onetripflag	= NO;
flag checksubs	= NO;
flag debugflag	= NO;
int nerr;
int nwarn;
int ndata;

flag saveall;
flag substars;
int parstate	= OUTSIDE;
flag headerdone	= NO;
int blklevel;
int impltype[26];
int implleng[26];
int implstg[26];

int tyint	= TYLONG ;
int tylogical	= TYLONG;
ftnint typesize[NTYPES]
	= { 1, SZADDR, SZSHORT, SZLONG, SZLONG, 2*SZLONG,
	    2*SZLONG, 4*SZLONG, SZLONG, 1, 1, 1};
int typealign[NTYPES]
	= { 1, ALIADDR, ALISHORT, ALILONG, ALILONG, ALIDOUBLE,
	    ALILONG, ALIDOUBLE, ALILONG, 1, 1, 1};
int procno;
int lwmno;
int proctype	= TYUNKNOWN;
char *procname;
int rtvlabel[NTYPES];
int fudgelabel;
Addrp typeaddr;
Addrp retslot;
int cxslot	= -1;
int chslot	= -1;
int chlgslot	= -1;
int procclass	= CLUNKNOWN;
int nentry;
flag multitype;
ftnint procleng;
int lastlabno	= 10;
int lastvarno;
int lastargslot;
int argloc;
ftnint autoleng;
ftnint bssleng	= 0;
int retlabel;
int ret0label;

int maxctl	= MAXCTL;
struct Ctlframe *ctls;
struct Ctlframe *ctlstack;
struct Ctlframe *lastctl;

Namep regnamep[MAXREGVAR];
int highregvar;
int nregvar;

int maxext	= MAXEXT;
struct Extsym *extsymtab;
struct Extsym *nextext;
struct Extsym *lastext;

int maxequiv	= MAXEQUIV;
struct Equivblock *eqvclass;

int maxhash	= MAXHASH;
struct Hashentry *hashtab;
struct Hashentry *lasthash;

int maxstno	= MAXSTNO;
struct Labelblock *labeltab;
struct Labelblock *labtabend;
struct Labelblock *highlabtab;

int maxdim	= MAXDIM;
struct Rplblock *rpllist	= NULL;
struct Chain *curdtp	= NULL;
flag toomanyinit;
ftnint curdtelt;
chainp templist	= NULL;
chainp holdtemps	= NULL;
int dorange	= 0;
struct Entrypoint *entries	= NULL;

chainp chains	= NULL;

flag inioctl;
Addrp ioblkp;
int iostmt;
int nioctl;
int nequiv	= 0;
int eqvstart	= 0;
int nintnames	= 0;

#ifdef SDB
int dbglabel	= 0;
flag sdbflag	= NO;
#endif

struct Literal litpool[MAXLITERALS];
int nliterals;



fileinit()
{
procno = 0;
lwmno = 0;
lastlabno = 10;
lastvarno = 0;
nliterals = 0;
nerr = 0;
ndata = 0;

ctls = ALLOCN(maxctl, Ctlframe);
extsymtab = ALLOCN(maxext, Extsym);
eqvclass = ALLOCN(maxequiv, Equivblock);
hashtab = ALLOCN(maxhash, Hashentry);
labeltab = ALLOCN(maxstno, Labelblock);

ctlstack = ctls - 1;
lastctl = ctls + maxctl;
nextext = extsymtab;
lastext = extsymtab + maxext;
lasthash = hashtab + maxhash;
labtabend = labeltab + maxstno;
highlabtab = labeltab;
}





procinit()
{
register Namep p;
register struct Dimblock *q;
register struct Hashentry *hp;
register struct Labelblock *lp;
struct Chain *cp;
int i;

pruse(asmfile, USECONST);
#if FAMILY == PCC
	p2pass(USETEXT);
#endif
parstate = OUTSIDE;
headerdone = NO;
blklevel = 1;
saveall = NO;
substars = NO;
nwarn = 0;
thislabel = NULL;
needkwd = 0;

++procno;
proctype = TYUNKNOWN;
procname = "MAIN_    ";
procclass = CLUNKNOWN;
nentry = 0;
multitype = NO;
typeaddr = NULL;
retslot = NULL;
cxslot = -1;
chslot = -1;
chlgslot = -1;
procleng = 0;
blklevel = 1;
lastargslot = 0;
#if TARGET==PDP11
	autoleng = 6;
#else
	autoleng = 0;
#endif

for(lp = labeltab ; lp < labtabend ; ++lp)
	lp->stateno = 0;

for(hp = hashtab ; hp < lasthash ; ++hp)
	if(p = hp->varp)
		{
		frexpr(p->vleng);
		if(q = p->vdim)
			{
			for(i = 0 ; i < q->ndim ; ++i)
				{
				frexpr(q->dims[i].dimsize);
				frexpr(q->dims[i].dimexpr);
				}
			frexpr(q->nelt);
			frexpr(q->baseoffset);
			frexpr(q->basexpr);
			free( (charptr) q);
			}
		if(p->vclass == CLNAMELIST)
			frchain( &(p->varxptr.namelist) );
		free( (charptr) p);
		hp->varp = NULL;
		}
nintnames = 0;
highlabtab = labeltab;

ctlstack = ctls - 1;
for(cp = templist ; cp ; cp = cp->nextp)
	free( (charptr) (cp->datap) );
frchain(&templist);
holdtemps = NULL;
dorange = 0;
nregvar = 0;
highregvar = 0;
entries = NULL;
rpllist = NULL;
inioctl = NO;
ioblkp = NULL;
eqvstart += nequiv;
nequiv = 0;

for(i = 0 ; i<NTYPES ; ++i)
	rtvlabel[i] = 0;
fudgelabel = 0;

if(undeftype)
	setimpl(TYUNKNOWN, (ftnint) 0, 'a', 'z');
else
	{
	setimpl(TYREAL, (ftnint) 0, 'a', 'z');
	setimpl(tyint,  (ftnint) 0, 'i', 'n');
	}
setimpl(-STGBSS, (ftnint) 0, 'a', 'z');	/* set class */
setlog();
}




setimpl(type, length, c1, c2)
int type;
ftnint length;
int c1, c2;
{
int i;
char buff[100];

if(c1==0 || c2==0)
	return;

if(c1 > c2)
	{
	sprintf(buff, "characters out of order in implicit:%c-%c", c1, c2);
	err(buff);
	}
else
	if(type < 0)
		for(i = c1 ; i<=c2 ; ++i)
			implstg[i-'a'] = - type;
	else
		{
		type = lengtype(type, (int) length);
		if(type != TYCHAR)
			length = 0;
		for(i = c1 ; i<=c2 ; ++i)
			{
			impltype[i-'a'] = type;
			implleng[i-'a'] = length;
			}
		}
}
