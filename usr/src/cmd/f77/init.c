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
struct labelblock *thislabel	= NULL;
flag nowarnflag	= NO;
flag ftn66flag	= NO;
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
int proctype	= TYUNKNOWN;
char *procname;
int rtvlabel[NTYPES];
int fudgelabel;
struct addrblock *typeaddr;
struct addrblock *retslot;
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
struct ctlframe ctls[MAXCTL];
struct ctlframe *ctlstack	= ctls-1;
struct ctlframe *lastctl	= ctls+MAXCTL ;

struct nameblock *regnamep[MAXREGVAR];
int highregvar;
int nregvar;

struct extsym extsymtab[MAXEXT];
struct extsym *nextext	= extsymtab;
struct extsym *lastext	= extsymtab+MAXEXT;

struct equivblock eqvclass[MAXEQUIV];
struct hashentry hashtab[MAXHASH];
struct hashentry *lasthash	= hashtab+MAXHASH;

struct labelblock labeltab[MAXSTNO];
struct labelblock *labtabend	= labeltab+MAXSTNO;
struct labelblock *highlabtab =	labeltab;
struct rplblock *rpllist	= NULL;
chainp curdtp	= NULL;
flag toomanyinit;
ftnint curdtelt;
chainp templist	= NULL;
chainp holdtemps	= NULL;
int dorange	= 0;
struct entrypoint *entries	= NULL;

chainp chains	= NULL;

flag inioctl;
struct addrblock *ioblkp;
int iostmt;
int nioctl;
int nequiv	= 0;
int nintnames	= 0;
int nextnames	= 0;

struct literal litpool[MAXLITERALS];
int nliterals;



fileinit()
{
procno = 0;
lastlabno = 10;
lastvarno = 0;
nextext = extsymtab;
nliterals = 0;
nerr = 0;
ndata = 0;
}





procinit()
{
register struct nameblock *p;
register struct dimblock *q;
register struct hashentry *hp;
register struct labelblock *lp;
chainp cp;
int i;

pruse(asmfile, USECONST);
#if FAMILY == SCJ
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
			free(q);
			}
		free(p);
		hp->varp = NULL;
		}
nintnames = 0;
highlabtab = labeltab;

ctlstack = ctls - 1;
for(cp = templist ; cp ; cp = cp->nextp)
	free(cp->datap);
frchain(&templist);
holdtemps = NULL;
dorange = 0;
nregvar = 0;
highregvar = 0;
entries = NULL;
rpllist = NULL;
inioctl = NO;
ioblkp = NULL;
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
	err( sprintf(buff, "characters out of order in implicit:%c-%c", c1, c2) );
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
