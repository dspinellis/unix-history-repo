static	char *sccsid = "@(#)gcos.c	4.1 (Berkeley) 81/02/28";
/* GCOS DEPENDENT PROCEDURES */


/* DEFAULT RULES FOR GCOS */

char *builtin[]
	{
	".SUFFIXES : .d .c .y .lib",
	".d.c:",
	"\t./dtgen $<",
	".y.c:",
	"\t./yacc $<",
	"\tcopy y.tab.c; /$@",
	".y.lib:",
	"\t./yacc $<",
	"\t./cc y.tab.c r=$@",
	".c.lib:",
	"\t./cc $< r=$@",
	0 };

# define MAXCSIZE 500
# define YZERO 60

int gtcalled 0;

/* all kinds of static declarations that must be used.. */

static double day { 64*1000*60*60*24 };  /* length of day in clock ticks */

struct { int lhs:18, rhs:18; };
struct catb {
	int words[6],
	name1, name2,
	passw1, passw2,
	word10, word11,
	datcreat, datmod,  datused,
	stuff[6],
	jjjj:18, tused:18;
	};
struct { int :3, slot:18; };  /* slot where time from cat. block fits */

struct catdesc {
	int cat1, cat2, cpass1, cpass2,
	    file1, file2, filep1, filep2,
	    endmark; };

extern int _q_reg, _a_reg;


# define A10(x,y) 10*x + y

/*	interpret the mm/dd/yy format */

struct d9 { int	:5, m1:4, :5, m2:4, :9,
		:5, d1:4, :5, d2:4, :9,
		:5, y1:4, :5, y2:4  ;};

struct d6 { int	:2, m61:4, :2, m62:4,
		:2, d61:4, :2, d62:4,
		:2, y61:4, :2, y62:4; };

static day6( d6word ){ /* return the day number of a word in bci format */
	int m, y, d;

	y = A10( d6word.y61, d6word.y62 );
	m = A10( d6word.m61, d6word.m62 );
	d = A10( d6word.d61, d6word.d62 );

	return( d + 31*( m + 12*(y-YZERO) ) );
	}

static day9( p ) register int *p; {

	int m, y, d;

	y = A10( p->y1, p->y2 );
	m = A10( p->m1, p->m2 );
	d = A10( p->d1, p->d2 );

	return( d + 31*( m + 12*(y-YZERO) ) );
	}


static int dfold( dayno, timeno ){
	int kk;
	kk = ( day*dayno + timeno) / 32768.;
	}

int prestime(){
	int date[2];
	drldrl( 021, date );
	return( dfold( day9(date), _q_reg ) );
	}



# define DODRL ar[0] = status; ar[1] = &b.cat1; drldrl(30,sp1,sp2); p=ar[0]<<18;

static struct { int fn1, fn2;  int ftm; } fbb[MAXCSIZE];
static int catsiz;

getcat() {

	register i, *p, j;
	int asname[4];
	struct catdesc b;
	int sp1, sp2, temp;
	int ar[2], status[2];
	int filbuf[380];

	gtcalled = 1;

	sp1 = ar;
	sp1 =>> 18;
	sp2 = filbuf;
	sp2 =>>18;
	sp2.lhs = 19;

	b.cat1 = b.cat2 = b.file1 = -1;
	b.cpass1 = b.cpass2 = 0202020202020;

	DODRL
	sp2.lhs++;
	for( i=0; p!=0 && i<MAXCSIZE; ++i ){

		fbb[i].fn1 = b.file1 = p->name1;
		fbb[i].fn2 = b.file2 = p->name2;
		b.filep1 = p->passw1;
		b.filep2 = p->passw2;
		b.endmark = -1;
		temp = 0;
		temp.slot = p->tused;
		fbb[i].ftm = dfold( day6(p->datmod), temp );
		DODRL
		}
	catsiz = i;
	}

 exists( cp ) char *cp; {
	char *s, name[13];
	int i, *p, bcd[2];

/*
   cheat about names with slashes -- try opening;
   if it is openable, it exists, and assume it was made
   at t=1 (long time ago); otherwise, assume it
   does not exist
*/

for(s=cp ; *s ; ++s)
	if(*s == '/')
		if(i = copen(cp,'r') < 0)
			return(0);
		else	{
			cclose(i);
			return(1);
			}

if(gtcalled == 0)  getcat();

	p = name;
	for( i=0; *cp; ++i ) name[i] = *cp++;
	while( i<12 ) name[i++] = ' ';
	f9to6( *p, bcd[0], 12 );
	for ( i=0; i<catsiz; ++i ){
		if( fbb[i].fn1 == bcd[0] && fbb[i].fn2 == bcd[1] )
			return( fbb[i].ftm );
		}
	return( 0 );
	}


#include "defs"

static char n13[13];
static char *n13end &n13[12];



struct depblock *srchdir(pat, mkchain, nextdbl)

char *pat; /* pattern to be matched in directory */
int mkchain;  /* nonzero if results to be remembered */
struct depblock *nextdbl;  /* final value for chain */
{
int dirf;
int i, nread;
char *dirname, *dirpref, *endir, *filepat, *p, temp[100];
char fullname[100], *p1, *p2, *copys();
struct nameblock *q;
struct depblock *thisdbl;
struct pattern *patp;
int *intp1, *intp2;

if(gtcalled == 0)  getcat();
thisdbl=0;

if(mkchain == 0)
	for(patp=firstpat ; patp!=0 ; patp = patp->nxtpattern)
		if(! unequal(pat, patp->patval)) return(0);

patp = ALLOC(pattern);
patp->nxtpattern = firstpat;
firstpat = patp;
patp->patval = copys(pat);

endir = 0;

for(p=pat; *p!='\0'; ++p)
	if(*p=='/') endir = p;

if(endir==0)
	{
	dirname = "";
	dirpref = "";
	filepat = pat;
	}
else	{
fatal("File name has an embedded slash");
	dirname = pat;
	*endir = '\0';
	dirpref = concat(dirname, "/", temp);
	filepat = endir+1;
	}

for(i=0;i<catsiz;++i)
	{
	intp1 = &fbb[i].fn1;
	intp2 = n13;
	f6to9(*intp1, *intp2, 12);
	for(p1=n13; p1<n13end && *p1!=' ' ; ++p1) 
		if('A'<=*p1 && *p1<='Z') *p1 =+ ('a'-'A');
		*p1 = '\0';

	if( amatch(n13,filepat) )
		{
		concat(dirpref,n13,fullname);
		if( (q=srchname(fullname)) ==0)
			q = makename(copys(fullname));
		if(mkchain)
			{
			thisdbl = ALLOC(depblock);
			thisdbl->nextp = nextdbl;
			thisdbl->depname = q;
			nextdbl = thisdbl;
			}
		}
	}

if(endir != 0)  *endir = '/';

return(thisdbl);
}

/* stolen from glob through find */

amatch(s, p)
char *s, *p;
{
	register int cc, scc, k;
	int c, lc;

	scc = *s;
	lc = 077777;
	switch (c = *p) {

	case '[':
		k = 0;
		while (cc = *++p) {
			switch (cc) {

			case ']':
				if (k)
					return(amatch(++s, ++p));
				else
					return(0);

			case '-':
				k =| lc <= scc & scc <= (cc=p[1]);
			}
			if (scc==(lc=cc)) k++;
		}
		return(0);

	case '?':
	caseq:
		if(scc) return(amatch(++s, ++p));
		return(0);
	case '*':
		return(umatch(s, ++p));
	case 0:
		return(!scc);
	}
	if (c==scc) goto caseq;
	return(0);
}

umatch(s, p)
char *s, *p;
{
	if(*p==0) return(1);
	while(*s)
		if (amatch(s++,p)) return(1);
	return(0);
}



dosys(comstring,nohalt)
char *comstring;
int nohalt;
{
char *p;

for(p=comstring ; *p!='\0' ; ++p);
if( p-comstring > 80)
	fatal("Command string longer than 80 characters");

system(comstring);

return(0);
}


touch(s)
char *s;
{
fprintf(stderr, "touch not yet implemented on GCOS\n");
cexit(2);
}
