
#ifndef lint
static char sccsid[] = "@(#)lpass2.c	1.1	(Berkeley)	%G%";
#endif lint

# include "manifest"
# include "lmanifest"

# define USED 01
# define VUSED 02
# define EUSED 04
# define RVAL 010
# define VARARGS 0100

# define NSZ 2048
# define TYSZ 3500
# define FSZ 250
# define NTY 50

typedef struct sty STYPE;
struct sty { ATYPE t; STYPE *next; };

typedef struct sym {
#ifndef FLEXNAMES
	char name[LCHNM];
#else
	char *name;
#endif
	short nargs;
	int decflag;
	int fline;
	STYPE symty;
	int fno;
	int use;
	} STAB;

STAB stab[NSZ];
STAB *find();

STYPE tary[TYSZ];
STYPE *tget();

#ifndef FLEXNAMES
char fnm[FSZ][LFNM];
#else
char *fnm[FSZ];
#endif

#ifdef FLEXNAMES
char *getstr();
#endif

int tfree;  /* used to allocate types */
int ffree;  /* used to save filenames */

struct ty atyp[NTY];
	/* r is where all the input ends up */
union rec r;

int hflag = 0;
int pflag = 0;
int xflag = 0;
int uflag = 1;
int ddddd = 0;

int cfno;  /* current file number */

main( argc, argv ) char *argv[]; {
	register char *p;

	/* first argument is intermediate file */
	/* second argument is - options */

	for( ; argc>2 && argv[argc-1][0] == '-' ; --argc ){
		for( p=argv[argc-1]; *p; ++p ){
			switch( *p ){

			case 'h':
				hflag = 1;
				break;

			case 'p':
				pflag = 1;
				break;

			case 'x':
				xflag = 1;
				break;

			case 'X':
				ddddd = 1;
				break;

			case 'u':
				uflag = 0;
				break;

				}
			}
		}

	if( argc < 2 || !freopen( argv[1], "r", stdin ) ){
		error( "cannot open intermediate file" );
		exit( 1 );
		}

	mloop( LDI|LIB );
	rewind( stdin );
	mloop( LDC|LDX );
	rewind( stdin );
	mloop( LRV|LUV|LUE|LUM );
	cleanup();
	return(0);
	}

mloop( m ){
	/* do the main loop */
	register STAB *q;

	while( lread(m) ){
		q = find();
		if( q->decflag ) chkcompat(q);
		else setuse(q);
		}
	}

lread(m){ /* read a line into r.l */

	register n;

	for(;;) {
		if( fread( (char *)&r, sizeof(r), 1, stdin ) <= 0 ) return(0);
		if( r.l.decflag & LFN ){
			/* new filename */
#ifdef FLEXNAMES
			r.f.fn = getstr();
#endif
			setfno( r.f.fn );
			continue;
			}
#ifdef FLEXNAMES
		r.l.name = getstr();
#endif

		n = r.l.nargs;
		if( n<0 ) n = -n;
		if( n ){
			if( n>=NTY ) error( "more than %d args?", n );
			fread( (char *)atyp, sizeof(ATYPE), n, stdin );
			}
		if( ( r.l.decflag & m ) ) return( 1 );
		}
	}

setfno( s ) char *s; {
	/* look up current file names */
	/* first, strip backwards to the beginning or to the first / */
	int i;

	/* now look up s */
	for( i=0; i<ffree; ++i ){
#ifndef FLEXNAMES
		if( !strncmp( s, fnm[i], LFNM ) ){
#else
		if (fnm[i] == s){
#endif
			cfno = i;
			return;
			}
		}
	/* make a new entry */
	if( ffree >= FSZ ) error( "more than %d files", FSZ );
#ifndef FLEXNAMES
	strncpy( fnm[ffree], s, LFNM );
#else
	fnm[ffree] = s;
#endif
	cfno = ffree++;
	}

/* VARARGS */
error( s, a ) char *s; {

#ifndef FLEXNAMES
	fprintf( stderr, "pass 2 error:(file %.*s) ", LFNM, fnm[cfno] );
#else
	fprintf( stderr, "pass 2 error:(file %s) ", fnm[cfno] );
#endif
	fprintf( stderr, s, a );
	fprintf( stderr, "\n" );
	exit(1);
	}

STAB *
find(){
	/* for this to work, NSZ should be a power of 2 */
	register h=0;
#ifndef FLEXNAMES
	{	register char *p, *q;
		for( h=0,p=r.l.name,q=p+LCHNM; *p&&p<q; ++p) {
			h = (h<<1)+ *p;
			if( h>=NSZ ){
				h = (h+1)&(NSZ-1);
				}
			}
		}
#else
		h = ((int)r.l.name)%NSZ;
#endif
	{	register STAB *p, *q;
		for( p=q= &stab[h]; q->decflag; ){
			/* this call to strncmp should be taken out... */
#ifndef FLEXNAMES
			if( !strncmp( r.l.name, q->name, LCHNM)) return(q);
#else
			if (r.l.name == q->name) return (q);
#endif
			if( ++q >= &stab[NSZ] ) q = stab;
			if( q == p ) error( "too many names defined" );
			}
#ifndef FLEXNAMES
		strncpy( q->name, r.l.name, LCHNM );
#else
		q->name = r.l.name;
#endif
		return( q );
		}
	}

STYPE *
tget(){
	if( tfree >= TYSZ ){
		error( "too many types needed" );
		}
	return( &tary[tfree++] );
	}

chkcompat(q) STAB *q; {
	/* are the types, etc. in r.l and q compatible */
	register int i;
	STYPE *qq;

	setuse(q);

	/* argument check */

	if( q->decflag & (LDI|LIB|LUV|LUE) ){
		if( r.l.decflag & (LUV|LIB|LUE) ){
			if( q->nargs != r.l.nargs ){
				if( !(q->use&VARARGS) ){
#ifndef FLEXNAMES
					printf( "%.8s: variable # of args.", q->name );
#else
					printf( "%s: variable # of args.", q->name );
#endif
					viceversa(q);
					}
				if( r.l.nargs > q->nargs ) r.l.nargs = q->nargs;
				if( !(q->decflag & (LDI|LIB) ) ) {
					q->nargs = r.l.nargs;
					q->use |= VARARGS;
					}
				}
			for( i=0,qq=q->symty.next; i<r.l.nargs; ++i,qq=qq->next){
				if( chktype( &qq->t, &atyp[i] ) ){
#ifndef FLEXNAMES
					printf( "%.8s, arg. %d used inconsistently",
#else
					printf( "%s, arg. %d used inconsistently",
#endif
						q->name, i+1 );
					viceversa(q);
					}
				}
			}
		}

	if( (q->decflag&(LDI|LIB|LUV)) && r.l.decflag==LUV ){
		if( chktype( &r.l.type, &q->symty.t ) ){
#ifndef FLEXNAMES
			printf( "%.8s value used inconsistently", q->name );
#else
			printf( "%s value used inconsistently", q->name );
#endif
			viceversa(q);
			}
		}

	/* check for multiple declaration */

	if( (q->decflag&LDI) && (r.l.decflag&(LDI|LIB)) ){
#ifndef FLEXNAMES
		printf( "%.8s multiply declared", q->name );
#else
		printf( "%s multiply declared", q->name );
#endif
		viceversa(q);
		}

	/* do a bit of checking of definitions and uses... */

	if( (q->decflag & (LDI|LIB|LDX|LDC|LUM)) && (r.l.decflag & (LDX|LDC|LUM)) && q->symty.t.aty != r.l.type.aty ){
#ifndef FLEXNAMES
		printf( "%.8s value declared inconsistently", q->name );
#else
		printf( "%s value declared inconsistently", q->name );
#endif
		viceversa(q);
		}

	/* better not call functions which are declared to be structure or union returning */

	if( (q->decflag & (LDI|LIB|LDX|LDC)) && (r.l.decflag & LUE) && q->symty.t.aty != r.l.type.aty ){
		/* only matters if the function returns union or structure */
		TWORD ty;
		ty = q->symty.t.aty;
		if( ISFTN(ty) && ((ty = DECREF(ty))==STRTY || ty==UNIONTY ) ){
#ifndef FLEXNAMES
			printf( "%.8s function value type must be declared before use", q->name );
#else
			printf( "%s function value type must be declared before use", q->name );
#endif
			viceversa(q);
			}
		}

	if( pflag && q->decflag==LDX && r.l.decflag == LUM && !ISFTN(q->symty.t.aty) ){
		/* make the external declaration go away */
		/* in effect, it was used without being defined */
		}
	}

viceversa(q) STAB *q; {
	/* print out file comparison */
#ifndef FLEXNAMES
	printf( "	%.*s(%d)  ::  %.*s(%d)\n",
		LFNM, fnm[q->fno], q->fline,
		LFNM, fnm[cfno], r.l.fline );
#else
	printf( "	%s(%d)  ::  %s(%d)\n",
		fnm[q->fno], q->fline,
		fnm[cfno], r.l.fline );
#endif
	}

	/* messages for defintion/use */
char *
mess[2][2] ={
	"",
#ifndef FLEXNAMES
	"%.8s used( %.*s(%d) ), but not defined\n",
	"%.8s defined( %.*s(%d) ), but never used\n",
	"%.8s declared( %.*s(%d) ), but never used or defined\n"
#else
	"%s used( %s(%d) ), but not defined\n",
	"%s defined( %s(%d) ), but never used\n",
	"%s declared( %s(%d) ), but never used or defined\n"
#endif
	};

lastone(q) STAB *q; {

	register nu, nd, uses;

	if( ddddd ) pst(q);

	nu = nd = 0;
	uses = q->use;

	if( !(uses&USED) && q->decflag != LIB ) {
#ifndef FLEXNAMES
		if( strncmp(q->name,"main",7) )
#else
		if (strcmp(q->name, "main"))
#endif
			nu = 1;
		}

	if( !ISFTN(q->symty.t.aty) ){
		switch( q->decflag ){

		case LIB:
			nu = nd = 0;  /* don't complain about uses on libraries */
			break;
		case LDX:
			if( !xflag ) break;
		case LUV:
		case LUE:
/* 01/04/80 */	case LUV | LUE:
		case LUM:
			nd = 1;
			}
		}
	if( uflag && ( nu || nd ) ) printf( mess[nu][nd],
#ifndef FLEXNAMES
		 q->name, LFNM, fnm[q->fno], q->fline );
#else
		 q->name, fnm[q->fno], q->fline );
#endif

	if( (uses&(RVAL+EUSED)) == (RVAL+EUSED) ){
#ifndef FLEXNAMES
		printf( "%.8s returns value which is %s ignored\n", q->name,
#else
		printf( "%s returns value which is %s ignored\n", q->name,
#endif
			uses&VUSED ? "sometimes" : "always" );
		}

	if( (uses&(RVAL+VUSED)) == (VUSED) && (q->decflag&(LDI|LIB)) ){
#ifndef FLEXNAMES
		printf( "%.8s value is used, but none returned\n", q->name );
#else
		printf( "%s value is used, but none returned\n", q->name );
#endif
		}
	}

cleanup(){ /* call lastone and die gracefully */
	STAB *q;
	for( q=stab; q< &stab[NSZ]; ++q ){
		if( q->decflag ) lastone(q);
		}
	exit(0);
	}

setuse(q) STAB *q; { /* check new type to ensure that it is used */

	if( !q->decflag ){ /* new one */
		q->decflag = r.l.decflag;
		q->symty.t = r.l.type;
		if( r.l.nargs < 0 ){
			q->nargs = -r.l.nargs;
			q->use = VARARGS;
			}
		else {
			q->nargs = r.l.nargs;
			q->use = 0;
			}
		q->fline = r.l.fline;
		q->fno = cfno;
		if( q->nargs ){
			int i;
			STYPE *qq;
			for( i=0,qq= &q->symty; i<q->nargs; ++i,qq=qq->next ){
				qq->next = tget();
				qq->next->t = atyp[i];
				}
			}
		}

	switch( r.l.decflag ){

	case LRV:
		q->use |= RVAL;
		return;
	case LUV:
		q->use |= VUSED+USED;
		return;
	case LUE:
		q->use |= EUSED+USED;
		return;
/* 01/04/80 */	case LUV | LUE:
	case LUM:
		q->use |= USED;
		return;

		}
	}

chktype( pt1, pt2 ) register ATYPE *pt1, *pt2; {
	TWORD t;

	/* check the two type words to see if they are compatible */
	/* for the moment, enums are turned into ints, and should be checked as such */
	if( pt1->aty == ENUMTY ) pt1->aty =  INT;
	if( pt2->aty == ENUMTY ) pt2->aty = INT;

	if( (t=BTYPE(pt1->aty)==STRTY) || t==UNIONTY ){
		return( pt1->aty!=pt2->aty || (
			pt1->extra!=pt2->extra ) );
		}

	if( pt2->extra ){ /* constant passed in */
		if( pt1->aty == UNSIGNED && pt2->aty == INT ) return( 0 );
		else if( pt1->aty == ULONG && pt2->aty == LONG ) return( 0 );
		}
	else if( pt1->extra ){ /* for symmetry */
		if( pt2->aty == UNSIGNED && pt1->aty == INT ) return( 0 );
		else if( pt2->aty == ULONG && pt1->aty == LONG ) return( 0 );
		}

	return( pt1->aty != pt2->aty );
	}

struct tb { int m; char * nm };
ptb( v, tp ) struct tb *tp; {
	/* print a value from the table */
	int flag;
	flag = 0;
	for( ; tp->m; ++tp ){
		if( v&tp->m ){
			if( flag++ ) putchar( '|' );
			printf( "%s", tp->nm );
			}
		}
	}

pst( q ) STAB *q; {
	/* give a debugging output for q */
	static struct tb dfs[] = {
		LDI, "LDI",
		LIB, "LIB",
		LDC, "LDC",
		LDX, "LDX",
		LRV, "LRV",
		LUV, "LUV",
		LUE, "LUE",
		LUM, "LUM",
		0, "" };

	static struct tb us[] = {
		USED, "USED",
		VUSED, "VUSED",
		EUSED, "EUSED",
		RVAL, "RVAL",
		VARARGS, "VARARGS",
		0,	0,
		};

#ifndef FLEXNAMES
	printf( "%.8s (", q->name );
#else
	printf( "%s (", q->name );
#endif
	ptb( q->decflag, dfs );
	printf( "), use= " );
	ptb( q->use, us );
	printf( ", line %d, nargs=%d\n", q->fline, q->nargs );
	}

#ifdef FLEXNAMES
char *
getstr()
{
	char buf[BUFSIZ];
	register char *cp = buf;
	register int c;

	if (feof(stdin) || ferror(stdin))
		return("");
	while ((c = getchar()) > 0)
		*cp++ = c;
	if (c < 0) {
		fprintf(stderr, "pass 2 error: intermediate file format error (getstr)");
		exit(1);
	}
	*cp++ = 0;
	return (hash(buf));
}

#define	NSAVETAB	4096
char	*savetab;
int	saveleft;

char *
savestr(cp)
	register char *cp;
{
	register int len;

	len = strlen(cp) + 1;
	if (len > saveleft) {
		saveleft = NSAVETAB;
		if (len > saveleft)
			saveleft = len;
		savetab = (char *)malloc(saveleft);
		if (savetab == 0) {
			fprintf(stderr, "pass 2 error: ran out of memory (savestr)");
			exit(1);
		}
	}
	strncpy(savetab, cp, len);
	cp = savetab;
	savetab += len;
	saveleft -= len;
	return (cp);
}

/*
 * The definition for the segmented hash tables.
 */
#define	MAXHASH	20
#define	HASHINC	1013
struct ht {
	char	**ht_low;
	char	**ht_high;
	int	ht_used;
} htab[MAXHASH];

char *
hash(s)
	char *s;
{
	register char **h;
	register i;
	register char *cp;
	struct ht *htp;
	int sh;

	/*
	 * The hash function is a modular hash of
	 * the sum of the characters with the sum
	 * doubled before each successive character
	 * is added.
	 */
	cp = s;
	i = 0;
	while (*cp)
		i = i*2 + *cp++;
	sh = (i&077777) % HASHINC;
	cp = s;
	/*
	 * There are as many as MAXHASH active
	 * hash tables at any given point in time.
	 * The search starts with the first table
	 * and continues through the active tables
	 * as necessary.
	 */
	for (htp = htab; htp < &htab[MAXHASH]; htp++) {
		if (htp->ht_low == 0) {
			register char **hp =
			    (char **) calloc(sizeof (char **), HASHINC);
			if (hp == 0) {
				fprintf(stderr, "pass 2 error: ran out of memory (hash)");
				exit(1);
			}
			htp->ht_low = hp;
			htp->ht_high = htp->ht_low + HASHINC;
		}
		h = htp->ht_low + sh;
		/*
		 * quadratic rehash increment
		 * starts at 1 and incremented
		 * by two each rehash.
		 */
		i = 1;
		do {
			if (*h == 0) {
				if (htp->ht_used > (HASHINC * 3)/4)
					break;
				htp->ht_used++;
				*h = savestr(cp);
				return (*h);
			}
			if (**h == *cp && strcmp(*h, cp) == 0)
				return (*h);
			h += i;
			i += 2;
			if (h >= htp->ht_high)
				h -= HASHINC;
		} while (i < HASHINC);
	}
	fprintf(stderr, "pass 2 error: ran out of hash tables");
	exit(1);
}
char	*tstrbuf[1];
#endif
