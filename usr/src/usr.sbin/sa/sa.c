#ifndef lint
static char *sccsid = "@(#)sa.c	4.11 (Berkeley) %G%";
#endif

/*
 *	Extensive modifications to internal data structures
 *	to allow arbitrary number of different commands and users added.
 *	
 *	Also allowed the digit option on the -v flag (interactive
 *	threshold compress) to be a digit string, so one can
 *	set the threshold > 9.
 *
 *	Also added the -f flag, to force no interactive threshold
 *	compression with the -v flag.
 *
 *	Robert Henry
 *	UC Berkeley
 *	31jan81
 */
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/acct.h>
#include <signal.h>
#include <utmp.h>
#include <pwd.h>
#include "pathnames.h"

/* interpret command time accounting */

#define	NC	sizeof(acctbuf.ac_comm)

struct acct acctbuf;
int	lflg;
int	cflg;
int	Dflg;
int	dflg;
int	iflg;
int	jflg;
int	Kflg;
int	kflg;
int	nflg;
int	aflg;
int	rflg;
int	oflg;
int	tflg;
int	vflg;
int	fflg;
int	uflg;
int	thres;
int	sflg;
int	bflg;
int	mflg;

struct	utmp	utmp;
#define	NAMELG	(sizeof(utmp.ut_name)+1)

struct 	Olduser{
	int	Us_cnt;
	double	Us_ctime;
	double	Us_io;
	double	Us_imem;
};
	
struct	user {
	char	name[NC];		/* this is <\001><user id><\000> */
	struct	Olduser	oldu;
	char	us_name[NAMELG];
};
#define	us_cnt		oldu.Us_cnt
#define	us_ctime	oldu.Us_ctime
#define	us_io		oldu.Us_io
#define	us_imem		oldu.Us_imem

/*
 *	We protect ourselves from preposterous user id's by looking
 *	through the passwd file for the highest uid allocated, and
 *	then adding 10 to that.
 *	This prevents the user structure from growing too large.
 */
#define	USERSLOP	10
int	maxuser;		/* highest uid from /etc/passwd, + 10 for slop*/

struct	process {
	char	name[NC];
	int	count;
	double	realt;
	double	cput;
	double	syst;
	double	imem;
	double	io;
};

union	Tab{
	struct	process	p;
	struct	user	u;
};

typedef	union Tab cell;

int	(*cmp)();	/* compares 2 cells; set to appropriate func */
cell	*enter();
struct	user *finduser();
struct	user *wasuser();

/*
 *	Table elements are keyed by the name of the file exec'ed.
 *	Because on large systems, many files can be exec'ed,
 *	a static table size may grow to be too large.
 *
 *	Table elements are allocated in chunks dynamically, linked
 *	together so that they may be retrieved sequentially.
 *
 *	An index into the table structure is provided by hashing through
 *	a seperate hash table.
 *	The hash table is segmented, and dynamically extendable.
 *	Realize that the hash table and accounting information is kept
 *	in different segments!
 *
 *	We have a linked list of hash table segments; within each
 *	segment we use a quadratic rehash that touches no more than 1/2
 *	of the buckets in the hash table when probing.
 *	If the probe does not find the desired symbol, it moves to the
 *	next segment, or allocates a new segment.
 *
 *	Hash table segments are kept on the linked list with the first
 *	segment always first (that will probably contain the
 *	most frequently executed commands) and
 *	the last added segment immediately after the first segment,
 *	to hopefully gain something by locality of reference.
 *
 *	We store the per user information in the same structure as
 *	the per exec'ed file information.  This allows us to use the
 *	same managers for both, as the number of user id's may be very
 *	large.
 *	User information is keyed by the first character in the name
 *	being a '\001', followed by four bytes of (long extended)
 *	user id number, followed by a null byte.
 *	The actual user names are kept in a seperate field of the
 *	user structure, and is filled in upon demand later.
 *	Iteration through all users by low user id to high user id
 *	is done by just probing the table, which is gross.
 */
#define	USERKEY	'\001'
#define	ISPROCESS(tp)	(tp->p.name[0] && (tp->p.name[0] != USERKEY))
#define	ISUSER(tp)	(tp->p.name[0] && (tp->p.name[0] == USERKEY))

#define	TABDALLOP	500
struct 	allocbox{
	struct	allocbox	*nextalloc;
	cell			tabslots[TABDALLOP];
};

struct	allocbox	*allochead;	/*head of chunk list*/
struct	allocbox	*alloctail;	/*tail*/
struct	allocbox	*newbox;	/*for creating a new chunk*/
cell			*nexttab;	/*next table element that is free*/
int			tabsleft;	/*slots left in current chunk*/
int			ntabs;
/*
 *	Iterate through all symbols in the symbol table in declaration
 *	order.
 *	struct	allocbox	*allocwalk;
 *	cell			*sp, *ub;
 *
 *	sp points to the desired item, allocwalk and ub are there
 *	to make the iteration go.
 */

#define DECLITERATE(allocwalk, walkpointer, ubpointer) \
	for(allocwalk = allochead; \
	    allocwalk != 0; \
	    allocwalk = allocwalk->nextalloc) \
		for (walkpointer = &allocwalk->tabslots[0],\
		        ubpointer = &allocwalk->tabslots[TABDALLOP], \
		        ubpointer = ubpointer > ( (cell *)alloctail) \
				 ? nexttab : ubpointer ;\
		     walkpointer < ubpointer; \
		     walkpointer++ )

#define TABCHUNKS(allocwalk, tabptr, size) \
	for (allocwalk = allochead; \
	    allocwalk != 0; \
	    allocwalk = allocwalk->nextalloc) \
	    if ( \
		(tabptr = &allocwalk->tabslots[0]), \
		(size = \
		 (   (&allocwalk->tabslots[TABDALLOP]) \
		   > ((cell *)alloctail) \
		 ) \
		   ? (nexttab - tabptr) : TABDALLOP \
		), \
		1 \
	    )
#define	PROCESSITERATE(allocwalk, walkpointer, ubpointer) \
	DECLITERATE(allocwalk, walkpointer, ubpointer) \
	if (ISPROCESS(walkpointer))

#define	USERITERATE(allocwalk, walkpointer, ubpointer) \
	DECLITERATE(allocwalk, walkpointer, ubpointer) \
	if (ISUSER(walkpointer))
/*
 *	When we have to sort the segmented accounting table, we
 *	create a vector of sorted queues that is merged
 *	to sort the entire accounting table.
 */
struct chunkdesc   {
	cell	*chunk_tp;
	int	chunk_n;
};

/*
 *	Hash table segments and manager
 */
#define	NHASH	1103
struct hashdallop {
	int	h_nused;
	struct	hashdallop	*h_next;
	cell		*h_tab[NHASH];
};
struct	hashdallop	*htab;	/* head of the list */
int	htabinstall;		/* install the symbol */

double	treal;
double	tcpu;
double	tsys;
double	tio;
double	timem;
cell	*junkp;
char	*sname;
double	ncom;
time_t	expand();
char	*getname();

/*
 *	usracct saves records of type Olduser.
 *	There is one record for every possible uid less than
 *	the largest uid seen in the previous usracct or in savacct.
 *	uid's that had no activity correspond to zero filled slots;
 *	thus one can index the file and get the user record out.
 *	It would be better to save only user information for users
 *	that the system knows about to save space, but that is not
 *	upward compatabile with the old system.
 *
 *	In the old version of sa, uid's greater than 999 were not handled
 *	properly; this system will do that.
 */

#ifdef	DEBUG
#define	USRACCT "./usracct"
#define	SAVACCT	"./savacct"
#define	ACCT	"./acct"
#else
#define	USRACCT _PATH_USRACCT
#define	SAVACCT	_PATH_SAVACCT
#define	ACCT	_PATH_ACCT
#endif	DEBUG


char *usracct = USRACCT;
char *savacct = SAVACCT;

int	cellcmp();
cell	*junkp = 0;
/*
 *	The threshold is built up from digits in the argv ;
 *	eg, -v1s0u1
 *	will build a value of thres of 101.
 *
 *	If the threshold is zero after processing argv, it is set to 1
 */
int	thres = 0;	
int	htabinstall = 1;
int	maxuser = -1;
int	(*cmp)();

/* we assume pagesize is at least 1k */
int	pgdiv;
#define	pgtok(x)	((x) / pgdiv)

extern	tcmp(), ncmp(), bcmp(), dcmp(), Dcmp(), kcmp(), Kcmp();
extern	double sum();

main(argc, argv)
	char **argv;
{
	FILE *ff;
	double ft;
	register struct	allocbox *allocwalk;
	register cell *tp, *ub;
	int i, j, size, nchunks, smallest;
	struct chunkdesc *chunkvector;

	pgdiv = getpagesize() / 1024;
	if (pgdiv == 0)
		pgdiv = 1;
	maxuser = USERSLOP + getmaxuid();

	tabinit();
	cmp = tcmp;
	if (argc>1)
	if (argv[1][0]=='-') {
		argv++;
		argc--;
		for(i=1; argv[0][i]; i++)
		switch(argv[0][i]) {

		case 'o':
			oflg++;
			break;

		case 'i':
			iflg++;
			break;

		case 'b':
			bflg++;
			cmp = bcmp;
			break;

		case 'l':
			lflg++;
			break;

		case 'c':
			cflg++;
			break;

		case 'd':
			dflg++;
			cmp = dcmp;
			break;

		case 'D':
			Dflg++;
			cmp = Dcmp;
			break;

		case 'j':
			jflg++;
			break;

		case 'k':
			kflg++;
			cmp = kcmp;
			break;

		case 'K':
			Kflg++;
			cmp = Kcmp;
			break;

		case 'n':
			nflg++;
			cmp = ncmp;
			break;

		case 'a':
			aflg++;
			break;

		case 'r':
			rflg++;
			break;

		case 't':
			tflg++;
			break;

		case 's':
			sflg++;
			aflg++;
			break;

		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			thres = thres * 10 + (argv[0][i]-'0');
			break;

		case 'v':
			vflg++;
			break;

		case 'f':
			fflg++;	/* force v option; no tty interaction */
			break;

		case 'u':
			uflg++;
			break;

		case 'm':
			mflg++;
			break;

		case 'U':
		case 'S':
			if (i != 1 || argv[0][2]) {	/* gross! */
				fprintf(stderr, "-U and -S options must be separate\n");
				exit(1);
			}
			argc++, argv--;			/* backup - yuk */
			goto doUS;

		default:
		    	fprintf(stderr, "Invalid option %c\n", argv[0][1]);
			exit(1);
		}
	}

#define optfile(f) {if (argc < 2) \
			{ fprintf(stderr, "Missing filename\n"); exit(1); } \
			argc--, argv++; f = argv[0]; }

doUS:
	for (argc--, argv++; argc && argv[0][0] == '-'; argc--, argv++) {
		switch(argv[0][1]) {
		    case 'U':
		    	optfile(usracct);
			break;

		    case 'S':
		    	optfile(savacct);
			break;

		    default:
		    	fprintf(stderr, "Invalid option %c\n", argv[0][1]);
			exit(1);
		}
	}

	if (thres == 0)
		thres = 1;
	if (iflg==0)
		init();
	if (argc<1)
		doacct(ACCT);
	else while (argc--)
		doacct(*argv++);
	if (uflg) {
		return;
	}

/*
 * cleanup pass
 * put junk together
 */

	if (vflg)
		strip();
	if(!aflg)
	PROCESSITERATE(allocwalk, tp, ub){
		for(j=0; j<NC; j++)
			if(tp->p.name[j] == '?')
				goto yes;
		if(tp->p.count != 1)
			continue;
	yes:
		if(junkp == 0)
			junkp = enter("***other");
		junkp->p.count += tp->p.count;
		junkp->p.realt += tp->p.realt;
		junkp->p.cput += tp->p.cput;
		junkp->p.syst += tp->p.syst;
		junkp->p.imem += tp->p.imem;
		junkp->p.io += tp->p.io;
		tp->p.name[0] = 0;
	}
	if (sflg) {
		signal(SIGINT, SIG_IGN);
		if ((ff = fopen(usracct, "w")) != NULL) {
			static	struct	user ZeroUser = {0};
			struct 	user	*up;
			int	uid;
			/*
			 *	Write out just enough user slots,
			 *	filling with zero slots for users that
			 *	weren't found.
			 *	The file can be indexed directly by uid
			 *	to get the correct record.
			 */
			for (uid = 0; uid < maxuser; uid++){
				if ( (up = wasuser(uid)) != 0)
					fwrite((char *)&(up->oldu),
						sizeof(struct Olduser),1,ff);
				else
					fwrite((char *)&(ZeroUser.oldu),
						sizeof(struct Olduser),1,ff);
			}
		}
		if ((ff = fopen(savacct, "w")) == NULL) {
			printf("Can't save\n");
			exit(0);
		}
		PROCESSITERATE(allocwalk, tp, ub)
			fwrite((char *)&(tp->p), sizeof(struct process), 1, ff);
		fclose(ff);
		creat(sname, 0644);
		signal(SIGINT, SIG_DFL);
	}
/*
 * sort and print
 */
	if (mflg) {
		printmoney();
		exit(0);
	}
	column(ncom, treal, tcpu, tsys, timem, tio);
	printf("\n");

	/*
	 *	the fragmented table is sorted by sorting each fragment
	 *	and then merging.
	 */
	nchunks = 0;
	TABCHUNKS(allocwalk, tp, size){
		qsort(tp, size, sizeof(cell), cellcmp);
		nchunks ++;
	}
	chunkvector = (struct chunkdesc *)calloc(nchunks,
		sizeof(struct chunkdesc));
	nchunks = 0;
	TABCHUNKS(allocwalk, tp, size){
		chunkvector[nchunks].chunk_tp = tp;
		chunkvector[nchunks].chunk_n = size;
		nchunks++;
	}
	for(; nchunks; ){
		/*
		 *	Find the smallest element at the head of the queues.
		 */
		smallest = 0;
		for (i = 1; i < nchunks; i++){
			if (cellcmp(chunkvector[i].chunk_tp,
				chunkvector[smallest].chunk_tp) < 0)
					smallest = i;
		}
		tp = chunkvector[smallest].chunk_tp++;
		/*
		 *	If this queue is drained, drop the chunk count,
		 *	and readjust the queues.
		 */
		if (--chunkvector[smallest].chunk_n == 0){
			nchunks--;
			for (i = smallest; i < nchunks; i++)
				chunkvector[i] = chunkvector[i+1];
		}
		if (ISPROCESS(tp)){
			ft = tp->p.count;
			column(ft, tp->p.realt, tp->p.cput,
				tp->p.syst, tp->p.imem, tp->p.io);
			printf("   %.14s\n", tp->p.name);
		}
	}	/* iterate to merge the lists */
}

printmoney()
{
	register i;
	register char *cp;
	register	struct user	*up;

	getnames();		/* fetches all of the names! */
	for (i = 0; i < maxuser; i++) {
		if ( (up = wasuser(i)) != 0){
			if (up->us_cnt) {
				if (up->us_name[0])
					printf("%-8s", up->us_name);
				else 
					printf("%-8d", i);
				printf("%7u %9.2fcpu %10.0ftio %12.0fk*sec\n",
					up->us_cnt, up->us_ctime / 60,
					up->us_io,
					up->us_imem / AHZ);
			}
		}
	}
}

column(n, a, b, c, d, e)
	double n, a, b, c, d, e;
{

	printf("%8.0f", n);
	if(cflg) {
		if(n == ncom)
			printf("%9s", ""); else
			printf("%8.2f%%", 100.*n/ncom);
	}
	col(n, a, treal, "re");
	if (oflg)
		col(n, 60*AHZ*(b/(b+c)), tcpu+tsys, "u/s");
	else if(lflg) {
		col(n, b, tcpu, "u");
		col(n, c, tsys, "s");
	} else
		col(n, b+c, tcpu+tsys, "cp");
	if(tflg)
		printf("%8.1fre/cp", a/(b+c));
	if(dflg || !Dflg)
		printf("%10.0favio", e/(n?n:1));
	else
		printf("%10.0ftio", e);
	if (kflg || !Kflg)
		printf("%10.0fk", d/((b+c)!=0.0?(b+c):1.0));
	else
		printf("%10.0fk*sec", d/AHZ);
}

col(n, a, m, cp)
	double n, a, m;
	char *cp;
{

	if(jflg)
		printf("%11.2f%s", a/(n*(double)AHZ), cp); else
		printf("%11.2f%s", a/(60.*(double)AHZ), cp);
	if(cflg) {
		if(a == m)
			printf("%9s", ""); else
			printf("%8.2f%%", 100.*a/m);
	}
}

doacct(f)
char *f;
{
	FILE *ff;
	long x, y, z;
	struct acct fbuf;
	register char *cp;
	register int c;
	register struct	user *up;
	register cell *tp;
#ifdef DEBUG
	int	nrecords = 0;
#endif DEBUG

	if (sflg && sname) {
		printf("Only 1 file with -s\n");
		exit(0);
	}
	if (sflg)
		sname = f;
	if ((ff = fopen(f, "r"))==NULL) {
		printf("Can't open %s\n", f);
		return;
	}
	while (fread((char *)&fbuf, sizeof(fbuf), 1, ff) == 1) {
#ifdef DEBUG
		if (++nrecords % 1000 == 0)
			printf("Input record from %s number %d\n",
				f, nrecords);
#endif DEBUG
		for (cp = fbuf.ac_comm; *cp && cp < &fbuf.ac_comm[NC]; cp++)
			if (!isascii(*cp) || iscntrl(*cp))
				*cp = '?';
		if (cp == fbuf.ac_comm)
			*cp++ = '?';
		if (fbuf.ac_flag&AFORK) {
			if (cp >= &fbuf.ac_comm[NC])
				cp = &fbuf.ac_comm[NC-1];
			*cp++ = '*';
		}
		if (cp < &fbuf.ac_comm[NC])
			*cp = '\0';
		x = expand(fbuf.ac_utime) + expand(fbuf.ac_stime);
		y = pgtok((u_short)fbuf.ac_mem);
		z = expand(fbuf.ac_io) / AHZ;
		if (uflg) {
			printf("%3d %6.2f cpu %8luk mem %6ld io %.*s\n",
			    fbuf.ac_uid, x/(double)AHZ, y, z, NC, fbuf.ac_comm);
			continue;
		}
		up = finduser(fbuf.ac_uid);
		if (up == 0)
			continue;	/* preposterous user id */
		up->us_cnt++;
		up->us_ctime += x/(double)AHZ;
		up->us_imem += x * y;
		up->us_io += z;
		ncom += 1.0;

		tp = enter(fbuf.ac_comm);
		tp->p.imem += x * y;
		timem += x * y;
		tp->p.count++;
		x = expand(fbuf.ac_etime);
		tp->p.realt += x;
		treal += x;
		x = expand(fbuf.ac_utime);
		tp->p.cput += x;
		tcpu += x;
		x = expand(fbuf.ac_stime);
		tp->p.syst += x;
		tsys += x;
		tp->p.io += z;
		tio += z;
	}
	fclose(ff);
}

/*
 *	Generalized cell compare routine, to cast out users
 */
cellcmp(p1, p2)
	cell *p1, *p2;
{
	if (ISPROCESS(p1)){
		if (ISPROCESS(p2))
			return((*cmp)(p1, p2));
		return(-1);
	}
	if (ISPROCESS(p2))
		return(1);
	return(0);
}

ncmp(p1, p2)
	cell *p1, *p2;
{

	if(p1->p.count == p2->p.count)
		return(tcmp(p1, p2));
	if(rflg)
		return(p1->p.count - p2->p.count);
	return(p2->p.count - p1->p.count);
}

bcmp(p1, p2)
	cell *p1, *p2;
{
	double f1, f2;
	double sum();

	f1 = sum(p1)/p1->p.count;
	f2 = sum(p2)/p2->p.count;
	if(f1 < f2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if(f1 > f2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

Kcmp(p1, p2)
	cell *p1, *p2;
{

	if (p1->p.imem < p2->p.imem) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (p1->p.imem > p2->p.imem) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

kcmp(p1, p2)
	cell *p1, *p2;
{
	double a1, a2;

	a1 = p1->p.imem / ((p1->p.cput+p1->p.syst)?(p1->p.cput+p1->p.syst):1);
	a2 = p2->p.imem / ((p2->p.cput+p2->p.syst)?(p2->p.cput+p2->p.syst):1);
	if (a1 < a2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (a1 > a2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

dcmp(p1, p2)
	cell *p1, *p2;
{
	double a1, a2;

	a1 = p1->p.io / (p1->p.count?p1->p.count:1);
	a2 = p2->p.io / (p2->p.count?p2->p.count:1);
	if (a1 < a2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (a1 > a2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

Dcmp(p1, p2)
	cell *p1, *p2;
{

	if (p1->p.io < p2->p.io) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (p1->p.io > p2->p.io) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

tcmp(p1, p2)
	cell *p1, *p2;
{
	extern double sum();
	double f1, f2;

	f1 = sum(p1);
	f2 = sum(p2);
	if(f1 < f2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if(f1 > f2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

double sum(p)
	cell *p;
{

	if(p->p.name[0] == 0)
		return(0.0);
	return( p->p.cput + p->p.syst);
}

init()
{
	struct user userbuf;
	struct process	tbuf;
	register cell *tp;
	register struct user *up;
	int uid;
	FILE *f;

	if ((f = fopen(savacct, "r")) == NULL)
		goto gshm;
	while (fread((char *)&tbuf, sizeof(struct process), 1, f) == 1) {
		tp = enter(tbuf.name);
		ncom += tbuf.count;
		tp->p.count = tbuf.count;
		treal += tbuf.realt;
		tp->p.realt = tbuf.realt;
		tcpu += tbuf.cput;
		tp->p.cput = tbuf.cput;
		tsys += tbuf.syst;
		tp->p.syst = tbuf.syst;
		tio += tbuf.io;
		tp->p.io = tbuf.io;
		timem += tbuf.imem;
		tp->p.imem = tbuf.imem;
	}
	fclose(f);
 gshm:
	if ((f = fopen(usracct, "r")) == NULL)
		return;
	for(uid = 0;
	    fread((char *)&(userbuf.oldu), sizeof(struct Olduser), 1, f) == 1;
	    uid++){
		if (userbuf.us_cnt){
			up = finduser(uid);
			if (up == 0)
				continue;	/* preposterous user id */
			up->oldu = userbuf.oldu;
		}
	}
	fclose(f);
}

strip()
{
	int c;
	register struct allocbox *allocwalk;
	register cell *tp, *ub, *junkp;

	if (fflg)
		printf("Categorizing commands used %d times or fewer as **junk**\n",
			thres);
	junkp = enter("**junk**");
	PROCESSITERATE(allocwalk, tp, ub){
		if (tp->p.name[0] && tp->p.count <= thres) {
			if (!fflg)
				printf("%.14s--", tp->p.name);
			if (fflg || ((c=getchar())=='y')) {
				tp->p.name[0] = '\0';
				junkp->p.count += tp->p.count;
				junkp->p.realt += tp->p.realt;
				junkp->p.cput += tp->p.cput;
				junkp->p.syst += tp->p.syst;
				junkp->p.imem += tp->p.imem;
				junkp->p.io += tp->p.io;
			}
			if (!fflg)
				while (c && c!='\n')
					c = getchar();
		}
	}
}

time_t
expand(t)
	unsigned t;
{
	register time_t nt;

	nt = t&017777;
	t >>= 13;
	while (t!=0) {
		t--;
		nt <<= 3;
	}
	return(nt);
}

static	char UserKey[NAMELG + 2];

char *
makekey(uid)
	int uid;
{
	(void)sprintf(UserKey+1, "%04x", uid);
	UserKey[0] = USERKEY;
	return(UserKey);
}

struct user *
wasuser(uid)
	int uid;
{
	struct user *tp;

	htabinstall = 0;
	tp = finduser(uid);
	htabinstall = 1;
	return(tp);
}

/*
 *	Only call this if you really want to insert it in the table!
 */
struct user *
finduser(uid)
	int uid;
{

	if (uid > maxuser){
		fprintf(stderr, "Preposterous user id, %d: ignored\n", uid);
		return(0);
	}
	return((struct user*)enter(makekey(uid)));
}

/*
 *	Set the names of all users in the password file.
 *	We will later not print those that didn't do anything.
 */
getnames()
{
	register struct user *tp;
	register struct passwd *pw;
	struct passwd *getpwent();

	setpwent();
	while (pw = getpwent()){
		/* use first name in passwd file for duplicate uid's */
		if ((tp = wasuser(pw->pw_uid)) != 0 && !isalpha(tp->us_name[0]))
			strncpy(tp->us_name, pw->pw_name, NAMELG);
	}
	endpwent();
}

int
getmaxuid()
{
	register struct user *tp;
	register struct passwd *pw;
	struct passwd *getpwent();
	int maxuid = -1;

	setpwent();
	while(pw = getpwent()){
		if (pw->pw_uid > maxuid)
			maxuid = pw->pw_uid;
	}
	endpwent();
	return(maxuid);
}

tabinit()
{
	allochead = 0;
	alloctail = 0;
	nexttab = 0;
	tabsleft = 0;
	htab = 0;
	ntabs = 0;
	htaballoc();		/* get the first part of the hash table */
}

#define ALLOCQTY 	sizeof (struct allocbox)
cell *
taballoc()
{

	if (tabsleft == 0){
		newbox = (struct allocbox *)calloc(1, ALLOCQTY);
		tabsleft = TABDALLOP;
		nexttab = &newbox->tabslots[0];
		if (alloctail == 0){
			allochead = alloctail = newbox;
		} else {
			alloctail->nextalloc = newbox;
			alloctail = newbox;
		}
	}
	--tabsleft;
	++ntabs;
#ifdef DEBUG
	if (ntabs % 100 == 0)
		printf("##Accounting table slot # %d\n", ntabs);
#endif DEBUG
	return(nexttab++);
}

htaballoc()
{
	register struct hashdallop *new;
#ifdef DEBUG
	static int ntables = 0;

	printf("%%%New hash table chunk allocated, number %d\n", ++ntables);
#endif DEBUG
	new = (struct hashdallop *)calloc(1, sizeof (struct hashdallop));
	if (htab == 0)
		htab = new;
	else {		/* add AFTER the 1st slot */
		new->h_next = htab->h_next;
		htab->h_next = new;
	}
}

#define 	HASHCLOGGED	(NHASH / 2)
/*
 *	Lookup a symbol passed in as the argument.
 *
 *	We take pains to avoid function calls; this function
 *	is called quite frequently, and the calling overhead
 *	contributes significantly to the overall execution speed of sa.
 */
cell *
enter(name)
	char *name;	
{
	static int initialprobe;
	register cell **hp;
	register char *from, *to;
	register int len, nprobes;
	static struct hashdallop *hdallop, *emptyhd;
	static cell **emptyslot, **hp_ub;

	emptyslot = 0;
	for (nprobes = 0, from = name, len = 0;
	     *from && len < NC;
	     nprobes <<= 2, nprobes += *from++, len++)
		continue;
	nprobes += from[-1] << 5;
	nprobes %= NHASH;
	if (nprobes < 0)
		nprobes += NHASH;

	initialprobe = nprobes;
	for (hdallop = htab; hdallop != 0; hdallop = hdallop->h_next){
		for (hp = &(hdallop->h_tab[initialprobe]),
				nprobes = 1,
				hp_ub = &(hdallop->h_tab[NHASH]);
		     (*hp) && (nprobes < NHASH);
				hp += nprobes,
				hp -= (hp >= hp_ub) ? NHASH:0,
				nprobes += 2)
		{
			from = name;
			to = (*hp)->p.name;

			for (len = 0; (len<NC) && *from; len++)
				if (*from++ != *to++)
					goto nextprobe;
			if (len >= NC)		/*both are maximal length*/
				return(*hp);
			if (*to == 0)		/*assert *from == 0*/
				return(*hp);
	nextprobe: ;
		}
		if (*hp == 0 && emptyslot == 0 &&
		    hdallop->h_nused < HASHCLOGGED) {
			emptyslot = hp;
			emptyhd = hdallop;
		}
	}
	if (emptyslot == 0) {
		htaballoc();
		hdallop = htab->h_next;		/* aren't we smart! */
		hp = &hdallop->h_tab[initialprobe];
	} else {
		hdallop = emptyhd;
		hp = emptyslot;
	}
	if (htabinstall){
		*hp = taballoc();
		hdallop->h_nused++;
		for(len = 0, from = name, to = (*hp)->p.name; (len<NC); len++)
			if ((*to++ = *from++) == '\0')
				break;
		return(*hp);
	}
	return(0);
}
