    /* sccsid:  @(#)gprof.h	1.7 (Berkeley) %G% */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <a.out.h>
#include <pagsiz.h>
#include "monitor.h"

    /*
     *	ticks per second
     */
#define	HZ	60

typedef	short UNIT;		/* unit of profiling */
char	*a_outname;
#define	A_OUTNAME		"a.out"

char	*gmonname;
#define	GMONNAME		"gmon.out"

    /*
     *	a constructed arc,
     *	    with pointers to the namelist entry of the parent and the child,
     *	    a count of how many times this arc was traversed,
     *	    and pointers to the next parent of this child and
     *		the next child of this parent.
     */
struct arcstruct {
    struct nl		*arc_parentp;	/* pointer to parent's nl entry */
    struct nl		*arc_childp;	/* pointer to child's nl entry */
    long		arc_count;	/* how calls from parent to child */
    double		arc_time;	/* time inherited along arc */
    double		arc_childtime;	/* childtime inherited along arc */
    struct arcstruct	*arc_parentlist; /* parents-of-this-child list */
    struct arcstruct	*arc_childlist;	/* children-of-this-parent list */
};
typedef struct arcstruct	arctype;

    /*
     *	a raw arc,
     *	    with pointers to the calling site and the called site
     *	    and a count.
     */
struct rawarc {
    unsigned long	raw_frompc;
    unsigned long	raw_selfpc;
    long		raw_count;
};
/*
 * The symbol table;
 * for each external in the specified file we gather
 * its address, the number of calls and compute its share of cpu time.
 */
struct nl {
	char		*name;		/* the name */
	unsigned long	value;		/* the pc entry point */
	double		time;		/* ticks in this routine */
	double		childtime;	/* cumulative ticks in children */
	long		ncall;		/* how many times called */
	long		selfcalls;	/* how many calls to self */
	int		index;		/* index in the graph list */
	int		toporder;	/* graph call chain top-sort order */
	int		cycleno;	/* internal number of cycle on */
	struct nl	*cyclehead;	/* pointer to head of cycle */
	struct nl	*cnext;		/* pointer to next member of cycle */
	arctype		*parents;	/* list of caller arcs */
	arctype		*children;	/* list of callee arcs */
};
typedef struct nl	nltype;

nltype	*nl;			/* the whole namelist */
nltype	*npe;			/* the virtual end of the namelist */
int		nname;			/* the number of function names */

    /*
     *	flag which marks a nl entry as topologically ``busy''
     */
#define	DFN_BUSY	-1

    /* 
     *	the number of cycles is estimated as this fraction of nnames
     *	ncycles, the number of allocated cycle namelist entries,
     *	not to be confused with cyclemax, the number of discovered cycles.
     */
#define	CYCLEFRACTION	( 0.10 )
int	ncycles;		/* maximum allocated cycle headers */
int	cyclemax;		/* number of cycles discovered */

/*
 * The header on the gmon.out file.
 * gmon.out consists of one of these headers,
 * and then an array of ncnt samples
 * representing the discretized program counter values.
 *	this should be a struct phdr, but since everything is done
 *	as UNITs, this is in UNITs too.
 */
struct hdr {
	UNIT	*lowpc;
	UNIT	*highpc;
	int	ncnt;
};

struct hdr	h;

int	debug;

/*
 * Each discretized pc sample has
 * a count of the number of samples in its range
 */
unsigned UNIT	*samples;

unsigned long	s_lowpc;	/* lowpc from the profile file */
unsigned long	s_highpc;	/* highpc from the profile file */
unsigned lowpc, highpc;		/* range profiled, in UNIT's */
unsigned sampbytes;		/* number of bytes of samples */
int	nsamples;		/* number of samples */
double	actime;			/* accumulated time thus far for putprofline */
double	totime;			/* total time for all routines */
double	scale;			/* scale factor converting samples to pc
				   values: each sample covers scale bytes */
char	*strtab;		/* string table in core */
off_t	ssiz;			/* size of the string table */
struct	exec xbuf;		/* exec header of a.out */
unsigned char	*textspace;		/* text space of a.out in core */

    /*
     *	option flags, from a to z.
     */
int	aflag;				/* static functions, too */
int	bflag;				/* blurbs, too */
int	cflag;				/* discovered call graph, too */
int	zflag;				/* zero time/called functions, too */

    /*
     * booleans
     */
typedef int	bool;
#define	FALSE	0
#define	TRUE	1

    /*
     *	opcode of the `calls' instruction
     */
#define	CALLS	0xfb

    /*
     *	register for pc relative addressing
     */
#define	PC	0xf

enum opermodes {
    literal, indexed, reg, regdef, autodec, autoinc, autoincdef, 
    bytedisp, bytedispdef, worddisp, worddispdef, longdisp, longdispdef,
    immediate, absolute, byterel, bytereldef, wordrel, wordreldef,
    longrel, longreldef
};
typedef enum opermodes	operandenum;

struct modebyte {
    unsigned int	regfield:4;
    unsigned int	modefield:4;
};

    /*
     *	function declarations
     */
		addarc();
int		arccmp();
arctype		*arclookup();
		asgnsamples();
		printblurb();
		cyclelink();
		dfn();
bool		dfn_busy();
		dfn_findcycle();
bool		dfn_numbered();
		dfn_post_visit();
		dfn_pre_visit();
		dfn_self_cycle();
		doarcs();
		done();
		findcalls();
		flatprofheader();
		flatprofline();
bool		funcsymbol();
		getnfile();
		getpfile();
		getstrtab();
		getsymtab();
		gettextspace();
		gprofheader();
		gprofline();
		main();
unsigned long	max();
int		membercmp();
unsigned long	min();
nltype		*nllookup();
FILE		*openpfile();
long		operandlength();
operandenum	operandmode();
char		*operandname();
		printchildren();
		printcycle();
		printgprof();
		printmembers();
		printname();
		printparents();
		printprof();
		readsamples();
unsigned long	reladdr();
		sortchildren();
		sortmembers();
		sortparents();
		tally();
		timecmp();
		topcmp();
int		totalcmp();
		valcmp();

#define	LESSTHAN	-1
#define	EQUALTO		0
#define	GREATERTHAN	1

#define	DFNDEBUG	1
#define	CYCLEDEBUG	2
#define	ARCDEBUG	4
#define	TALLYDEBUG	8
#define	TIMEDEBUG	16
#define	SAMPLEDEBUG	32
#define	AOUTDEBUG	64
#define	CALLSDEBUG	128
#define	LOOKUPDEBUG	256
#define	ANYDEBUG	512
