/*	SC	A Spreadsheet Calculator
 *		Expression interpreter and assorted support routines.
 *
 *		original by James Gosling, September 1982
 *		modified by Mark Weiser and Bruce Israel, 
 *			University of Maryland
 *
 *              More mods Robert Bond, 12/86
 *		More mods by Alan Silverstein, 3-4/88, see list of changes.
 *		$Revision: 6.8 $
 */

#define DEBUGDTS 1		/* REMOVE ME */
/* #define EXPRTREE	/* expr. dependency tree stuff, not ready yet */

#ifdef aiws
#undef _C_func			/* Fixes for undefined symbols on AIX */
#endif

#ifdef IEEE_MATH
#include <ieeefp.h>
#endif /* IEEE_MATH */

#include <math.h>
#include <signal.h>
#include <setjmp.h>
#include <stdio.h>

extern int errno;		/* set by math functions */
#ifdef BSD42
#include <strings.h>
#include <sys/time.h>
#ifndef strchr
#define strchr index
#endif
#else
#include <time.h>
#ifndef SYSIII
#include <string.h>
#endif
#endif

#include <curses.h>
#include "sc.h"

#if defined(BSD42) || defined(BSD43)
char *re_comp();
#endif
#if defined(SYSV2) || defined(SYSV3)
char *regcmp();
char *regex();
#endif

#ifdef SIGVOID
    void quit();
#else
    int quit();
#endif

/* Use this structure to save the the last 'g' command */

struct go_save {
	int g_type;
	double g_n;
	char *g_s;
	int  g_row;
	int  g_col;
} gs;

/* g_type can be: */

#define G_NONE 0			/* Starting value - must be 0*/
#define G_NUM 1
#define G_STR 2
#define G_CELL 3

#define ISVALID(r,c)	((r)>=0 && (r)<maxrows && (c)>=0 && (c)<maxcols)

extern FILE *popen();

jmp_buf fpe_save;
int	exprerr;	/* Set by eval() and seval() if expression errors */
double  prescale = 1.0;	/* Prescale for constants in let() */
int	extfunc  = 0;	/* Enable/disable external functions */
int     loading = 0;	/* Set when readfile() is active */
double fn1_eval();
double fn2_eval();
struct	ent *firstev = (struct ent *)0;	/* first expr in the eval list */

#define PI (double)3.14159265358979323846
#define dtr(x) ((x)*(PI/(double)180.0))
#define rtd(x) ((x)*(180.0/(double)PI))

double finfunc(fun,v1,v2,v3)
int fun;
double v1,v2,v3;
{
 	double answer,p;
 
 	p = fn2_eval(pow, 1 + v2, v3);
 
 	switch(fun)
 	{
 	case PV:
 		answer = v1 * (1 - 1/p) / v2;
 		break;
 	case FV:
 		answer = v1 * (p - 1) / v2;
 		break;
 	case PMT:
 		answer = v1 * v2 / (1 - 1/p);
 		break;
	default:
		error("Unknown function in finfunc");
		return((double)0);
	}
	return(answer);
}

char *
dostindex( val, minr, minc, maxr, maxc)
double val;
int minr, minc, maxr, maxc;
{
    register r,c;
    register struct ent *p;
    char *pr;
    int x;

    x = (int) val;
    r = minr; c = minc;
    p = (struct ent *)0;
    if ( minr == maxr ) { /* look along the row */
	c = minc + x - 1;
	if (c <= maxc && c >=minc)
	    p = *ATBL(tbl, r, c);
    } else if ( minc == maxc ) { /* look down the column */
	r = minr + x - 1;
	if (r <= maxr && r >=minr)
	    p = *ATBL(tbl, r, c);
    } else {
	error ("range specified to @stindex");
	return((char *)0);
    }

    if (p && p->label) {
	pr = xmalloc((unsigned)(strlen(p->label)+1));
	(void)strcpy(pr, p->label);
	return (pr);
     } else
	return((char *)0);
}

double
doindex( val, minr, minc, maxr, maxc)
double val;
int minr, minc, maxr, maxc;
{
    double v;
    register r,c;
    register struct ent *p;
    int x;

    x = (int) val;
    v = (double)0;
    r = minr; c = minc;
    if ( minr == maxr ) { /* look along the row */
	c = minc + x - 1;
	if (c <= maxc && c >=minc 
		&& (p = *ATBL(tbl, r, c)) && p->flags&is_valid )
					return p->v;
	}
    else if ( minc == maxc ){ /* look down the column */
	r = minr + x - 1;
	if (r <= maxr && r >=minr 
		&& (p = *ATBL(tbl, r, c)) && p->flags&is_valid )
					return p->v;
	}
    else error(" range specified to @index");
    return v;
}

double
dolookup( val, minr, minc, maxr, maxc, offr, offc)
struct enode * val;
int minr, minc, maxr, maxc, offr, offc;
{
    double v, ret = (double)0;
    register r,c;
    register struct ent *p = (struct ent *)0;
    int incr,incc,fndr,fndc;
    char *s;

    incr = (offc != 0); incc = (offr != 0);
    if (etype(val) == NUM) {
	v = eval(val);
	for (r = minr, c = minc; r <= maxr && c <= maxc; r+=incr, c+=incc) {
	    if ( (p = *ATBL(tbl, r, c)) && p->flags&is_valid ) {
		if (p->v <= v) {
		    fndr = incc ? (minr + offr) : r;
		    fndc = incr ? (minc + offc) : c;
		    if (ISVALID(fndr,fndc))
			p = *ATBL(tbl, fndr, fndc);
		    else error(" range specified to @[hv]lookup");
		    if ( p && p->flags&is_valid)
			ret = p->v;
		} else break;
	    }
	}
    } else {
	s = seval(val);
	for (r = minr, c = minc; r <= maxr && c <= maxc; r+=incr, c+=incc) {
	    if ( (p = *ATBL(tbl, r, c)) && p->label ) {
		if (strcmp(p->label,s) == 0) {
		    fndr = incc ? (minr + offr) : r;
		    fndc = incr ? (minc + offc) : c;
		    if (ISVALID(fndr,fndc))
			p = *ATBL(tbl, fndr, fndc);
		    else error(" range specified to @[hv]lookup");
		    break;
		}
	    }
	}
	if ( p && p->flags&is_valid)
	    ret = p->v;
	xfree(s);
    }
    return ret;
}

double
docount(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
    int v;
    register r,c;
    register struct ent *p;

    v = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++)
	    if ((p = *ATBL(tbl, r, c)) && p->flags&is_valid)
		v++;
    return v;
}

double
dosum(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
    double v;
    register r,c;
    register struct ent *p;

    v = (double)0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++)
	    if ((p = *ATBL(tbl, r, c)) && p->flags&is_valid)
		v += p->v;
    return v;
}

double
doprod(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
    double v;
    register r,c;
    register struct ent *p;

    v = 1;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++)
	    if ((p = *ATBL(tbl, r, c)) && p->flags&is_valid)
		v *= p->v;
    return v;
}

double
doavg(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
    double v;
    register r,c,count;
    register struct ent *p;

    v = (double)0;
    count = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++)
	    if ((p = *ATBL(tbl, r, c)) && p->flags&is_valid) {
		v += p->v;
		count++;
	    }

    if (count == 0) 
	return ((double) 0);

    return (v / (double)count);
}

double
dostddev(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
    double lp, rp, v, nd;
    register r,c,n;
    register struct ent *p;

    n = 0;
    lp = 0;
    rp = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++)
	    if ((p = *ATBL(tbl, r, c)) && p->flags&is_valid) {
		v = p->v;
		lp += v*v;
		rp += v;
		n++;
	    }

    if ((n == 0) || (n == 1)) 
	return ((double) 0);
    nd = (double)n;
    return (sqrt((nd*lp-rp*rp)/(nd*(nd-1))));
}

double
domax(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
    double v = (double)0;
    register r,c,count;
    register struct ent *p;

    count = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++)
	    if ((p = *ATBL(tbl, r, c)) && p->flags&is_valid) {
		if (!count) {
		    v = p->v;
		    count++;
		} else if (p->v > v)
		    v = p->v;
	    }

    if (count == 0) 
	return ((double) 0);

    return (v);
}

double
domin(minr, minc, maxr, maxc)
int minr, minc, maxr, maxc;
{
    double v = (double)0;
    register r,c,count;
    register struct ent *p;

    count = 0;
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++)
	    if ((p = *ATBL(tbl, r, c)) && p->flags&is_valid) {
		if (!count) {
		    v = p->v;
		    count++;
		} else if (p->v < v)
		    v = p->v;
	    }

    if (count == 0) 
	return ((double) 0);

    return (v);
}

#define sec_min 60
#define sec_hr  3600L
#define sec_day 86400L
#define sec_yr  31471200L     /* 364.25 days/yr */
#define sec_mo  2622600L       /* sec_yr/12: sort of an average */
int mdays[12]={ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

double
dodts(mo, day, yr)
int mo, day, yr;
{
    long trial;
    register struct tm *tp; 
    register int i;
    register long jdate;

    mdays[1] = 28 + (yr%4 == 0);

    if (mo < 1 || mo > 12 || day < 1 || day > mdays[--mo] ||
		yr > 1999 || yr < 1970) {
	error("@dts: invalid argument");
	return(0.0);
    }

    jdate = day-1;
    for (i=0; i<mo; i++)
	    jdate += mdays[i];
    for (i = 1970; i < yr; i++)
	    jdate += 365 + (i%4 == 0);

    trial = jdate * sec_day; 

    yr -= 1900;

    tp = localtime(&trial);

    if (tp->tm_year != yr) {
	    /*
	    * We may fail this test once a year because of time zone
	     * and daylight savings time errors.  This bounces the
	     * trial time past the boundary.  The error introduced is
	     * corrected below.
	     */
	    trial += sec_day*(yr - tp->tm_year);
	    tp = localtime(&trial);
    }
    if (tp->tm_mon != mo) {
	    /* We may fail this test once a month.  */
	    trial += sec_day*(mo - tp->tm_mon);
	    tp = localtime(&trial);
    }
    if (tp->tm_mday + tp->tm_hour + tp->tm_min + tp->tm_sec != day) {
	trial -= (tp->tm_mday - day)*sec_day +  tp->tm_hour*sec_hr
		 + tp->tm_min*sec_min + tp->tm_sec;
    }

#ifdef DEBUGDTS
    tp = localtime(&trial);
    if (tp->tm_mday + tp->tm_hour + tp->tm_min + tp->tm_sec + 
	tp->tm_year + tp->tm_mon != yr+mo+day)
		error("Dts broke down");
#endif

    return ((double)trial);
}

double
dotts(hr, min, sec)
int hr, min, sec;
{
    if (hr < 0 || hr > 23 || min < 0 || min > 59 || sec < 0 || sec > 59) {
	error ("@tts: Invalid argument");
	return ((double)0);
    }
    return ((double)(sec+min*60+hr*3600));
}

double
dotime(which, when)
int which;
double when;
{
	long time();

	static long t_cache;
	static struct tm tm_cache;
	struct tm *tp;
	long tloc;

	if (which == NOW) 
	    return (double)time((long *)0);

	tloc = (long)when;

	if (tloc != t_cache) {
	    tp = localtime(&tloc);
	    tm_cache = *tp;
	    tm_cache.tm_mon += 1;
	    tm_cache.tm_year += 1900;
	    t_cache = tloc;
	}

	switch (which) {
	    case HOUR: return((double)(tm_cache.tm_hour));
	    case MINUTE: return((double)(tm_cache.tm_min));
	    case SECOND: return((double)(tm_cache.tm_sec));
	    case MONTH: return((double)(tm_cache.tm_mon));
	    case DAY: return((double)(tm_cache.tm_mday));
	    case YEAR: return((double)(tm_cache.tm_year));
	}
	/* Safety net */
	return ((double)0);
}

double
doston(s)
char *s;
{
    char *strtof();
    double v;

    if (!s)
	return((double)0);

    (void)strtof(s, &v);
    xfree(s);
    return(v);
}

double
doeqs(s1, s2)
char *s1, *s2;
{
    double v;

    if (!s1 && !s2)
	return((double)1.0);

    if (!s1 || !s2)
	v = 0.0;
    else if (strcmp(s1, s2) == 0)
	v = 1.0;
    else
	v = 0.0;

    if (s1)
    	xfree(s1);

    if (s2)
    	xfree(s2);

    return(v);
}


/*
 * Given a string representing a column name and a value which is a column
 * number, return a pointer to the selected cell's entry, if any, else 0.  Use
 * only the integer part of the column number.  Always free the string.
 */

struct ent *
getent (colstr, rowdoub)
    char *colstr;
    double rowdoub;
{
    int collen;		/* length of string */
    int row, col;	/* integer values   */
    struct ent *ep = (struct ent *)0;	/* selected entry   */

    if (((row = (int) floor (rowdoub)) >= 0)
     && (row < maxrows)				/* in range */
     && ((collen = strlen (colstr)) <= 2)	/* not too long */
     && ((col = atocol (colstr, collen)) >= 0)
     && (col < maxcols))			/* in range */
    {
	ep = *ATBL(tbl, row, col);
    }

    xfree (colstr);
    return (ep);
}


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's numeric value, if any.
 */

double
donval (colstr, rowdoub)
    char *colstr;
    double rowdoub;
{
    struct ent *ep;

    return (((ep = getent (colstr, rowdoub)) && ((ep -> flags) & is_valid)) ?
	    (ep -> v) : (double)0);
}


/*
 *	The list routines (e.g. dolmax) are called with an LMAX enode.
 *	The left pointer is a chain of ELIST nodes, the right pointer
 *	is a value.
 */
double
dolmax(ep)
struct enode *ep;
{
	register int count = 0;
	register double maxval = 0; /* Assignment to shut up lint */
	register struct enode *p;
	register double v;

	for (p = ep; p; p = p->e.o.left) {
		v = eval(p->e.o.right);
		if (!count || v > maxval) {
			maxval = v; count++;
		}
	}
	if (count) return maxval;
	else return (double)0;
}

double
dolmin(ep)
struct enode *ep;
{
	register int count = 0;
	register double minval = 0; /* Assignment to shut up lint */
	register struct enode *p;
	register double v;

	for (p = ep; p; p = p->e.o.left) {
		v = eval(p->e.o.right);
		if (!count || v < minval) {
			minval = v; count++;
		}
	}
	if (count) return minval;
	else return (double)0;
}

double 
eval(e)
register struct enode *e;
{
    if (e == (struct enode *)0) return (double)0;
    switch (e->op) {
	case '+':	return (eval(e->e.o.left) + eval(e->e.o.right));
	case '-':	return (eval(e->e.o.left) - eval(e->e.o.right));
	case '*':	return (eval(e->e.o.left) * eval(e->e.o.right));
	case '/':       return (eval(e->e.o.left) / eval(e->e.o.right));
	case '%':     {	double num, denom;
			num = floor(eval(e->e.o.left));
			denom = floor(eval (e->e.o.right));
			return denom ? num - floor(num/denom)*denom : (double)0; }
	case '^':	return (fn2_eval(pow,eval(e->e.o.left),eval(e->e.o.right)));
	case '<':	return (eval(e->e.o.left) < eval(e->e.o.right));
	case '=':	return (eval(e->e.o.left) == eval(e->e.o.right));
	case '>':	return (eval(e->e.o.left) > eval(e->e.o.right));
	case '&':	return (eval(e->e.o.left) && eval(e->e.o.right));
	case '|':	return (eval(e->e.o.left) || eval(e->e.o.right));
	case IF:
	case '?':	return eval(e->e.o.left) ? eval(e->e.o.right->e.o.left)
						: eval(e->e.o.right->e.o.right);
	case 'm':	return (-eval(e->e.o.right));
	case 'f':	return (eval(e->e.o.right));
	case '~':	return (eval(e->e.o.right) == 0.0);
	case 'k':	return (e->e.k);
	case 'v':	return (e->e.v.vp->v);
	case INDEX:
	case LOOKUP:
	case HLOOKUP:
	case VLOOKUP:
	    {	register r,c;
		register maxr, maxc;
		register minr, minc;
		maxr = e->e.o.right->e.r.right.vp -> row;
		maxc = e->e.o.right->e.r.right.vp -> col;
		minr = e->e.o.right->e.r.left.vp -> row;
		minc = e->e.o.right->e.r.left.vp -> col;
		if (minr>maxr) r = maxr, maxr = minr, minr = r;
		if (minc>maxc) c = maxc, maxc = minc, minc = c;
		switch(e->op){
		case LOOKUP:
		    return dolookup(e->e.o.left, minr, minc, maxr, maxc,
				     minr==maxr, minc==maxc);
		case HLOOKUP:
	            return dolookup(e->e.o.left->e.o.left, minr,minc,maxr,maxc,
			(int) eval(e->e.o.left->e.o.right), 0);
		case VLOOKUP:
	            return dolookup(e->e.o.left->e.o.left, minr,minc,maxr,maxc,
			0, (int) eval(e->e.o.left->e.o.right));
		case INDEX:
		    return doindex(eval(e->e.o.left), minr, minc, maxr, maxc);
		}
	    }
	case REDUCE | '+':
 	case REDUCE | '*':
 	case REDUCE | 'a':
 	case REDUCE | 'c':
 	case REDUCE | 's':
	case REDUCE | MAX:
	case REDUCE | MIN:
	    {	register r,c;
		register maxr, maxc;
		register minr, minc;
		maxr = e->e.r.right.vp -> row;
		maxc = e->e.r.right.vp -> col;
		minr = e->e.r.left.vp -> row;
		minc = e->e.r.left.vp -> col;
		if (minr>maxr) r = maxr, maxr = minr, minr = r;
		if (minc>maxc) c = maxc, maxc = minc, minc = c;
	        switch (e->op) {
	            case REDUCE | '+': return dosum(minr, minc, maxr, maxc);
 	            case REDUCE | '*': return doprod(minr, minc, maxr, maxc);
 	            case REDUCE | 'a': return doavg(minr, minc, maxr, maxc);
 	            case REDUCE | 'c': return docount(minr, minc, maxr, maxc);
 	            case REDUCE | 's': return dostddev(minr, minc, maxr, maxc);
 	            case REDUCE | MAX: return domax(minr, minc, maxr, maxc);
 	            case REDUCE | MIN: return domin(minr, minc, maxr, maxc);
		}
	    }
	case ABS:	 return (fn1_eval( fabs, eval(e->e.o.right)));
	case ACOS:	 return (fn1_eval( acos, eval(e->e.o.right)));
	case ASIN:	 return (fn1_eval( asin, eval(e->e.o.right)));
	case ATAN:	 return (fn1_eval( atan, eval(e->e.o.right)));
	case ATAN2:	 return (fn2_eval( atan2, eval(e->e.o.left), eval(e->e.o.right)));
	case CEIL:	 return (fn1_eval( ceil, eval(e->e.o.right)));
	case COS:	 return (fn1_eval( cos, eval(e->e.o.right)));
	case EXP:	 return (fn1_eval( exp, eval(e->e.o.right)));
	case FABS:	 return (fn1_eval( fabs, eval(e->e.o.right)));
	case FLOOR:	 return (fn1_eval( floor, eval(e->e.o.right)));
	case HYPOT:	 return (fn2_eval( hypot, eval(e->e.o.left), eval(e->e.o.right)));
	case LOG:	 return (fn1_eval( log, eval(e->e.o.right)));
	case LOG10:	 return (fn1_eval( log10, eval(e->e.o.right)));
	case POW:	 return (fn2_eval( pow, eval(e->e.o.left), eval(e->e.o.right)));
	case SIN:	 return (fn1_eval( sin, eval(e->e.o.right)));
	case SQRT:	 return (fn1_eval( sqrt, eval(e->e.o.right)));
	case TAN:	 return (fn1_eval( tan, eval(e->e.o.right)));
	case DTR:	 return (dtr(eval(e->e.o.right)));
	case RTD:	 return (rtd(eval(e->e.o.right)));
	case RND:	 {
			    double temp;
			    temp = eval(e->e.o.right);
			    return(temp-floor(temp) < 0.5 ?
					     floor(temp) : ceil(temp));
			}
 	case ROUND:	{
			    double temp = eval(e->e.o.left);
			    int prec = (int) eval(e->e.o.right), scal = 1;
			    while (prec-- > 0) scal *= 10;
			    temp *= scal;
			    temp = ((temp-floor(temp)) < 0.5 ?
				    floor(temp) : ceil(temp));
			    return(temp / scal);
			}
	case FV:
	case PV:
	case PMT:	return(finfunc(e->op,eval(e->e.o.left),
				   eval(e->e.o.right->e.o.left),
				      eval(e->e.o.right->e.o.right)));
	case HOUR:	 return (dotime(HOUR, eval(e->e.o.right)));
	case MINUTE:	 return (dotime(MINUTE, eval(e->e.o.right)));
	case SECOND:	 return (dotime(SECOND, eval(e->e.o.right)));
	case MONTH:	 return (dotime(MONTH, eval(e->e.o.right)));
	case DAY:	 return (dotime(DAY, eval(e->e.o.right)));
	case YEAR:	 return (dotime(YEAR, eval(e->e.o.right)));
	case NOW:	 return (dotime(NOW, (double)0.0));
	case DTS:	 return (dodts((int)eval(e->e.o.left),
				 (int)eval(e->e.o.right->e.o.left),
				 (int)eval(e->e.o.right->e.o.right)));
	case TTS:	 return (dotts((int)eval(e->e.o.left),
				 (int)eval(e->e.o.right->e.o.left),
				 (int)eval(e->e.o.right->e.o.right)));
	case STON:	 return (doston(seval(e->e.o.right)));
	case EQS:        return (doeqs(seval(e->e.o.right),seval(e->e.o.left)));
	case LMAX:	 return dolmax(e);
	case LMIN:	 return dolmin(e);
	case NVAL:       return (donval(seval(e->e.o.left),eval(e->e.o.right)));
	default:	 error ("Illegal numeric expression");
			 exprerr = 1;
    }
    return((double)0.0);
}

#ifdef SIGVOID
void
#endif
eval_fpe(signo) /* Trap for FPE errors in eval */
int signo;
{
#ifdef IEEE_MATH
	(void)fpsetsticky((fp_except)0); 		/* Clear exception */
#endif /* IEEE_MATH */
	longjmp(fpe_save, 1);
}

double fn1_eval(fn, arg)
double (*fn)();
double arg;
{
	double res;
	errno = 0;
	res = (*fn)(arg);
	if(errno)
	  eval_fpe(0);

	return res;
}

double fn2_eval(fn, arg1, arg2)
double (*fn)();
double arg1, arg2;
{
	double res;
	errno = 0;
	res = (*fn)(arg1, arg2);
	if(errno) 
	    eval_fpe(0);

	return res;
}

/* 
 * Rules for string functions:
 * Take string arguments which they xfree.
 * All returned strings are assumed to be xalloced.
 */

char *
docat(s1, s2)
register char *s1, *s2;
{
    register char *p;
    char *arg1, *arg2;

    if (!s1 && !s2)
	return((char *)0);
    arg1 = s1 ? s1 : "";
    arg2 = s2 ? s2 : "";
    p = xmalloc((unsigned)(strlen(arg1)+strlen(arg2)+1));
    (void) strcpy(p, arg1);
    (void) strcat(p, arg2);
    if (s1)
        xfree(s1);
    if (s2)
        xfree(s2);
    return(p);
}

char *
dodate(tloc)
long tloc;
{
    char *tp;
    char *p;

    tp = ctime(&tloc);
    tp[24] = '\0';
    p = xmalloc((unsigned)25);
    (void) strcpy(p, tp);
    return(p);
}


char *
dofmt(fmtstr, v)
char *fmtstr;
double v;
{
    char buff[FBUFLEN];
    char *p;

    if (!fmtstr)
	return((char *)0);
    (void)sprintf(buff, fmtstr, v);
    p = xmalloc((unsigned)(strlen(buff)+1));
    (void) strcpy(p, buff);
    xfree(fmtstr);
    return(p);
}


/*
 * Given a command name and a value, run the command with the given value and
 * read and return its first output line (only) as an allocated string, always
 * a copy of prevstr, which is set appropriately first unless external
 * functions are disabled, in which case the previous value is used.  The
 * handling of prevstr and freeing of command is tricky.  Returning an
 * allocated string in all cases, even if null, insures cell expressions are
 * written to files, etc.
 */

#ifdef VMS
char *
doext(command, value)
char *command;
double value;
{
    error("Warning: External functions unavailable on VMS");
    if (command)
	xfree(command);
    return (strcpy (xmalloc((unsigned) 1), "\0"));
}

#else /* VMS */

char *
doext (command, value)
char   *command;
double value;
{
    static char *prevstr = (char *)0;	/* previous result */
    char buff[FBUFLEN];		/* command line/return, not permanently alloc */

    if (!prevstr) {
	prevstr = xmalloc((unsigned)1);
	*prevstr = '\0';
    }
    if (!extfunc)    {
	error ("Warning: external functions disabled; using %s value",
		prevstr ? "previous" : "null");

	if (command) xfree (command);
    } else {
	if (prevstr) xfree (prevstr);		/* no longer needed */
	prevstr = '\0';

	if ((! command) || (! *command)) {
	    error ("Warning: external function given null command name");
	    if (command) xfree (command);
	} else {
	    FILE *pp;

	    (void) sprintf (buff, "%s %g", command, value); /* build cmd line */
	    xfree (command);

	    error ("Running external function...");
	    (void) refresh();

	    if ((pp = popen (buff, "r")) == (FILE *) NULL)	/* run it */
		error ("Warning: running \"%s\" failed", buff);
	    else {
		if (fgets (buff, sizeof(buff)-1, pp) == NULL)	/* one line */
		    error ("Warning: external function returned nothing");
		else {
		    char *cp;

		    error ("");				/* erase notice */
		    buff[sizeof(buff)-1] = '\0';

		    if (cp = strchr (buff, '\n'))	/* contains newline */
			*cp = '\0';			/* end string there */

		    (void) strcpy (prevstr = 
			 xmalloc ((unsigned) (strlen (buff) + 1)), buff);
			 /* save alloc'd copy */
		}
		(void) pclose (pp);

	    } /* else */
	} /* else */
    } /* else */
    return (strcpy (xmalloc ((unsigned) (strlen (prevstr) + 1)), prevstr));
}

#endif /* VMS */


/*
 * Given a string representing a column name and a value which is a column
 * number, return the selected cell's string value, if any.  Even if none,
 * still allocate and return a null string so the cell has a label value so
 * the expression is saved in a file, etc.
 */

char *
dosval (colstr, rowdoub)
    char *colstr;
    double rowdoub;
{
    struct ent *ep;
    char *label;

    label = (ep = getent (colstr, rowdoub)) ? (ep -> label) : "";
    return (strcpy (xmalloc ((unsigned) (strlen (label) + 1)), label));
}


/*
 * Substring:  Note that v1 and v2 are one-based to users, but zero-based
 * when calling this routine.
 */

char *
dosubstr(s, v1, v2)
char *s;
register int v1,v2;
{
    register char *s1, *s2;
    char *p;

    if (!s)
	return((char *)0);

    if (v2 >= strlen (s))		/* past end */
	v2 =  strlen (s) - 1;		/* to end   */

    if (v1 < 0 || v1 > v2) {		/* out of range, return null string */
	xfree(s);
	p = xmalloc((unsigned)1);
	p[0] = '\0';
	return(p);
    }
    s2 = p = xmalloc((unsigned)(v2-v1+2));
    s1 = &s[v1];
    for(; v1 <= v2; s1++, s2++, v1++)
	*s2 = *s1;
    *s2 = '\0';
    xfree(s);
    return(p);
}

char *
seval(se)
register struct enode *se;
{
    register char *p;

    if (se == (struct enode *)0) return (char *)0;
    switch (se->op) {
	case O_SCONST: p = xmalloc((unsigned)(strlen(se->e.s)+1));
		     (void) strcpy(p, se->e.s);
		     return(p);
	case O_VAR:    {
			struct ent *ep;
			ep = se->e.v.vp;

			if (!ep->label)
			    return((char *)0);
			p = xmalloc((unsigned)(strlen(ep->label)+1));
			(void) strcpy(p, ep->label);
			return(p);
		     }
	case '#':    return(docat(seval(se->e.o.left), seval(se->e.o.right)));
	case 'f':    return(seval(se->e.o.right));
	case IF:
	case '?':    return(eval(se->e.o.left) ? seval(se->e.o.right->e.o.left)
					     : seval(se->e.o.right->e.o.right));
	case DATE:   return(dodate((long)(eval(se->e.o.right))));
	case FMT:    return(dofmt(seval(se->e.o.left), eval(se->e.o.right)));
 	case STINDEX:
 		{	register r,c;
 		register maxr, maxc;
 		register minr, minc;
 		maxr = se->e.o.right->e.r.right.vp -> row;
 		maxc = se->e.o.right->e.r.right.vp -> col;
 		minr = se->e.o.right->e.r.left.vp -> row;
 		minc = se->e.o.right->e.r.left.vp -> col;
 		if (minr>maxr) r = maxr, maxr = minr, minr = r;
 		if (minc>maxc) c = maxc, maxc = minc, minc = c;
 	        return dostindex(eval(se->e.o.left), minr, minc, maxr, maxc);
		}
	case EXT:    return(doext(seval(se->e.o.left), eval(se->e.o.right)));
	case SVAL:   return(dosval(seval(se->e.o.left), eval(se->e.o.right)));
	case SUBSTR: return(dosubstr(seval(se->e.o.left),
			    (int)eval(se->e.o.right->e.o.left) - 1,
			    (int)eval(se->e.o.right->e.o.right) - 1));
	default:
		     error ("Illegal string expression");
		     exprerr = 1;
		     return((char *)0);
	}
}

/*
 * The graph formed by cell expressions which use other cells's values is not
 * evaluated "bottom up".  The whole table is merely re-evaluated cell by cell,
 * top to bottom, left to right, in RealEvalAll().  Each cell's expression uses
 * constants in other cells.  However, RealEvalAll() notices when a cell gets a
 * new numeric or string value, and reports if this happens for any cell.
 * EvalAll() repeats calling RealEvalAll() until there are no changes or the
 * evaluation count expires.
 */

int propagation = 10;	/* max number of times to try calculation */

void
setiterations(i)
int i;
{
	if(i<1) {
		error("iteration count must be at least 1");
		propagation = 1;
		}
	else propagation = i;
}

void
EvalAll () {
      int lastcnt, repct = 0;
  
     while ((lastcnt = RealEvalAll()) && (repct++ <= propagation));
     if((propagation>1)&& (lastcnt >0 ))
 	    error("Still changing after %d iterations",propagation-1);
}

/*
 * Evaluate all cells which have expressions and alter their numeric or string
 * values.  Return the number of cells which changed.
 */

int 
RealEvalAll () {
    register int i,j;
    int chgct = 0;
    register struct ent *p;

    (void) signal(SIGFPE, eval_fpe);
#ifdef EXPRTREE
    for (p = firstev; p; p = p->evnext)
	    RealEvalOne(p, &chgct);
#else
    if(calc_order == BYROWS ) {
	for (i=0; i<=maxrow; i++)
	    for (j=0; j<=maxcol; j++)
		if ((p=tbl[i][j]) && p->expr) RealEvalOne(p,i,j, &chgct);
    }
    else if ( calc_order == BYCOLS ) {
	for (j=0; j<=maxcol; j++)
	{   for (i=0; i<=maxrow; i++)
		if ((p=tbl[i][j]) && p->expr) RealEvalOne(p,i,j, &chgct);
	}
    }
    else error("Internal error calc_order");
#endif
 
    (void) signal(SIGFPE, quit);
    return(chgct);
}

void
#ifdef EXPRTREE
RealEvalOne(p, chgct)
register struct ent *p;
int *chgct;
#else
RealEvalOne(p, i, j, chgct)
register struct ent *p;
int i, j, *chgct;
#endif
{
	if (p->flags & is_strexpr) {
	    char *v;
	    if (setjmp(fpe_save)) {
#ifdef EXPRTREE
		error("Floating point exception %s", v_name(p->row, p->col));
#else
		error("Floating point exception %s", v_name(i, j));
#endif
		v = "";
	    } else {
		v = seval(p->expr);
	    }
	    if (!v && !p->label) /* Everything's fine */
		return;
	    if (!p->label || !v || strcmp(v, p->label) != 0) {
		(*chgct)++;
		p->flags |= is_changed;
		changed++;
	    }
	    if(p->label)
		xfree(p->label);
	    p->label = v;
	} else {
	    double v;
	    if (setjmp(fpe_save)) {
#ifdef EXPRTREE
		error("Floating point exception %s", v_name(p->row, p->col));
#else
		error("Floating point exception %s", v_name(i, j));
#endif
		v = (double)0.0;
	    } else {
		v = eval (p->expr);
	    }
	    if (v != p->v) {
		p->v = v; (*chgct)++;
		p->flags |= is_changed|is_valid;
		changed++;
	    }
	}
}

struct enode *
new(op, a1, a2)
int	op;
struct enode *a1, *a2;
{
    register struct enode *p;
    p = (struct enode *) xmalloc ((unsigned)sizeof (struct enode));
    p->op = op;
    p->e.o.left = a1;
    p->e.o.right = a2;
    return p;
}

struct enode *
new_var(op, a1)
int	op;
struct ent_ptr a1;
{
    register struct enode *p;
    p = (struct enode *) xmalloc ((unsigned)sizeof (struct enode));
    p->op = op;
    p->e.v = a1;
    return p;
}

struct enode *
new_range(op, a1)
int	op;
struct range_s a1;
{
    register struct enode *p;
    p = (struct enode *) xmalloc ((unsigned)sizeof (struct enode));
    p->op = op;
    p->e.r = a1;
    return p;
}

struct enode *
new_const(op, a1)
int	op;
double a1;
{
    register struct enode *p;
    p = (struct enode *) xmalloc ((unsigned)sizeof (struct enode));
    p->op = op;
    p->e.k = a1;
    return p;
}

struct enode *
new_str(s)
char *s;
{
    register struct enode *p;

    p = (struct enode *) xmalloc ((unsigned)sizeof(struct enode));
    p->op = O_SCONST;
    p->e.s = s;
    return(p);
}

void
copy(dv1, dv2, v1, v2)
struct ent *dv1, *dv2, *v1, *v2;
{
    int minsr, minsc;
    int maxsr, maxsc;
    int mindr, mindc;
    int maxdr, maxdc;
    int vr, vc;
    int r, c;

    mindr = dv1->row;
    mindc = dv1->col;
    maxdr = dv2->row;
    maxdc = dv2->col;
    if (mindr>maxdr) r = maxdr, maxdr = mindr, mindr = r;
    if (mindc>maxdc) c = maxdc, maxdc = mindc, mindc = c;
    maxsr = v2->row;
    maxsc = v2->col;
    minsr = v1->row;
    minsc = v1->col;
    if (minsr>maxsr) r = maxsr, maxsr = minsr, minsr = r;
    if (minsc>maxsc) c = maxsc, maxsc = minsc, minsc = c;
    checkbounds(&maxdr, &maxdc);

    erase_area(mindr, mindc, maxdr, maxdc);
    if (minsr == maxsr && minsc == maxsc) {
	/* Source is a single cell */
	for(vr = mindr; vr <= maxdr; vr++)
	    for (vc = mindc; vc <= maxdc; vc++)
		copyrtv(vr, vc, minsr, minsc, maxsr, maxsc);
    } else if (minsr == maxsr) {
	/* Source is a single row */
	for (vr = mindr; vr <= maxdr; vr++)
	    copyrtv(vr, mindc, minsr, minsc, maxsr, maxsc);
    } else if (minsc == maxsc) {
	/* Source is a single column */
	for (vc = mindc; vc <= maxdc; vc++)
	    copyrtv(mindr, vc, minsr, minsc, maxsr, maxsc);
    } else {
	/* Everything else */
	copyrtv(mindr, mindc, minsr, minsc, maxsr, maxsc);
    }
    sync_refs();
}

void
copyrtv(vr, vc, minsr, minsc, maxsr, maxsc)
int vr, vc, minsr, minsc, maxsr, maxsc;
{
    register struct ent *p;
    register struct ent *n;
    register int sr, sc;
    register int dr, dc;

    for (dr=vr, sr=minsr; sr<=maxsr; sr++, dr++)
	for (dc=vc, sc=minsc; sc<=maxsc; sc++, dc++) {
	    if (p = *ATBL(tbl, sr, sc))
	    {	n = lookat (dr, dc);
		(void) clearent(n);
		copyent( n, p, dr - sr, dc - sc);
	    }
	    else
	    if (n = *ATBL(tbl, dr, dc))
		(void) clearent(n);
	}
}

void
eraser(v1, v2)
struct ent *v1, *v2;
{
	FullUpdate++;
	flush_saved();
	erase_area(v1->row, v1->col, v2->row, v2->col);
	sync_refs();
}

/* Goto subroutines */

void
g_free()
{
    switch (gs.g_type) {
    case G_STR: xfree(gs.g_s); break;
    default: break;
    }
    gs.g_type = G_NONE;
}

void
go_last()
{
    switch (gs.g_type) {
    case G_NONE:
		error("Nothing to repeat"); break;
    case G_NUM:
		num_search(gs.g_n);
		break;
    case  G_CELL:
		moveto(gs.g_row, gs.g_col);
	    	break;
    case  G_STR: 
		gs.g_type = G_NONE;	/* Don't free the string */
   	    	str_search(gs.g_s); 
	   	break;

    default: error("go_last: internal error");
    }
}

void
moveto(row, col)
int row, col;
{
    currow = row;
    curcol = col;
    g_free();
    gs.g_type = G_CELL;
    gs.g_row = currow;
    gs.g_col = curcol;
}

void
num_search(n)
double n;
{
    register struct ent *p;
    register int r,c;
    int	endr, endc;

    g_free();
    gs.g_type = G_NUM;
    gs.g_n = n;

    if (currow > maxrow)
	endr = maxrow ? maxrow-1 : 0;
    else
	endr = currow;
    if (curcol > maxcol)
	endc = maxcol ? maxcol-1 : 0;
    else
	endc = curcol;
    r = endr;
    c = endc;
    do {
	if (c < maxcol)
	    c++;
	else {
	    if (r < maxrow) {
		while(++r < maxrow && row_hidden[r]) /* */;
		c = 0;
	    } else {
		r = 0;
		c = 0;
	    }
	}
	if (r == endr && c == endc) {
	    error("Number not found");
	    return;
	}
	p = *ATBL(tbl, r, c);
    } while(col_hidden[c] || !p || p && (!(p->flags & is_valid) 
                                        || (p->flags&is_valid) && p->v != n));
    currow = r;
    curcol = c;
}

void
str_search(s)
char *s;
{
    register struct ent *p;
    register int r,c;
    int	endr, endc;
    char *tmp;

#if defined(BSD42) || defined(BSD43)
    if ((tmp = re_comp(s)) != (char *)0) {
	xfree(s);
	error(tmp);
	return;
    }
#endif
#if defined(SYSV2) || defined(SYSV3)
    if ((tmp = regcmp(s, (char *)0)) == (char *)0) {
	xfree(s);
	error("Invalid search string");
	return;
    }
#endif
    g_free();
    gs.g_type = G_STR;
    gs.g_s = s;
    if (currow > maxrow)
	endr = maxrow ? maxrow-1 : 0;
    else
	endr = currow;
    if (curcol > maxcol)
	endc = maxcol ? maxcol-1 : 0;
    else
	endc = curcol;
    r = endr;
    c = endc;
    do {
	if (c < maxcol)
	    c++;
	else {
	    if (r < maxrow) {
		while(++r < maxrow && row_hidden[r]) /* */;
		c = 0;
	    } else {
		r = 0;
		c = 0;
	    }
	}
	if (r == endr && c == endc) {
	    error("String not found");
#if defined(SYSV2) || defined(SYSV3)
	    free(tmp);
#endif
	    return;
	}
	p = *ATBL(tbl, r, c);
    } while(col_hidden[c] || !p || p && (!(p->label) 
#if defined(BSD42) || defined(BSD43)
		  			|| (re_exec(p->label) == 0)));
#else
#if defined(SYSV2) || defined(SYSV3)
                                       || (regex(tmp, p->label) == (char *)0)));
#else
                                       || (strcmp(s, p->label) != 0)));
#endif
#endif
    currow = r;
    curcol = c;
#if defined(SYSV2) || defined(SYSV3)
    free(tmp);
#endif
}

void
fill (v1, v2, start, inc)
struct ent *v1, *v2;
double start, inc;
{
    register r,c;
    register struct ent *n;
    int maxr, maxc;
    int minr, minc;

    maxr = v2->row;
    maxc = v2->col;
    minr = v1->row;
    minc = v1->col;
    if (minr>maxr) r = maxr, maxr = minr, minr = r;
    if (minc>maxc) c = maxc, maxc = minc, minc = c;
    checkbounds(&maxr, &maxc);
    if (minr < 0) minr = 0;
    if (minr < 0) minr = 0;

    FullUpdate++;
    if( calc_order == BYROWS ) {
    for (r = minr; r<=maxr; r++)
	for (c = minc; c<=maxc; c++) {
	    n = lookat (r, c);
	    (void) clearent(n);
	    n->v = start;
	    start += inc;
	    n->flags |= (is_changed|is_valid);
	}
    }
    else if ( calc_order == BYCOLS ) {
    for (c = minc; c<=maxc; c++)
	for (r = minr; r<=maxr; r++) {
	    n = lookat (r, c);
	    (void) clearent(n);
	    n->v = start;
	    start += inc;
	    n->flags |= (is_changed|is_valid);
	}
    }
    else error(" Internal error calc_order");
    changed++;
}

void
let (v, e)
struct ent *v;
struct enode *e;
{
    double val;

    exprerr = 0;
    (void) signal(SIGFPE, eval_fpe);
    if (setjmp(fpe_save)) {
	error ("Floating point exception in cell %s", v_name(v->row, v->col));
	val = (double)0.0;
    } else {
	val = eval(e);
    }
    (void) signal(SIGFPE, quit);
    if (exprerr) {
	efree((struct ent *)0, e);
	return;
    }
    if (constant(e)) {
	if (!loading)
	    v->v = val * prescale;
	else
	    v->v = val;
	if (!(v->flags & is_strexpr)) {
            efree(v, v->expr);
	    v->expr = (struct enode *)0;
	}
	efree((struct ent *)0, e);
        v->flags |= (is_changed|is_valid);
        changed++;
        modflg++;
	return;
    }
    efree (v, v->expr);
    v->expr = e;
    v->flags |= (is_changed|is_valid);
    v->flags &= ~is_strexpr;

#ifdef EXPRTREE
    totoptree(v);
#endif
    changed++;
    modflg++;
}

void
slet (v, se, flushdir)
struct ent *v;
struct enode *se;
int flushdir;
{
    char *p;

    exprerr = 0;
    (void) signal(SIGFPE, eval_fpe);
    if (setjmp(fpe_save)) {
	error ("Floating point exception in cell %s", v_name(v->row, v->col));
	p = "";
    } else {
	p = seval(se);
    }
    (void) signal(SIGFPE, quit);
    if (exprerr) {
	efree((struct ent *)0, se);
	return;
    }
    if (constant(se)) {
	label(v, p, flushdir);
	if (p)
	    xfree(p);
	efree((struct ent *)0, se);
	if (v->flags & is_strexpr) {
            efree (v, v->expr);
	    v->expr = (struct enode *)0;
	    v->flags &= ~is_strexpr;
	}
	return;
    }
    efree (v, v->expr);
    v->expr = se;
    v->flags |= (is_changed|is_strexpr);
    if (flushdir<0) v->flags |= is_leftflush;
    else v->flags &= ~is_leftflush;

#ifdef EXPRTREE
    totoptree();
#endif
    FullUpdate++;
    changed++;
    modflg++;
}

#ifdef EXPRTREE
/*
 * put an expression in the expression tree, only the top of each branch is
 * in the firstev list
 */
totoptree(v)
struct	ent *v;
{
    int	right;
    int	left;
    if (!v->expr)
	return;

#ifdef notdef
    right = FALSE;
    left = FALSE;
    switch(v->expr->op)
    {
		/* no real expression */
	case 'v':
		if (v->expr->o.v->evnext)
			evdel(v->expr->o.v);
	case 'k':
	case LMAX:
	case LMIN:
	case NOW:
	case O_SCONST:
	case O_VAR:
	default:
		return;

		/* left && right */
	case '#':
	case '%':
	case '&':
	case '*':
	case '+':
	case '-':
	case '/':
	case '<':
	case '=':
	case '>':
	case '?':
	case '^':
	case '|':
	case ATAN2:
	case DTS:
	case EQS:
	case EXT:
	case FMT:
	case FV:
	case HYPOT:
	case IF:
	case NVAL:
	case PMT:
	case POW:
	case PV:
	case REDUCE | '*':
	case REDUCE | '+':
	case REDUCE | 'a':
	case REDUCE | 'c':
	case REDUCE | 's':
	case REDUCE | MAX:
	case REDUCE | MIN:
	case ROUND:
	case STINDEX:
	case SUBSTR:
	case SVAL:
	case TTS:
		left = right = TRUE;
		break;
		/* right only */
	case 'f':
	case 'm':
	case '~':
	case ABS:
	case ACOS:
	case ASIN:
	case ATAN:
	case CEIL:
	case COS:
	case DATE:
	case DAY:
	case DTR:
	case EXP:
	case FABS:
	case FLOOR:
	case HLOOKUP:
	case HOUR:
	case IF:
	case INDEX:
	case LOG10:
	case LOG:
	case LOOKUP:
	case MINUTE:
	case MONTH:
	case RND:
	case RTD:
	case SECOND:
	case SIN:
	case SQRT:
	case STON:
	case TAN:
	case VLOOKUP:
	case YEAR:
		right = TRUE;
		break;
    }
	/* for now insert at the beginning of the list */
    v->evnext = firstev;
    v->evprev = (struct ent *)0;
    if (firstev)
	firstev->evprev = v;
    firstev = v;
#endif
    firstev = v;
}
#endif /* EXPRTREE*/

void
hide_row(arg)
int arg;
{
    if (arg < 0) {
	error("Invalid Range");
	return;
    }
    if (arg >= maxrows-1)
    {
	if (!growtbl(GROWROW, arg+1, 0))
	{	error("You can't hide the last row");
		return;
	}
    }
    FullUpdate++;
    row_hidden[arg] = 1;
}

void
hide_col(arg)
int arg;
{
    if (arg < 0) {
	error("Invalid Range");
	return;
    }
    if (arg >= maxcols-1)
    {	if ((arg >= ABSMAXCOLS-1) || !growtbl(GROWCOL, 0, arg+1))
	{	error("You can't hide the last col");
		return;
	}
    }
    FullUpdate++;
    col_hidden[arg] = 1;
}

void
clearent (v)
struct ent *v;
{
    if (!v)
	return;
    label(v,"",-1);
    v->v = (double)0;
    if (v->expr)
	efree(v, v->expr);
    v->expr = (struct enode *)0;
    v->flags |= (is_changed);
    v->flags &= ~(is_valid);
    changed++;
    modflg++;
}

/*
 * Say if an expression is a constant (return 1) or not.
 */
int
constant (e)
    register struct enode *e;
{
    return ((e == (struct enode *)0)
	 || ((e -> op) == O_CONST)
	 || ((e -> op) == O_SCONST)
	 || (((e -> op) != O_VAR)
	  && (((e -> op) & REDUCE) != REDUCE)
	  && constant (e -> e.o.left)
	  && constant (e -> e.o.right)
	  && (e -> op != EXT)	 /* functions look like constants but aren't */
	  && (e -> op != NVAL)
	  && (e -> op != SVAL)
	  && (e -> op != NOW)));
}

void
efree (v, e)
struct ent *v;
struct enode *e;
{
    if (e) {
	if (e->op != O_VAR && e->op !=O_CONST && e->op != O_SCONST
		&& (e->op & REDUCE) != REDUCE) {
	    efree(v, e->e.o.left);
	    efree(v, e->e.o.right);
	}
	if (e->op == O_SCONST && e->e.s)
	    xfree(e->e.s);
	xfree ((char *)e);

#ifdef EXPRTREE
	/* delete this cell from the eval list */
	if (v)
	{	if (v->evprev)
			v->evprev->evnext = v->evnext;
		if (v->evnext)
			v->evnext->evprev = v->evprev;
	}
#endif /* EXPRTREE */
    }
}

void
label (v, s, flushdir)
register struct ent *v;
register char *s;
int	flushdir;
{
    if (v) {
	if (flushdir==0 && v->flags&is_valid) {
	    register struct ent *tv;
	    if (v->col>0 && ((tv=lookat(v->row,v->col-1))->flags&is_valid)==0)
		v = tv, flushdir = 1;
	    else if (((tv=lookat (v->row,v->col+1))->flags&is_valid)==0)
		v = tv, flushdir = -1;
	    else flushdir = -1;
	}
	if (v->label) xfree((char *)(v->label));
	if (s && s[0]) {
	    v->label = xmalloc ((unsigned)(strlen(s)+1));
	    (void) strcpy (v->label, s);
	} else
	    v->label = (char *)0;
	if (flushdir<0) v->flags |= is_leftflush;
	else v->flags &= ~is_leftflush;
	FullUpdate++;
	modflg++;
    }
}

void
decodev (v)
struct ent_ptr v; 
{
	register struct range *r;

	if (!v.vp) (void)sprintf (line+linelim,"VAR?");
	else if ((r = find_range((char *)0, 0, v.vp, v.vp)) && !r->r_is_range)
	    (void)sprintf(line+linelim, "%s", r->r_name);
	else
	    (void)sprintf (line+linelim, "%s%s%s%d",
			v.vf & FIX_COL ? "$" : "",
			coltoa(v.vp->col),
			v.vf & FIX_ROW ? "$" : "",
			v.vp->row);
	linelim += strlen (line+linelim);
}

char *
coltoa(col)
int col;
{
    static char rname[3];
    register char *p = rname;

    if (col > 25) {
	*p++ = col/26 + 'A' - 1;
	col %= 26;
    }
    *p++ = col+'A';
    *p = '\0';
    return(rname);
}

/*
 *	To make list elements come out in the same order
 *	they were entered, we must do a depth-first eval
 *	of the ELIST tree
 */
static void
decompile_list(p)
struct enode *p;
{
	if (!p) return;
	decompile_list(p->e.o.left);	/* depth first */
        decompile(p->e.o.right, 0);
	line[linelim++] = ',';
}

void
decompile(e, priority)
register struct enode *e;
int	priority;
{
    register char *s;
    if (e) {
	int mypriority;
	switch (e->op) {
	default: mypriority = 99; break;
	case '?': mypriority = 1; break;
	case ':': mypriority = 2; break;
	case '|': mypriority = 3; break;
	case '&': mypriority = 4; break;
	case '<': case '=': case '>': mypriority = 6; break;
	case '+': case '-': case '#': mypriority = 8; break;
	case '*': case '/': case '%': mypriority = 10; break;
	case '^': mypriority = 12; break;
	}
	if (mypriority<priority) line[linelim++] = '(';
	switch (e->op) {
	case 'f':	for (s="fixed "; line[linelim++] = *s++;);
			linelim--;
			decompile (e->e.o.right, 30);
			break;
	case 'm':	line[linelim++] = '-';
			decompile (e->e.o.right, 30);
			break;
	case '~':	line[linelim++] = '~';
			decompile (e->e.o.right, 30);
			break;
	case 'v':	decodev (e->e.v);
			break;
	case 'k':	(void)sprintf (line+linelim,"%.15g",e->e.k);
			linelim += strlen (line+linelim);
			break;
	case '$':	(void)sprintf (line+linelim, "\"%s\"", e->e.s);
			linelim += strlen(line+linelim);
			break;

	case REDUCE | '+': range_arg( "@sum(", e); break;
	case REDUCE | '*': range_arg( "@prod(", e); break;
	case REDUCE | 'a': range_arg( "@avg(", e); break;
	case REDUCE | 'c': range_arg( "@count(", e); break;
	case REDUCE | 's': range_arg( "@stddev(", e); break;
	case REDUCE | MAX: range_arg( "@max(", e); break;
	case REDUCE | MIN: range_arg( "@min(", e); break;

	case ABS:		one_arg( "@abs(", e); break;
	case ACOS:	one_arg( "@acos(", e); break;
	case ASIN:	one_arg( "@asin(", e); break;
	case ATAN:	one_arg( "@atan(", e); break;
	case ATAN2:	two_arg( "@atan2(", e); break;
	case CEIL:	one_arg( "@ceil(", e); break;
	case COS:	one_arg( "@cos(", e); break;
	case EXP:	one_arg( "@exp(", e); break;
	case FABS:	one_arg( "@fabs(", e); break;
	case FLOOR:	one_arg( "@floor(", e); break;
	case HYPOT:	two_arg( "@hypot(", e); break;
	case LOG:	one_arg( "@ln(", e); break;
	case LOG10:	one_arg( "@log(", e); break;
	case POW:	two_arg( "@pow(", e); break;
	case SIN:	one_arg( "@sin(", e); break;
	case SQRT:	one_arg( "@sqrt(", e); break;
	case TAN:	one_arg( "@tan(", e); break;
	case DTR:	one_arg( "@dtr(", e); break;
	case RTD:	one_arg( "@rtd(", e); break;
	case RND:	one_arg( "@rnd(", e); break;
	case ROUND:	two_arg( "@round(", e); break;
	case HOUR:	one_arg( "@hour(", e); break;
	case MINUTE:	one_arg( "@minute(", e); break;
	case SECOND:	one_arg( "@second(", e); break;
	case MONTH:	one_arg( "@month(", e); break;
	case DAY:	one_arg( "@day(", e); break;
	case YEAR:	one_arg( "@year(", e); break;
	case DATE:	one_arg( "@date(", e); break;
	case DTS:	three_arg( "@dts(", e); break;
	case TTS:	three_arg( "@tts(", e); break;
	case STON:	one_arg( "@ston(", e); break;
	case FMT:	two_arg( "@fmt(", e); break;
	case EQS:	two_arg( "@eqs(", e); break;
	case NOW:	for ( s = "@now"; line[linelim++] = *s++;);
			linelim--;
			break;
	case LMAX:	list_arg("@max(", e); break;
	case LMIN: 	list_arg("@min(", e); break;
	case FV:	three_arg("@fv(", e); break;
	case PV:	three_arg("@pv(", e); break;
	case PMT:	three_arg("@pmt(", e); break;
	case NVAL:	two_arg("@nval(", e); break;
	case SVAL:	two_arg("@sval(", e); break;
	case EXT:	two_arg("@ext(", e); break;
	case SUBSTR:	three_arg("@substr(", e); break;
	case STINDEX:	index_arg("@stindex(", e); break;
	case INDEX:	index_arg("@index(", e); break;
	case LOOKUP:	index_arg("@lookup(", e); break;
	case HLOOKUP:	two_arg_index("@hlookup(", e); break;
	case VLOOKUP:	two_arg_index("@vlookup(", e); break;
	case IF:	three_arg("@if(", e); break;
	default:	decompile (e->e.o.left, mypriority);
			line[linelim++] = e->op;
			decompile (e->e.o.right, mypriority+1);
			break;
	}
	if (mypriority<priority) line[linelim++] = ')';
    } else line[linelim++] = '?';
}

void
index_arg(s, e)
char *s;
struct enode *e;
{
    for (; line[linelim++] = *s++;);
    linelim--;
    decompile( e-> e.o.left, 0 );
    range_arg(", ", e->e.o.right);
}

void
two_arg_index(s, e)
char *s;
struct enode *e;
{
    for (; line[linelim++] = *s++;);
    linelim--;
    decompile( e->e.o.left->e.o.left, 0 );
    range_arg(",", e->e.o.right);
    linelim--;
    line[linelim++] = ',';
    decompile( e->e.o.left->e.o.right, 0 );
    line[linelim++] = ')';
}

void
list_arg(s, e)
char *s;
struct enode *e;
{
    for (; line[linelim++] = *s++;);
    linelim--;

    decompile (e->e.o.right, 0);
    line[linelim++] = ',';
    decompile_list(e->e.o.left);
    line[linelim - 1] = ')';
}

void
one_arg(s, e)
char *s;
struct enode *e;
{
    for (; line[linelim++] = *s++;);
    linelim--;
    decompile (e->e.o.right, 0);
    line[linelim++] = ')';
}

void
two_arg(s,e)
char *s;
struct enode *e;
{
    for (; line[linelim++] = *s++;);
    linelim--;
    decompile (e->e.o.left, 0);
    line[linelim++] = ',';
    decompile (e->e.o.right, 0);
    line[linelim++] = ')';
}

void
three_arg(s,e)
char *s;
struct enode *e;
{
    for (; line[linelim++] = *s++;);
    linelim--;
    decompile (e->e.o.left, 0);
    line[linelim++] = ',';
    decompile (e->e.o.right->e.o.left, 0);
    line[linelim++] = ',';
    decompile (e->e.o.right->e.o.right, 0);
    line[linelim++] = ')';
}

void
range_arg(s,e)
char *s;
struct enode *e;
{
    struct range *r;

    for (; line[linelim++] = *s++;);
    linelim--;
    if ((r = find_range((char *)0, 0, e->e.r.left.vp,
			     e->e.r.right.vp)) && r->r_is_range) {
	(void)sprintf(line+linelim, "%s", r->r_name);
	linelim += strlen(line+linelim);
    } else {
	decodev (e->e.r.left);
	line[linelim++] = ':';
	decodev (e->e.r.right);
    }
    line[linelim++] = ')';
}

void
editv (row, col)
int row, col;
{
    register struct ent *p;

    p = lookat (row, col);
    (void)sprintf (line, "let %s = ", v_name(row, col));
    linelim = strlen(line);
    if (p->flags & is_strexpr || p->expr == 0) {
	(void)sprintf (line+linelim, "%.15g", p->v);
	linelim += strlen (line+linelim);
    } else {
        editexp(row,col);
    }
}

void
editexp(row,col)
int row, col;
{
    register struct ent *p;

    p = lookat (row, col);
    decompile (p->expr, 0);
    line[linelim] = '\0';
}

void
edits (row, col)
int row, col;
{
    register struct ent *p;

    p = lookat (row, col);
    (void)sprintf (line, "%sstring %s = ",
			((p->flags&is_leftflush) ? "left" : "right"),
			v_name(row, col));
    linelim = strlen(line);
    if (p->flags & is_strexpr && p->expr) {
	editexp(row, col);
    } else if (p->label) {
        (void)sprintf (line+linelim, "\"%s\"", p->label);
        linelim += strlen (line+linelim);
    } else {
        (void)sprintf (line+linelim, "\"");
        linelim += 1;
    }
}
