static char *sccsid = "%W%";

extern errno;

#include "parms.h"
#include "structs.h"
#include <signal.h>			/* signal processing */

#define		LOCKTRY		10	/* number of shots at grabbing */

/*
 * dounix(charstring, flag) will execute that character string as a shell
 * command stolen from shell, though this one catches more signals.
 *
 *	R. Kolstad -- 11/2/80
 *	modified: R. Essick January 1982, to clean up some signal processing
 *
 */
dounix (uid, ttymode, arg0, arg1, arg2, arg3, arg4)
char   *arg0, *arg1, *arg2, *arg3, *arg4;
{
    register    pid,
                rpid;
    int     (*p) (),
            (*q) (),
            (*r) ();
#ifdef	SIGTSTP
    int     (*s) ();
#endif
    char *cmd;
    int     retcode;
    extern char *myshell;

    if (ttymode)
	ttystop();			/* give back to normal mode */
    if ((pid = fork()) == 0) {
	uncatchem();			/* reset this process signals */
					/* if user can get his hands on it */
	if (uid) {			/* only set uid if giving shell */
	    setuid(globuid);		/* give him his uid */
	    umask(msk);			/* restore umask */
	} else {
	    setuid(NOTESUID);
	}

	if (arg0 == 0) {
	    execlp(myshell, myshell, 0);
	    cmd = myshell;
	} else {
	    execlp(arg0, arg0, arg1, arg2, arg3, arg4);
	    cmd = arg0;
	}
	fprintf(stderr, "dounix: execlp failed\n");
	perror(cmd);
	_exit(1);
    }
    p = signal(SIGHUP, SIG_IGN);
    q = signal(SIGINT, SIG_IGN);
    r = signal(SIGQUIT, SIG_IGN);
#ifdef	SIGTSTP
    s = signal(SIGTSTP, SIG_DFL);
#endif
    while ((rpid = wait(&retcode)) != pid && rpid != -1);
    signal(SIGHUP, p);
    signal(SIGINT, q);
    signal(SIGQUIT, r);
#ifdef	SIGTSTP
    signal(SIGTSTP, s);
#endif
    if (ttymode)
	ttystrt();			/* set terminal mode back */
    return(rpid!=-1 ? retcode>>8 : 1);
}

/*
 *	print out an error message and die.
 *	call includes a number and nam of routine where it
 *	died.
 *
 *	Ray Essick 10/23/80
 */
x (cond, p)
char   *p;
{
    if (cond == 0) {
	return;						/* didnt fail */
    }
    perror("notes");
    printf("%s <---- abort (%d)\n", p, errno);
    printf("See a notes guru\n");
    ttystop();						/* back to normal */
#ifdef DUMPCORE
    abort();
#else
    exit(BAD);						/* for production */
#endif
}

int ignsigs;

/*
 *	lock creates a lock file, or waits until it can create the lock.
 *	lock files are of the form lock#  where # is a character passed
 *	to the routine.
 *
 *	Rob Kolstad	10/20/80
 *	modified: rbe December 1981 to add full path name for lock file 
 */
lock (io, c)
struct io_f *io;
char    c;
{
    int     i,
            trys;
    char    p[WDLEN];

    ignsigs++;
    sprintf (p, "%s/%s/%c%s", MSTDIR, LOCKS, c, io->nf);
							/* generate file name */
    trys = LOCKTRY;					/* set him up */
    while ((i = creat (p, 0)) < 0) {
	if (trys-- == 0) {
	    fprintf (stderr, "lock%c combo lost - see notes guru\n", c);
	    ttystop ();
	    exit (BAD);
	}
	sleep (2);				/* guarantee at least 1 */
    }
    close (i);
}

/*
 *	unlock takes the same arguements as the lock routine, and it
 *	will remove the corresponding lock file
 *
 *	Rob Kolstad 10/20/80
 *	modified: rbe December 1981 to add full path name for lock name
 */
unlock (io, c)
struct io_f *io;
char    c;
{
    char    p[WDLEN];

    sprintf (p, "%s/%s/%c%s", MSTDIR, LOCKS, c, io->nf);
							/* generate file name */
    x (unlink (p) < 0, "unlock: unlink lock");
    ignsigs--;
}

/*
 *	glock creates a lock file, or waits until it can create the lock.
 *	lock files are of the form lock#  where # is a character passed
 *	to the routine.
 *		This lock file is a GLOBAL lock - across all notefiles
 *
 *	taken from lock routine above by R. Essick December 1981
 */
glock (io, c)
struct io_f *io;				/* unused in this routine */
char    c;
{
    int     i,
            trys;
    char    p[WDLEN];

    ignsigs++;
    sprintf (p, "%s/%s/%c", MSTDIR, LOCKS, c);		/* generate file name */
    trys = LOCKTRY;
    while ((i = creat (p, 0)) < 0) {
	if (trys-- == 0) {
	    fprintf (stderr, "lock%c combo lost\n", c);
	    ttystop ();
	    exit (BAD);
	}
	sleep (2);		/* is there a smaller time interval */
    }
    close (i);
}


/*
 *	gunlock takes the same arguements as the lock routine, and it
 *	will remove the corresponding lock file
 *		This is GLOBAL locking - across all notefiles
 *
 *	copy of code from unlock, with minor changes
 *	Ray Essick	December 1981
 */
gunlock (io, c)
struct io_f *io;				/* not used by this routine */
char    c;
{
    char    p[WDLEN];

    sprintf (p, "%s/%s/%c", MSTDIR, LOCKS, c);		/* make the file name */
    x (unlink (p) < 0, "gunlock: unlink lock");
    ignsigs--;
}


/*
 * length tells us max(length of string, 1)
 */
len (p, n) char *p;
{
    int     i;
    i = n;
    p += n;
    while (*--p == ' ' && --i) {
	if (i == 0) {
	    i = 1;
	}
    }
    return(i);
}

/*
 *	shell - give the user a shell
 *	this includes:
 *	1) changing to the directory where he came in from
 *	2) giving him a shell
 *	3) return to the notefile directory
 *
 *	original author: Ray Essick may 29, 1981
 *
 */
gshell()
{
	int ret;

	printf ("\n");
	ret = dounix (1, 1, 0, 0, 0, 0, 0);
	if (ret != 0)
		wfchar();
	return(ret);
}

/*	copydate merely moves a when_f structure from 'from' to 'to' */
/*	ray essick - 20-nov-1981	*/
copydate (from, to)
struct when_f  *from,
               *to;
{
    to->w_year = from->w_year;
    to->w_month = from->w_month;
    to->w_day = from->w_day;
    to->w_hours = from->w_hours;
    to->w_mins = from->w_mins;
}

/*	strcmp - tell whether two null terminated strings are equal */
/*	returns < if a < b, 0 if a == b, > if a > b                */
/*	r. essick 20-nov-81 */
strcmp (p, q)
char   *p,
       *q;
{
    register char  *pp,
                   *qq;				/* make it FAST */
    pp = p;
    qq = q;					/* must initialize them */
    for (; *pp == *qq; pp++, qq++) {
	if (*pp == '\0') {
	    return(0);
	}
    }
    return(*pp - *qq);
}

/*	strmove - copy a null terminated string to another */
/*	returns the count of characters moved, this count includes the */
/*	null terminator.. */
/*	r. essick 20-nov-81 */
strmove (p, q)
char   *p,
       *q;						/* from p to q */
{
    int     count;
    register char  *pp,
                   *qq;

    count = 0;				/* start with no characters moved */
    pp = p;
    qq = q;				/* use registers for speed */
    while (*qq++ = *pp++) {
	count++;
    }
    return(count);			/* return count of characters moved */
					/* don't include the terminator */
}

/*	copyauth(from, to) struct auth_f *from,*auth
 *	copys author from from to to
 *	Ray Essick December 1981
 */
copyauth (from, to)
struct auth_f  *from,
               *to;
{

    strmove (from->aname, to->aname);		/* copy the author name */
    to->aid = from->aid;				/* and user id */
}

/*	listget, listconv - parse a list of numbers. 
 *	this is all taken ( sort of ) from Rob Kolstad's getpg
 *	program 
 */
listget (buf, ptr, start, finish)
char    buf[];
int    *ptr,
       *start,
       *finish;
{
    if ((buf[*ptr] < '0' || buf[*ptr] > '9') && buf[*ptr] != ' ') {
	return(0);				/* end of this list */
    }
    *start = listconv (buf, ptr);		/* get the first */
    *finish = *start;				/* default to single */
    if (buf[*ptr] == '-') {
	++(*ptr);				/* trash that separator */
	*finish = listconv (buf, ptr);		/* grab second */
	++(*ptr);				/* bump past delimiter */
	return(2);				/* parsed 2 arguements */
    } else {
	if (buf[*ptr] != '\0') {
	    ++(*ptr);				/* dump delimiter */
	}
	return(1);
    }
}

listconv (buf, ptr)
char    buf[];
int    *ptr;
{
    int     i;
    i = 0;
    while (buf[*ptr] == ' ') {
	++(*ptr);
    }
    while (buf[*ptr] >= '0' && buf[*ptr] <= '9') {
	i = 10 * i + buf[*ptr] - '0';
	++(*ptr);					/* bump him */
    }
    return(i);
}

/*	tolcase - check to see if upper case, and convert to lcase */
/*	R. Essick	Feb 1982 */
tolcase (c)
char    c;
{
    if (c < 'A' || c > 'Z') {
	return c;
    } else {
	return(c - 'A' + 'a');				/* ascii only !!!!! */
    }
}

/*
 * miscellaneous output routines for the terminal
 *
 * center(p,len,row,col) takes a character string pointed at by p and 
 * centers it within a field of length n.  it is printed on screen at row,col
 * (centered).  It is also assumed that p's string is BLANK TERMINATED
 *
 * prdate(w) struct when_f *w;  prints the date.  Assumes 'at' is already done
 *
 * sprdate(w,str) struct when_f *w; char str[]; formats the date and returns
 *		the result in the string pointed to by str.
 *
 */
center(p, len, row, col)
char *p;
{
	int i;
	char *r;

	r = p + len;
	i = len;
	while (*--r == ' ' && --i)
		continue;		/* scan backwards to first nonblank */
	if (i != 0) {
		at(row, col + (len - i) / 2);
		/* text on stdout */
		fwrite(p, sizeof *r, i, stdout);
	}
}

char   *mnames[13] =				/* so indexes work right */
{
	"???", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
	"Sep", "Oct", "Nov", "Dec"
};

sprdate(w, str)
struct when_f *w;
char *str;
{
	char *m;
	int h, i, j;	/* temps to print 0's or funny strings */

	m = "am";
	h = w->w_hours;
	if (h >= 12)
		m = "pm";
	if (h == 0)
		h = 12;
	if (h > 12)
		h -= 12;
	i = w->w_mins / 10;
	j = w->w_mins % 10;			/* get those leading zeroes */
	sprintf(str, "%2d:%d%d %2s  %3s %2d, %4d", h, i, j, m,
	    mnames[w->w_month], w->w_day, w->w_year);
}

prdate(zdate)
struct when_f *zdate;
{
	char line[DATELEN];

	sprdate(zdate, line);		/* have the formatter do stuff to it */
	printf("%s", line);		/* and print the thing */
}
