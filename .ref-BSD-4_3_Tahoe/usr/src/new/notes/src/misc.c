#include <errno.h>
#include <sys/param.h>
#include "parms.h"
#include "structs.h"
#include <signal.h>					/* signal processing */
#include <ctype.h>
#ifdef	SIGCHLD
#include <sys/wait.h>					/* for child status */
#endif	SIGCHLD

#ifdef	RCSIDENT
static char rcsid[] = "$Header: misc.c,v 1.7.0.5 85/10/06 01:41:01 notes Rel $";
#endif	RCSIDENT

#define		LOCKTRY		10			/* number of shots at grabbing */
#define		FORKTRY		10			/* tries at forking */

/*
 * dounix(charstring, flag) will execute that character string as a shell command
 * stolen from shell, though this one catches more signals.
 *
 *	Depending on the RUNSUID flag the routine sets things back to
 *	the users group or uid. Early versions were setuid and newer
 *	versions only run setgid.  Don't get confused by the "hisuid"
 *	argument: it really means "reset to his permissions".
 *	R. Kolstad -- 11/2/80
 *	modified: R. Essick January 1982, to clean up some signal processing
 *
 */

#if	defined(SIGCHLD)
static int  kidpid;					/* passed by kidwatch() */
static  union wait kidstatus;
#endif	defined(SIGCHLD)


#ifndef	FASTFORK
dounix (linebuf, hisuid, ttymode)
char    linebuf[];
#else
dounix (hisuid, ttymode, arg0, arg1, arg2, arg3, arg4)
char   *arg0,
       *arg1,
       *arg2,
       *arg3,
       *arg4;
#endif
{
    register    pid,
                forktry,
                rpid;
    int     (*p) (),
            (*q) (),
            (*r) ();
#if	defined(SIGTSTP)
    int     (*s) ();
#endif	defined(SIGTSTP)
#if	defined(SIGCHLD)
    int     (*t) ();
    extern int  watchkid ();				/* catch stopped kids */
#endif	defined(SIGCHLD)
    int     retcode;


    if (ttymode)
	ttystop ();					/* give back to normal mode */
    pid = 0;						/* satisfy init conditions */
    forktry = 0;					/* init the counter */
    while (pid <= 0 && ++forktry < FORKTRY)
    {
	if ((pid = fork ()) == 0)
	{
	    uncatchem ();				/* reset this process signals */
							/* if user can get his hands on it */
	    if (hisuid)					/* only set uid if giving shell */
#ifdef	RUNSUID
		setuid (globuid);			/* give him his uid */
#else
	    setgid (getgid ());				/* his group */
#endif	RUNSUID
	    for (rpid = 3; rpid < NOFILE; rpid++)	/* close extra files */
		close (rpid);

#ifndef	FASTFORK
	    if (linebuf == 0)
		execl (hisshell, hisshell, 0);
	    else
		execl (DFLTSH, "sh", "-c", linebuf, 0);
	    printf ("Rats -- Couldn't load %s\n", DFLTSH);
#else
	    if (arg0 == 0)
		execlp (hisshell, hisshell, 0);
	    else
		execlp (arg0, arg0, arg1, arg2, arg3, arg4);
	    printf ("Rats - Couldn't load %s\n", arg0);
#endif

	    _exit (BAD);				/* if exec fails .. */
	}
	if (pid <= 0)					/* if fork failed */
	    sleep (2);					/* wait a bit */
    }
    if (pid > 0)					/* only if have son */
    {
	p = signal (SIGHUP, SIG_IGN);
	q = signal (SIGINT, SIG_IGN);
	r = signal (SIGQUIT, SIG_IGN);
#if	defined(SIGTSTP)
	s = signal (SIGTSTP, SIG_DFL);
#endif	defined(SIGTSTP)
#if	defined(SIGCHLD)
	t = signal (SIGCHLD, watchkid);			/* if he signals */
#endif	defined(SIGCHLD)
	while ((rpid = wait (&retcode)) != pid && rpid != -1);
	if (rpid == -1)
	{
#if	defined(SIGCHLD)
/*
 *	watchkid() might have sucked down the status of the terminated
 *	child, so we load whatever value it left for us in kidstatus
 *	(provided that kidpid was ok)
 */
	    if (pid == kidpid)				/* the one we wanted */
		retcode = kidstatus.w_status;		/* from watchkid() */
	    else
		retcode = 1 << 8;			/* make an error */
#else
/*
 *	normal case, if the wait() failed for some reason we say that it
 *	is an error.
 */
	    retcode = 1 << 8;				/* indicates error  */
#endif	defined(SIGCHLD)
	}
	signal (SIGHUP, p);
	signal (SIGINT, q);
	signal (SIGQUIT, r);
#if	defined(SIGTSTP)
	signal (SIGTSTP, s);
#endif	defined(SIGTSTP)
#if	defined(SIGCHLD)
	signal (SIGCHLD, t);
#endif	defined(SIGCHLD)
    }
    else
	retcode = -1;					/* some sort of error */
    if (ttymode)
	ttystrt ();					/* back into raw mode */
    return retcode >> 8;				/* hand him the completion code */
}

#if	defined(SIGCHLD)
/*
 *	watchkid()
 *
 *	called when we receive a SIGCHLD signal, indicating that a child's
 *	status has changed.  This routine looks via wait3() to see if
 *	the children have merely been suspended.  If so, it stops itself
 *	so that it's parent can decide what to do.
 *
 *	This catches problems with programs like vi which run in raw mode
 *	and catch ^z as a character. They later try to signal the entire
 *	process group but are unable to signal the notes process since it
 *	has a different effective uid. By watching the SIGCHLD signal, we
 *	get notification when the vi process has stopped and we can stop
 *	ourselves. 
 *				Ray Essick,	Augst 22, 1984
 */
static int  watchkid (sig)
int     sig;
{

    kidpid = wait3 (&kidstatus, WUNTRACED | WNOHANG, 0);/* get status */
    if (kidpid == 0)					/* nothing to report */
	return 0;					/* get out */
    if (kidpid == (-1))					/* no children at all */
	return 0;					/* get out */
    if (WIFSTOPPED (kidstatus))				/* stopped himself */
    {
	kill (getpid (), SIGTSTP);			/* stop myself */
    }
}
#endif	defined(SIGCHLD)

/*
 *	If the condition is TRUE (non-zero) abort the program.
 *
 *	Print the supplied message and halt.
 *	Leave an optional core dump for debugging later.
 *
 *	Ray Essick 10/23/80
 */

x (cond, p) char   *p;
{
    if (cond == 0)
	return;						/* didnt fail */

    perror ("notes");
    fprintf (stderr, "Fatal Internal Notesfile Error: %s\n", p);
    ttystop ();						/* back to normal */


#ifdef	DUMPCORE
    if (chdir (Mstdir) < 0)				/* go to known place */
	exit (BAD);					/* drop out */
    if (chdir (UTILITY) < 0)
	exit (BAD);					/* drop out */
    if (chdir (DUMPCORE) < 0)				/* writeable to all */
	exit (BAD);					/* drop out */
#ifdef	RUNSUID
    setuid (globuid);					/* won't dump if euid != uid */
#else
    setgid (getgid ());					/* no gift groups */
#endif	RUNSUID
#endif	DUMPCORE

#ifdef	NFMAINT
/*
 *	This code is kind of risky.  If the NFMAINT notesfile ever
 *	gets trashed and starts calling this routine, look out because
 *	it will recursively fail. This is the unfortunate byproduct
 *	of the fact that the "x" routine doesn't know what the 
 *	current notesfile is.
 */
    {
	char    pbuf[512];				/* hold message */
	char    pbuf2[128];				/* and title */
	char    pbuf3[256];				/* core image */
	char   *tail;					/* end of invocation */

	sprintf (pbuf2, "%s: aborted", Invokedas);
	sprintf (pbuf, "Program:\t%s\nMessage:\t%s\n\nerrno:\t\t%d  (%s)\n",
		Invokedas, p, errno,
		errno >= sys_nerr ? "Unknown error code" : sys_errlist[errno]);
#ifdef	DUMPCORE
	if ((tail = rindex (Invokedas, '/')) == NULL)	/* pathname? */
	    tail = Invokedas;				/* simple invocation */
	else
	    tail++;					/* strip the slash */
	sprintf (pbuf3, "%s/%s/%s/%s", Mstdir, UTILITY, DUMPCORE, tail);
	nfabort (NFMAINT, pbuf, pbuf2, pbuf3, BAD);	/* log & abort */
#else	! DUMPCORE
	nfcomment (NFMAINT, pbuf, pbuf2, 0, 0);		/* actual insertion */
#endif	DUMPCORE
    }
#endif	NFMAINT

/*
 *	Handle the exit if NFMAINT is undefined.
 */

#ifdef	DUMPCORE
    abort ();						/* dump in "core" */
#else
    exit (BAD);						/* for production */
#endif	DUMPCORE
}

/*
 *	lock creates a lock file, or waits until it can create the lock.
 *	lock files are of the form lock#  where # is a character passed
 *	to the routine.
 *
 *	Rob Kolstad	10/20/80
 *	modified: rbe December 1981 to add full path name for lock file 
 */

locknf (io, c)
struct io_f *io;
char    c;
{
    register int    i,
                    holderr,
                    trys;
    char    p[WDLEN];

    sprintf (p, "%s/%s/%c%s", Mstdir, LOCKS, c, io -> nf);
							/* generate file name */
    trys = LOCKTRY;					/* set him up */
    while ((i = creat (p, 0)) < 0)
    {
	if (trys-- == 0)
	{
	    holderr = errno;				/* before it's abused */
	    fprintf (stderr, "lock %c (%s) permanently locked - consult a guru\n",
		    c, io -> nf);
#ifdef	NFMAINT
	    if (strcmp (NFMAINT, io -> nf))		/* avoid loops */
	    {
		char    pbuf[256];			/* for error logging */
		char    tbuf[256];			/* title */
		sprintf (pbuf,
			"lock %c failed for %s,\nerrno = %d (%s)\nProgram = %s\n",
			c, io -> fullname, holderr, sys_errlist[holderr],
			Invokedas);
		sprintf (tbuf, "%s: locked (%c)", io -> nf, c);
		nfcomment (NFMAINT, pbuf, tbuf, 0, 0);
	    }
#endif	NFMAINT
	    ttystop ();
	    exit (BAD);
	}
	sleep (2);					/* guarantee at least 1 */
    }
    ignoresigs++;					/* critical section */
/*
 *	could be above getting the lock, but wanted to be able to suspend
 *	while getting the lock.  The interuptable window is very small
 */
    close (i);
}

/*
 *	unlock takes the same arguements as the lock routine, and it
 *	will remove the corresponding lock file
 *
 *	Rob Kolstad 10/20/80
 *	modified: rbe December 1981 to add full path name for lock name
 */

unlocknf (io, c)
struct io_f *io;
char    c;
{
    char    p[WDLEN];

    sprintf (p, "%s/%s/%c%s", Mstdir, LOCKS, c, io -> nf);
							/* generate file name */
    x (unlink (p) < 0, "unlock: unlink lock");
    ignoresigs--;					/* no longer critical */
}

/*
 *	glock creates a lock file, or waits until it can create the lock.
 *	lock files are of the form lock#  where # is a character passed
 *	to the routine.
 *		This lock file is a GLOBAL lock - across all notefiles
 *
 *	taken from lock routine above by R. Essick December 1981
 */

glocknf (io, c)
struct io_f *io;					/* unused in this routine */
char    c;
{
    register int    i,
                    holderr,
                    trys;
    char    p[WDLEN];

    sprintf (p, "%s/%s/%c", Mstdir, LOCKS, c);		/* generate file name */
    trys = LOCKTRY;
    while ((i = creat (p, 0)) < 0)
    {
	if (trys-- == 0)
	{
	    holderr = errno;				/* before it's abused */
	    fprintf (stderr, "lock%c combo lost - consult your local guru\n", c);
#ifdef	NFMAINT
	    if (strcmp (NFMAINT, io -> nf))		/* don't loop on self */
	    {
		char    pbuf[256];			/* for error logging */
		char    pbuf2[256];
		sprintf (pbuf,
			"glock %c failed for %s, errno = %d (%s)\nProgram = %s\n",
			c, io -> fullname, holderr, sys_errlist[holderr],
			Invokedas);
		sprintf (pbuf2, "Frozen Global Lock (%c)", c);
		nfcomment (NFMAINT, pbuf, pbuf2, 0, 0);
	    }
#endif	NFMAINT
	    ttystop ();
	    exit (BAD);
	}
	sleep (2);					/* is there a smaller time interval */
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

gunlocknf (io, c)
struct io_f *io;					/* not used by this routine */
char    c;
{
    char    p[WDLEN];

    sprintf (p, "%s/%s/%c", Mstdir, LOCKS, c);		/* make the file name */
    x (unlink (p) < 0, "gunlock: unlink lock");
}

/*
 * length tells us max(length of string, 1)
 */
len (p, n) char *p;
{
    int     i;
    i = n;
    p += n;
    while (*--p == ' ' && --i)
	if (i == 0)
	    i = 1;
    return i;
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

gshell ()
{
    printf ("\n");
#ifndef	FASTFORK
    dounix (0, 1, 1);					/* give him his shell */
#else
    dounix (1, 1, 0, 0, 0, 0, 0);
#endif
    return 0;
}

/*	copydate merely moves a when_f structure from 'from' to 'to' */
/*	ray essick - 20-nov-1981	*/

copydate (from, to)
struct when_f  *from,
               *to;
{
    *to = *from;					/* use block move */
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

    count = 0;						/* start with no characters moved */
    pp = p;
    qq = q;						/* use registers for speed */
    while (*qq++ = *pp++)
	count++;
    return count;					/* return count of characters moved */
							/* don't include the terminator */
}

/*	copyauth(from, to) struct auth_f *from,*auth
 *	copys author from from to to
 *	Ray Essick December 1981
 *
 *	SHOULD USE STRUCTURE ASSIGNMENT IN ALL PLACES THAT CALL THIS
 */
copyauth (from, to)
struct auth_f  *from,
               *to;
{

    strncpy (to -> aname, from -> aname, NAMESZ);	/* author name */
    strncpy (to -> asystem, from -> asystem, HOMESYSSZ);/* home machine */
    to -> aid = from -> aid;				/* and user id */
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
    if ((buf[*ptr] < '0' || buf[*ptr] > '9') && buf[*ptr] != ' ')
    {
	return 0;					/* end of this list */
    }
    *start = listconv (buf, ptr);			/* get the first */
    *finish = *start;					/* default to single */
    if (buf[*ptr] == '-')
    {
	++(*ptr);					/* trash that separator */
	*finish = listconv (buf, ptr);			/* grab second */
	++(*ptr);					/* bump past delimiter */
	return 2;					/* parsed 2 arguements */
    }
    else
    {
	if (buf[*ptr] != '\0')
	    ++(*ptr);					/* dump delimiter */
	return 1;
    }
}

listconv (buf, ptr)
char    buf[];
int    *ptr;
{
    int     i;
    i = 0;
    while (buf[*ptr] == ' ')
	++(*ptr);
    while (buf[*ptr] >= '0' && buf[*ptr] <= '9')
    {
	i = 10 * i + buf[*ptr] - '0';
	++(*ptr);					/* bump him */
    }
    return (i);
}

/*	tolcase - check to see if upper case, and convert to lcase */
/*	R. Essick	Feb 1982 */
tolcase (c)
char    c;
{
    if (isascii (c) && isupper (c))
	return (tolower (c));				/* to lower case */
    return (c);						/* leave as is */
}

/*
 *	Date printing stuff.
 *
 *	CHANGE TO CTIME(III) FORMAT EVENTUALLY
 */

char   *mnames[13] =					/* so indexes work right */
{
    "???", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
    "Sep", "Oct", "Nov", "Dec"
};

sprdate (w, str) struct when_f *w;
char   *str;
{
    char   *m;
    int     h,
            i,
            j;						/* temps to print 0's or funny strings */
    m = "am";
    h = w -> w_hours;
    if (h >= 12)
	m = "pm";
    if (h == 0)
	h = 12;
    if (h > 12)
	h -= 12;
    i = w -> w_mins / 10;
    j = w -> w_mins % 10;				/* get those leading zeroes */
    sprintf (str, "%2d:%d%d %2s  %3s %2d, %4d", h, i, j, m, mnames[w -> w_month], w -> w_day, w -> w_year);
							/* sprintf puts it into a string */
}

prdate (zdate) struct when_f   *zdate;
{
    char    line[DATELEN];

    sprdate (zdate, line);				/* format it */
    printf ("%s", line);				/* and print it */
}

/*
 *	Saves a string with malloc() and returns a pointer
 *	to where it wound up.  Useful for building lists of
 *	stuff.
 *
 *	Courtesy of Lou Salkind & Rick Spickelmier.
 */


char   *strsave (s)
char   *s;
{
    char   *p;
    extern char *malloc ();

    p = malloc (strlen (s) + 1);
    strcpy (p, s);
    return (p);
}

/*
 *	substr(a,b)	see if A is a substring of B
 *
 *	uses: strlen.
 */

substr (a, b)
char   *a;
char   *b;
{
    register char   first;
    register int    length;				/* length of a */
    register int    count;				/* max checks */

    first = *a;						/* get first */
    length = strlen (a);				/* for strncmp */
    count = strlen (b) - length + 1;			/* max checks */
    while (count-- > 0)					/* can try */
    {
	if (*b == first && !strncmp (a, b, length))
	    return (1);					/* is a substring */
	b++;						/* on to next */
    }
    return (0);						/* not a substring */
}

/*
 *	routine to process a string and remove any
 *	nasties like control characters and escape codes.
 */

int     strclean (p)
char   *p;
{
    if (p == (char *) NULL)
	return 0;
    if (*p == '\0')
	return 0;
    do
    {
	if (!isascii (*p) || iscntrl (*p))
	    *p = '_';					/* kill controls */
    } while (*++p != '\0');
    return (0);
}
