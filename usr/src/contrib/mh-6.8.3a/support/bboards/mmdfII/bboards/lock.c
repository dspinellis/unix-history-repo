/* lock.c - universal locking routines */
#ifndef	lint
static char ident[] = "@(#)$Id: lock.c,v 2.19 1993/08/25 17:33:09 jromine Exp $";
#endif
/* compile-time priority:
 *	LOCKF	use if defined
 *	FCNTL	use if SYS5  defined and LOCKF not defined
 *	FLOCK	use if BSD42 defined and LOCKF and SYS5 not defined
 */

#ifdef	MMDFONLY
#define	LOCKONLY
#endif

#include "../h/mh.h"
#include <stdio.h>
#ifndef	LOCKONLY
#include "../h/strings.h"
#include "mts.h"
#else	/* LOCKONLY */
#include "strings.h"
#ifdef	MMDFONLY
#include "mmdfonly.h"
#include "mts.h"
#else	/* not MMDFONLY */
#include "lockonly.h"
#endif	/* not MMDFONLY */
#endif	/* LOCKONLY */
#include <sys/types.h>
#include <sys/stat.h>
#ifdef SVR4
#define LOCKF
#include <unistd.h>
#endif
#ifdef	LOCKF
#include <sys/errno.h>
#include <sys/file.h>
#ifndef	F_ULOCK
#ifdef	UNISTD
#include <unistd.h>
#else	/* UNISTD */
#include <sys/fcntl.h>
#endif	/* UNISTD */
#endif
#endif	/* LOCKF */
#if defined(_AIX) || defined(AUX)
#include <sys/file.h>
#endif

#ifdef	SYS5
#define	u_short	ushort
#define u_long  ulong
#endif


#if defined(SYS5) && !defined(_AIX)
#define	index	strchr
#define	rindex	strrchr
#endif
#ifdef	BSD42
#define	FLOCK		/* LOCKF will override this, if defined */
#endif

extern int  errno;

#ifdef	LOCKONLY
#ifndef	MMDFONLY
char   *lockldir = "/usr/spool/locks";
#endif	/* not MMDFONLY */
#endif	/* LOCKONLY */

static int	b_lkopen(), lockit(), f_lkopen();
static		lockname(), timerON(), timerOFF();

long	time ();

/*  */

int	lkopen (file, access)
register char   *file;
register int     access;
{
    mts_init ("mts");
    switch (lockstyle) {
	case LOK_UNIX:
#if	defined (FLOCK) || defined(LOCKF) || defined(FCNTL)
	    return f_lkopen (file, access);
#endif

	default:
	    return b_lkopen (file, access);
	}
}

/*  */

static int  b_lkopen (file, access)
register char   *file;
register int     access;
{
    register int    i,
                    j;
    long    curtime;
    char    curlock[BUFSIZ],
            tmplock[BUFSIZ];
    struct stat st;

    if (stat (file, &st) == NOTOK)
	return NOTOK;
    lockname (curlock, tmplock, file, (int) st.st_dev, (int) st.st_ino);

    for (i = 0;;)
	switch (lockit (tmplock, curlock)) {
	    case OK: 
		if ((i = open (file, access)) == NOTOK) {
		    j = errno;
		    (void) unlink (curlock);
		    errno = j;
		}
		timerON (curlock, i);
		return i;

	    case NOTOK: 
		if (stat (curlock, &st) == NOTOK) {
		    if (i++ > 5)
			return NOTOK;
		    sleep (5);
		    break;
		}

		i = 0;
		(void) time (&curtime);
		if (curtime < st.st_ctime + 60L)
		    sleep (5);
		else
		    (void) unlink (curlock);
		break;
	}
}


static int  lockit (tmp, file)
register char   *tmp,
	        *file;
{
    register int    fd;

    if ((fd = creat (tmp, 0400)) == NOTOK)
	return NOTOK;
#if defined(hpux) || defined(ncr)
    write(fd, "MH lock\n",8);
#endif /* hpux */
    (void) close (fd);

    fd = link (tmp, file);
    (void) unlink (tmp);

    return (fd != NOTOK ? OK : NOTOK);
}

/*  */

static  lockname (curlock, tmplock, file, dev, ino)
register char   *curlock,
	        *tmplock,
	        *file;
register int     dev,
		 ino;
{
    register char  *bp,
                   *cp;

    bp = curlock;
    if ((cp = rindex (file, '/')) == NULL || *++cp == 0)
	cp = file;
    if (lockldir == NULL || *lockldir == 0) {
	if (cp != file) {
	    (void) sprintf (bp, "%.*s", cp - file, file);
	    bp += strlen (bp);
	}
    }
    else {
	(void) sprintf (bp, "%s/", lockldir);
	bp += strlen (bp);
    }

    switch (lockstyle) {
	case LOK_BELL: 
	default: 
	    (void) sprintf (bp, "%s.lock", cp);
	    break;

	case LOK_MMDF: 
	    (void) sprintf (bp, "LCK%05d.%05d", dev, ino);
	    break;
    }

    if (tmplock) {
	if ((cp = rindex (curlock, '/')) == NULL || *++cp == 0)
	    (void) strcpy (tmplock, ",LCK.XXXXXX");
	else
	    (void) sprintf (tmplock, "%.*s,LCK.XXXXXX",
		cp - curlock, curlock);
	(void) unlink (mktemp (tmplock));
    }
}

/*  */

#if	defined(FLOCK) || defined(LOCKF) || defined(FCNTL)

#if	defined(BSD42) || defined(SVR4)
#include <sys/file.h>
#if	defined(SUN40) || defined(SVR4)
#include <sys/fcntl.h>
#endif
#else 
#ifdef	FCNTL
#include <fcntl.h>
#endif
#endif

static int  f_lkopen (file, access)
register char   *file;
register int     access;
{
    register int    fd,
                    i,
		    j;
#ifdef FCNTL
    struct flock    buf;
#endif /* FCNTL */

    for (i = 0; i < 5; i++) {
#if defined(LOCKF) || defined(FCNTL)
	j = access;
	access &= ~O_APPEND;	/* make sure we open at the beginning */
	if ((access & 03) == O_RDONLY) {
	/* We MUST have write permission or lockf/fcntl() won't work */
	/* (Stupid eh?) */
	    access &= ~O_RDONLY;
	    access |= O_RDWR;
	}
#endif	/* LOCKF || FCNTL */
	if ((fd = open (file, access | O_NDELAY)) == NOTOK)
	    return NOTOK;
#ifndef	LOCKF
#ifndef	FLOCK
#ifndef	FCNTL
	/* should be an error? */
#else /* FCNTL */
	buf.l_type = F_WRLCK;
	buf.l_whence = 0;
	buf.l_start = 0;
	buf.l_len = 0;
	if (fcntl (fd, F_SETLK, &buf) != NOTOK)
	    return fd;
#endif
#else /* FLOCK */
	if (flock (fd, LOCK_EX | LOCK_NB) != NOTOK)
	    return fd;
#endif
#else /* LOCKF */
	if (lockf (fd, F_TLOCK, 0L) != NOTOK) {
	    /* see if we should be at the end */
	    if (j & O_APPEND)
#ifdef SVR4
		lseek (fd, (off_t)0, SEEK_END);
#else
		lseek (fd, (off_t)0, L_XTND);
#endif
	    return fd;
	}
	/* Fix errno - lockf screws it */
	if (errno == EACCES)
	    errno = EWOULDBLOCK;
#endif
	j = errno;
	(void) close (fd);

	sleep (5);
    }

    (void) close (fd);
    errno = j;
    return NOTOK;
}
#endif	/* FLOCK || LOCKF || FCNTL */

/*  */

/* ARGSUSED */

int     lkclose (fd, file)
register int     fd;
register char   *file;
{
    char    curlock[BUFSIZ];
    struct stat st;
#ifdef FCNTL
    struct flock buf;
#endif

    if (fd == NOTOK)
	return OK;
    switch (lockstyle) {
	case LOK_UNIX: 
#ifndef	LOCKF
#ifndef	FLOCK
#ifndef	FCNTL
	/* should be an error? */
#else	/* FCNTL */
	    buf.l_type = F_UNLCK;
	    buf.l_whence = 0;
	    buf.l_start = 0;
	    buf.l_len = 0;
	    fcntl(fd, F_SETLK, &buf);
	    break;
#endif
#else	/* FLOCK */
	    flock (fd, LOCK_UN);
	    break;
#endif
#else	/* LOCKF */
	    lseek (fd, (off_t)0, L_SET); /* make sure we unlock the whole thing */
	    lockf (fd, F_ULOCK, 0L);
	    break;
#endif	

	default: 
	    if (fstat (fd, &st) != NOTOK) {
		lockname (curlock, NULLCP, file, (int) st.st_dev, (int) st.st_ino);
		(void) unlink (curlock);
		timerOFF (fd);
	    }
    }

    return (close (fd));
}


/*  */

FILE	*lkfopen (file, mode)
register char   *file,
 	        *mode;
{
    register int    fd;
    register FILE  *fp;

    if ((fd = lkopen (file, strcmp (mode, "r") ? 2 : 0)) == NOTOK)
	return NULL;

    if ((fp = fdopen (fd, mode)) == NULL) {
	(void) close (fd);
	return NULL;
    }

    return fp;
}


/* ARGSUSED */

int	lkfclose (fp, file)
register FILE	*fp;
register char	*file;
{
    char    curlock[BUFSIZ];
    struct stat st;
#ifdef FCNTL
    struct flock buf;
#endif

    if (fp == NULL)
	return OK;

    switch (lockstyle) {
	case LOK_UNIX: 
#ifndef	LOCKF
#ifndef	FLOCK
#ifndef	FCNTL
	/* should be an error? */
#else	/* FCNTL */
	    buf.l_type = F_UNLCK;
	    buf.l_whence = 0;
	    buf.l_start = 0;
	    buf.l_len = 0;
	    fcntl(fileno(fp), F_SETLK, &buf);
	    break;
#endif
#else /* FLOCK */
	    flock (fileno(fp), LOCK_UN);
	    break;
#endif
#else	/* LOCKF */
	    fseek (fp, 0L, 0); /* make sure we unlock the whole thing */
	    lockf (fileno(fp), F_ULOCK, 0L);
	    break;
#endif

	default: 
	    if (fstat (fileno (fp), &st) != NOTOK) {
		lockname (curlock, NULLCP, file, (int) st.st_dev, (int) st.st_ino);
		(void) unlink (curlock);
	    }
    }

    return (fclose (fp));
}

/*  */

#include <signal.h>

#define	NSECS	((unsigned) 20)


struct lock {
    int		 l_fd;
    char	*l_lock;
    struct lock *l_next;
};
#define	NULLP	((struct lock *) 0)

static struct lock *l_top = NULLP;


/* ARGSUSED */

static TYPESIG alrmser (sig)
int	sig;
{
    register int    j;
    register char  *cp;
    register struct lock   *lp;

#ifndef	BSD42
    (void) signal (SIGALRM, alrmser);
#endif	/* BSD42 */

    for (lp = l_top; lp; lp = lp -> l_next)
	if (*(cp = lp -> l_lock) && (j = creat (cp, 0400)) != NOTOK)
	    (void) close (j);

    (void) alarm (NSECS);
}

/*  */

static timerON (lock, fd)
char   *lock;
int	fd;
{
    register struct lock   *lp;

    if ((lp = (struct lock *) malloc ((unsigned) (sizeof *lp))) == NULLP)
	return;			/* XXX */

    lp -> l_fd = fd;
    if ((lp -> l_lock = malloc ((unsigned) (strlen (lock) + 1))) == NULLCP) {
	free ((char *) lp);
	return;			/* XXX */
    }
    (void) strcpy (lp -> l_lock, lock);
    lp -> l_next = NULLP;

    if (l_top)
	lp -> l_next = l_top -> l_next;
    else {
	(void) signal (SIGALRM, alrmser);/* perhaps SIGT{STP,TIN,TOU} */
	(void) alarm (NSECS);
    }
    l_top = lp;
}


static timerOFF (fd)
int	fd;
{
    register struct lock   *pp,
                           *lp;

    (void) alarm (0);

    if (l_top) {
	for (pp = lp = l_top; lp; pp = lp, lp = lp -> l_next)
	    if (lp -> l_fd == fd)
		break;
	if (lp) {
	    if (lp == l_top)
		l_top = lp -> l_next;
	    else
		pp -> l_next = lp -> l_next;

	    free (lp -> l_lock);
	    free ((char *) lp);
	}
    }

    if (l_top)
	(void) alarm (NSECS);
}
