/* getpassword.c - generic read-the-password-from-the-tty */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/compat/RCS/getpassword.c,v 7.2 91/02/22 09:15:11 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/compat/RCS/getpassword.c,v 7.2 91/02/22 09:15:11 mrose Interim $
 *
 *
 * $Log:	getpassword.c,v $
 * Revision 7.2  91/02/22  09:15:11  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/21  11:29:39  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  21:23:01  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <signal.h>
#include <stdio.h>
#include "general.h"
#include "manifest.h"
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#include <termio.h>
#endif
#include <sys/ioctl.h>


#ifdef	BSD44
char   *getpass ();
#endif

/*  */

/* roll our own since want to get past UNIX's limit of 8 octets... */

char *getpassword (prompt)
char *prompt;
{
#ifndef	BSD44
    register int    c;
    int	    flags,
	    isopen;
    register char  *bp,
		   *ep;
#if	!defined(SYS5) && !defined(XOS_2)
    struct sgttyb   sg;
#else
    struct termio   sg;
#endif
    SFP	    istat;
    FILE    *fp;
    static char buffer[BUFSIZ];

#ifdef SUNLINK_7_0
    fp = stdin, isopen = 0;	/* will help greatly to work off a script */
#else
    if ((c = open ("/dev/tty", O_RDWR)) != NOTOK && (fp = fdopen (c, "r")))
	setbuf (fp, NULLCP), isopen = 1;
    else {
	if (c != NOTOK)
	    (void) close (c);

	fp = stdin, isopen = 0;
    }
#endif

    istat = signal (SIGINT, SIG_IGN);

#if	!defined(SYS5) && !defined(XOS_2)
    (void) gtty (fileno (fp), &sg);
    flags = sg.sg_flags;
    sg.sg_flags &= ~ECHO;
    (void) stty (fileno (fp), &sg);
#else
    (void) ioctl (fileno (fp), TCGETA, (char *) &sg);
    flags = sg.c_lflag;
    sg.c_lflag &= ~ECHO;
    (void) ioctl (fileno (fp), TCSETAW, (char *) &sg);
#endif

#ifdef SUNLINK_7_0
    fprintf (stdout, "%s", prompt);
    (void) fflush (stdout);
#else
    fprintf (stderr, "%s", prompt);
    (void) fflush (stderr);
#endif

    for (ep = (bp = buffer) + sizeof buffer - 1; (c = getc (fp)) != EOF;)
#ifndef	apollo
	if (c == '\n')
#else
	if (c == '\n' || c == '\r')
#endif
	    break;
	else
	    if (bp < ep)
		*bp++ = c;
    *bp = NULL;

#ifdef SUNLINK_7_0
    fprintf (stdout, "\n");
    (void) fflush (stdout);
#else
    fprintf (stderr, "\n");
    (void) fflush (stderr);
#endif

#if	!defined(SYS5) && !defined(XOS_2)
    sg.sg_flags = flags;
    (void) stty (fileno (fp), &sg);
#else
    sg.c_lflag = flags;
    (void) ioctl (fileno (fp), TCSETAW, (char *) &sg);
#endif

    (void) signal (SIGINT, istat);

    if (isopen)
	(void) fclose (fp);

    return buffer;
#else
    return getpass (prompt);
#endif
}
