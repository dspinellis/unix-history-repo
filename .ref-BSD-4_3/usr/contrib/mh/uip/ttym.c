/* ttym.c - miscellaneous routines */

#include <pwd.h>
#ifndef	NSIG
#include <signal.h>
#endif	NSIG
#include <sys/time.h>

/*  */

static  ttym (fd, command, line, user, vec)
int     fd;
char   *command,
       *line,
       *user,
      **vec;
{
    int     (*pstat) ();
    char   *ap,
           *term,
           *myself,
           *getlogin (),
           *rindex (),
           *ttyname ();
    struct passwd  *pw,
                   *getpwuid ();

    if ((term = ap = ttyname (2)) && (term = rindex (term, '/')))
	term++;
    if (term == NULL || *term == NULL)
	term = ap;
    if ((myself = getlogin ()) == NULL || *myself == NULL)
	myself = (pw = getpwuid (getuid ())) ? pw -> pw_name : NULL;

    pstat = signal (SIGPIPE, SIG_IGN);
    (void) write (fd, command, strlen (command));
    (void) write (fd, "", 1);

    if (term)
	(void) write (fd, term, strlen (term));
    (void) write (fd, "", 1);

    if (myself)
	(void) write (fd, myself, strlen (myself));
    (void) write (fd, "", 1);

    if (line && *line)
	(void) write (fd, line, strlen (line));
    (void) write (fd, "", 1);

    if (user && *user)
	(void) write (fd, user, strlen (user));
    (void) write (fd, "", 1);

    if (vec)
	while (ap = *vec++) {
	    (void) write (fd, ap, strlen (ap));
	    (void) write (fd, "", 1);
	}

    (void) write (fd, "", 1);
    (void) signal (SIGPIPE, pstat);
}

/*  */

static int  ttyv (fd)
int     fd;
{
    int     ifds,
	    nbits;
    char    c;
    struct timeval tv;

    ifds = 1 << fd;
    nbits = getdtablesize();
    tv.tv_sec = SMLWAIT;
    tv.tv_usec = 0;
    if (select (nbits, &ifds, (int *) 0, (int *) 0, &tv) <= 0
	    || read (fd, &c, 1) != 1)
	return NOTOK;
    if (c == NULL)
	return fd;
    putc (c, stderr);

    (void) ttyf (fd, stderr);
    return NOTOK;
}


static int  ttyf (fd, f)
int     fd;
FILE * f;
{
    int     i;
    char    buffer[BUFSIZ];

    while ((i = read (fd, buffer, sizeof buffer)) > 0)
	(void) fwrite (buffer, sizeof (char), i, f);
    return i;
}
