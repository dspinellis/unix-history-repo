/* popwrd.c - set password for a POP subscriber */
#ifndef	lint
static char ident[] = "@(#)$Id: popwrd.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/strings.h"
#include "../zotnet/bboards.h"
#include <errno.h>
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#ifdef	SYS5
#include <fcntl.h>
#endif	/* SYS5 */

#ifndef	__STDC__
#ifdef	SYS5
struct passwd *getpwnam();
#endif
#endif

static char  temp[] = "ptmp";
static char  home[BUFSIZ];

extern int errno;

char   *crypt (), *getpass (), *tail ();
long	time ();

#define	compar(s,t)	(strcmp (s, t) ? s : "")

/*  */

/* ARGSUSED */

main (argc, argv)
int	argc;
char  **argv;
{
    int     i,
            fd,
            flags,
            insist;
    long    salt;
    char    c,
           *cp,
            buffer[BUFSIZ],
            saltc[2],
          **ap;
    struct bboard  *bb,
                   *bp;
    struct passwd  *pw;
    FILE * fp;

    if ((pw = getpwnam (POPUID)) == NULL) {
	fprintf (stderr, "no entry for ~%s.\n", POPUID);
	exit (1);
    }
    if (chdir (strcpy (home, pw -> pw_dir)) < 0) {
	fprintf (stderr, "no home directory for ~%s.\n", POPUID);
	exit (1);
    }
    if (!setpwinfo (pw, POPDB, 0)) {
	fprintf (stderr, "setbbinfo(%s, %s, 0) failed -- %s.\n",
		pw -> pw_name, POPDB, getbberr ());
	exit (1);
    }

    if (argc != 2) {
	fprintf (stderr, "usage: %s pop-subscriber\n", *argv);
	exit (1);
    }
    if ((bb = getbbnam (argv[1])) == NULL
	    && (bb = getbbaka (argv[1])) == NULL) {
	fprintf (stderr, "Permission denied.\n");
	exit (1);
    }

    if (!ldrbb (bb) && getuid () && !ldrchk (bb))
	exit (1);

    if ((bp = getbbcpy (bb)) == NULL) {
	fprintf (stderr, "getbbcpy loses.\n");
	exit (1);
    }

    (void) endbbent ();

#ifdef	lint
    flags = 0;
#endif	/* lint */
    for (insist = 0; insist < 2; insist++) {
	if (insist)
	    printf ("Please use %s.\n",
		    flags == 1 ? "at least one non-numeric character"
		    : "a longer password");

	if ((i = strlen (strcpy (buffer, getpass ("New password:")))) == 0) {
	    fprintf (stderr, "Password unchanged.\n");
	    exit (1);
	}

	flags = 0;
	for (cp = buffer; c = *cp++;)
	    if (c >= 'a' && c <= 'z')
		flags |= 2;
	    else
		if (c >= 'A' && c <= 'Z')
		    flags |= 4;
		else
		    if (c >= '0' && c <= '9')
			flags |= 1;
		    else
			flags |= 8;

	if ((flags >= 7 && i >= 4)
		|| ((flags == 2 || flags == 4) && i >= 6)
		|| ((flags == 3 || flags == 5 || flags == 6) && i >= 5))
	    break;
    }

    if (strcmp (buffer, getpass ("Retype new password:"))) {
	fprintf (stderr, "Mismatch - password unchanged.\n");
	exit (1);
    }

    (void) time (&salt);
    salt ^= 9 * getpid ();
    saltc[0] = salt & 077;
    saltc[1] = (salt >> 6) & 077;
    for (i = 0; i < 2; i++) {
	c = saltc[i] + '.';
	if (c > '9')
	    c += 7;
	if (c > 'Z')
	    c += 6;
	saltc[i] = c;
    }
    cp = crypt (buffer, saltc);

    (void) signal (SIGHUP, SIG_IGN);
    (void) signal (SIGINT, SIG_IGN);
    (void) signal (SIGQUIT, SIG_IGN);

    (void) umask (0);
    if ((fd = open (temp, O_WRONLY | O_CREAT | O_EXCL, 0644)) < 0) {
	switch (errno) {
	    case EEXIST: 
		fprintf (stderr, "POP file busy - try again later.\n");
		break;

	    default: 
		perror (temp);
		break;
	}
	exit (1);
    }

#ifdef	SIGTSTP
    (void) signal (SIGTSTP, SIG_IGN);
#endif	/* SIGTSTP */
    if ((fp = fdopen (fd, "w")) == NULL) {
	fprintf (stderr, "fdopen loses.\n");
	(void) unlink (temp);
	exit (1);
    }

    (void) setbbent (SB_STAY | SB_FAST);
    while (bb = getbbent ()) {
	if (strcmp (bb -> bb_name, bp -> bb_name) == 0)
	    bb -> bb_passwd = cp;
	fprintf (fp, "%s:", bb -> bb_name);
	if (ap = bb -> bb_aka)
	    for (; *ap; ap++)
		fprintf (fp, "%s%s", ap != bb -> bb_aka ? "," : "", *ap);
	fprintf (fp, ":%s:%s:", tail (bb -> bb_file), bb -> bb_passwd);
	if ((ap = bb -> bb_leader) != NULL
		&& (strcmp (*ap, POPUID) != 0 || ap[1] != NULL))
	    for (; *ap; ap++)
		fprintf (fp, "%s%s", ap != bb -> bb_leader ? "," : "", *ap);
	fprintf (fp, ":%s:%s:%s:",
		compar (bb -> bb_addr, bb -> bb_name),
		compar (bb -> bb_request, POPUID),
		bb -> bb_relay);
	if (ap = bb -> bb_dist)
	    for (; *ap; ap++)
		fprintf (fp, "%s%s", ap != bb -> bb_dist ? "," : "", *ap);
	fprintf (fp, ":%o\n", bb -> bb_flags);
    }
    (void) endbbent ();

    if (rename (temp, POPDB) < 0) {
	perror ("rename");
	(void) unlink (temp);
	exit (1);
    }
    (void) fclose (fp);

    exit (0);
}

/*  */

char   *tail (s)
char   *s;
{
    int     i;
    char   *cp;

    if (strncmp (s, home, i = strlen (home)) == 0
	    && *(cp = s + i) == '/'
	    && *++cp)
	return cp;

    return s;
}
