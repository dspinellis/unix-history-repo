/* bbexp.c - expunge the BBoards area */
#ifndef	lint
static char ident[] = "@(#)$Id: bbexp.c,v 1.6 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/dropsbr.h"
#include "../zotnet/bboards.h"
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>


#define	FIRST	12
#define	SECOND	20


static int  broken_pipe;

static	int	move();
static		process(), chgrp();
int	pipeser ();


#ifndef	__STDC__
#ifdef	SYS5
struct passwd *getpwnam ();
#endif
#endif

/*  */

/* ARGSUSED */

main (argc, argv)
int	argc;
char  **argv;
{
    int	    first = FIRST,
	    second = SECOND;
    char   *cp;
    struct bboard  *bb;
    struct passwd  *pw;

    invo_name = r1bindex (*argv++, '/');
    m_foil (NULLCP);

    if ((pw = getpwnam (BBOARDS)) == NULL)
	adios (NULLCP, "no entry for ~%s", BBOARDS);
    if (pw -> pw_uid != geteuid ())
	adios (NULLCP, "not running setuid to %s", BBOARDS);

    if (*argv && **argv == '-') {
	if ((first = atoi (*argv + 1)) < 1)
	    first = FIRST;
	argv++;
    }
    if (*argv && **argv == '-') {
	if ((second = atoi (*argv + 1)) < 1)
	    second = SECOND;
	argv++;
    }
    
    (void) setbbent (SB_STAY);
    if (*argv == NULL)
	while (bb = getbbent ()) {
	    if ((bb -> bb_flags & BB_ARCH) & (BB_ASAV | BB_AREM))
		process (bb, pw, first, second);
	}
    else
	while (cp = *argv++)
	    if ((bb = getbbnam (cp)) || (bb = getbbaka (cp))) {
		if ((bb -> bb_flags & BB_ARCH) & (BB_ASAV | BB_AREM))
		    process (bb, pw, first, second);
	    }
	    else
		advise (NULLCP, "no such BBoard as %s", cp);
    (void) endbbent ();

    exit (0);
}

/*  */

static process (bb, pw, first, second)
struct bboard *bb;
struct passwd *pw;
int	first,
	second;
{
    int     fd,
            td;
    char   *cp,
            command[BUFSIZ],
            tmpfil[BUFSIZ];
    FILE   *pf;
    struct stat st;

    if ((fd = lkopen (bb -> bb_file, 6)) == NOTOK) {
	advise (bb -> bb_file, "unable to lock and open");
	return;
    }

    (void) sprintf (tmpfil, "%s/#bbexpXXXXXX", pw -> pw_dir);
    (void) unlink (mktemp (tmpfil));
    if ((td = creat (tmpfil, BBMODE)) == NOTOK) {
	advise (tmpfil, "unable to create");
	goto out1;
    }
    (void) close (td);
    if ((td = creat (cp = map_name (tmpfil), BBMODE)) == NOTOK) {
	advise (cp, "unable to create");
	goto out2;
    }
    (void) close (td);

    if ((bb -> bb_flags & BB_ARCH)
	    && stat (bb -> bb_archive, &st) == NOTOK
	    && stat (bb -> bb_file, &st) != NOTOK
	    && (td = creat (bb -> bb_archive, (int) (st.st_mode & 0777))) != NOTOK)
	(void) close (td);

    (void) sprintf (command, "%s %s%s", mshproc, bb -> bb_file,
	    isatty (fileno (stdout)) ? " 2>&1 | cat" : "");
    printf ("%s  (%s old messages)\n", command,
	    (bb -> bb_flags & BB_ARCH) == BB_ASAV ? "archive" : "remove");
    (void) fflush (stdout);
    if ((pf = popen (command, "w")) == NULL) {
	advise (NULLCP, "unable to popen \"%s\" for writing", command);
	goto out3;
    }
    (void) signal (SIGPIPE, pipeser);
    broken_pipe = 0;

    fprintf (pf, "pick %s -before -%d -sequence select -zero\n",
	    "-datefield BB-Posted", first);
    fprintf (pf, "pick -before -%d -sequence select -nozero\n", second);
    fprintf (pf, "scan select\n");
    if ((bb -> bb_flags & BB_ARCH) == BB_ASAV)
	fprintf (pf, "pack select -file %s\n", bb -> bb_archive);
    fprintf (pf, "rmm select\n");
    fprintf (pf, "packf all -file %s\n", tmpfil);
#ifdef	notdef			/* want real EOF to end it */
    fprintf (pf, "quit\n");
#endif	/* notdef */
    if (td = pclose (pf))
	advise (NULLCP, "msh returns %d", td);
    (void) signal (SIGPIPE, SIG_DFL);

    if (move (tmpfil, bb -> bb_file) != NOTOK)
	(void) move (cp, bb -> bb_map);

out3: ;
    (void) unlink (cp);
out2: ;
    (void) unlink (tmpfil);
out1: ;
    (void) lkclose (fd, bb -> bb_file);
}

/*  */

static  int move (input, output)
char   *input,
       *output;
{
    int     i,
            in,
            out;
    struct stat st1,
                st2;

    if ((in = open (input, 0)) == NOTOK) {
	advise (input, "unable to re-open");
	return NOTOK;
    }

    i = stat (output, &st1);
    if ((out = creat (output, BBMODE)) == NOTOK) {
	advise (output, "unable to re-create");
	return NOTOK;
    }
    if (i != NOTOK && chmod (output, (int) (st1.st_mode & 0777)) == NOTOK)
	admonish (output, "unable to change mode of");
    if (i != NOTOK && stat (output, &st2) != NOTOK && st2.st_gid != st1.st_gid)
	chgrp (output, st1.st_gid);

    cpydata (in, out, input, output);

    (void) close (in);
    (void) close (out);

    return OK;
}

/*  */

static  chgrp (file, gid)
char   *file;
short   gid;
{
    int     child_id;
    char    group[BUFSIZ];

    switch (child_id = fork ()) {
	case NOTOK: 
	    admonish ("fork", "unable to");
	    return;

	case OK: 
	    (void) setuid (geteuid ());
	    (void) sprintf (group, "%d", gid);
	    execlp ("/bin/chgrp", chgrp, group, file, NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror ("/bin/chgrp");
	    _exit (1);

	default: 
	    (void) pidwait (child_id, OK);
	    break;
    }
}

/*  */

/* ARGSUSED */

static  int pipeser (i)
int     i;
{
#ifndef	BSD42
    (void) signal (SIGPIPE, pipeser);
#endif	/* not BSD42 */

    if (!broken_pipe++)
	advise (NULLCP, "broken pipe");
}
