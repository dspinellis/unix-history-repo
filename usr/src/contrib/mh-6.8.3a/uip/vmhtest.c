/* vmhtest.c - test out vmh protocol */
#ifndef	lint
static char ident[] = "@(#)$Id: vmhtest.c,v 1.2 1992/11/04 01:06:43 jromine Exp $";
#endif	lint

#include "../h/mh.h"
#include "../h/vmhsbr.h"
#include <ctype.h>
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	READSW	0
    "vmhread fd", 7,	
#define	WRITESW	1
    "vmhwrite fd", 8,	

#define	HELPSW	2
    "help", 4,		

    NULL, NULL
};

/*  */

#define	NWIN	20
static	int	numwins = 0;
static	int	windows[NWIN + 1];


static	int	selcmds = 0;
#define	selcmd()	(selcmds++ % 2)

static	int	selwins = 0;
#define	selwin()	(selwins++ % 2 ? 3 : 1)

/*  */

main (argc, argv, envp)
int     argc;
char  **argv,
      **envp;
{
    int     fd1,
            fd2;
    char   *cp,
            buffer[BUFSIZ],
          **ap,
          **argp = argv + 1,
           *arguments[MAXARGS];

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    m_foil (NULLCP);

/*  */

    while (cp = *argp++)
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buffer, "%s [switches]", invo_name);
		    help (buffer, switches);
		    done (1);

		case READSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((fd1 = atoi (cp)) < 1)
			adios (NULLCP, "bad argument %s %s", argp[-2], cp);
		    continue;
		case WRITESW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((fd2 = atoi (cp)) < 1)
			adios (NULLCP, "bad argument %s %s", argp[-2], cp);
		    continue;
	    }
	else
	    adios (NULLCP, "usage: %s [switches]", invo_name);

/*  */

    (void) rcinit (fd1, fd2);
    (void) pINI ();
    (void) pLOOP ();

    done (0);
}

/*  */

static int  pINI () {
    int     i,
            vrsn;
    char   *bp;
    struct record   rcs,
                   *rc = &rcs;

    initrc (rc);

    switch (peer2rc (rc)) {
	case RC_INI: 
	    bp = rc -> rc_data;
	    while (isspace (*bp))
		bp++;
	    if (sscanf (bp, "%d", &vrsn) != 1) {
	bad_init: ;
		(void) fmt2peer (RC_ERR, "bad init \"%s\"", rc -> rc_data);
		done (1);
	    }
	    if (vrsn != RC_VRSN) {
		(void) fmt2peer (RC_ERR, "version %d unsupported", vrsn);
		done (1);
	    }

	    while (*bp && !isspace (*bp))
		bp++;
	    while (isspace (*bp))
		bp++;
	    if (sscanf (bp, "%d", &numwins) != 1 || numwins <= 0)
		goto bad_init;
	    if (numwins > NWIN)
		numwins = NWIN;

	    for (i = 1; i <= numwins; i++) {
		while (*bp && !isspace (*bp))
		    bp++;
		while (isspace (*bp))
		    bp++;
		if (sscanf (bp, "%d", &windows[i]) != 1 || windows[i] <= 0)
		    goto bad_init;
	    }
	    (void) rc2peer (RC_ACK, 0, NULLCP);
	    return OK;

	case RC_XXX: 
	    adios (NULLCP, "%s", rc -> rc_data);

	default: 
	    (void) fmt2peer (RC_ERR, "pINI protocol screw-up");
	    done (1);		/* NOTREACHED */
    }
}

/*  */

static int  pLOOP () {
    struct record   rcs,
                   *rc = &rcs;

    initrc (rc);

    for (;;)
	switch (peer2rc (rc)) {
	    case RC_QRY: 
		(void) pQRY (rc -> rc_data);
		break;

	    case RC_CMD: 
		(void) pCMD (rc -> rc_data);
		break;

	    case RC_FIN: 
		done (0);

	    case RC_XXX: 
		adios (NULLCP, "%s", rc -> rc_data);

	    default: 
		(void) fmt2peer (RC_ERR, "pLOOP protocol screw-up");
		done (1);
	}
}

/*  */

static int  pQRY (str)
char   *str;
{
    (void) rc2peer (RC_EOF, 0, NULLCP);
    return OK;
}

/*  */

static int  pCMD (str)
char   *str;
{
    if ((selcmd () ? pTTY (str) : pWIN (str)) == NOTOK)
	return NOTOK;
    (void) rc2peer (RC_EOF, 0, NULLCP);
    return OK;
}

/*  */

static int  pTTY (str)
char   *str;
{
    struct record   rcs,
                   *rc = &rcs;

    initrc (rc);

    switch (rc2rc (RC_TTY, 0, NULLCP, rc)) {
	case RC_ACK: 
	    break;

	case RC_ERR: 
	    return NOTOK;

	case RC_XXX: 
	    adios (NULLCP, "%s", rc -> rc_data);

	default: 
	    (void) fmt2peer (RC_ERR, "pTTY protocol screw-up");
	    done (1);
    }

    system (str);

    switch (rc2rc (RC_EOF, 0, NULLCP, rc)) {
	case RC_ACK: 
	    return OK;

	case RC_XXX: 
	    adios (NULLCP, "%s", rc -> rc_data);/* NOTREACHED */

	default: 
	    (void) fmt2peer (RC_ERR, "pTTY protocol screw-up");
	    done (1);		/* NOTREACHED */
    }
}

/*  */

static int  pWIN (str)
char   *str;
{
    int     i,
            pid,
            pd[2];
    char    buffer[BUFSIZ];
    struct record   rcs,
                   *rc = &rcs;

    initrc (rc);

    (void) sprintf (buffer, "%d", selwin ());
    switch (str2rc (RC_WIN, buffer, rc)) {
	case RC_ACK: 
	    break;

	case RC_ERR: 
	    return NOTOK;

	case RC_XXX: 
	    adios (NULLCP, "%s", rc -> rc_data);

	default: 
	    (void) fmt2peer (RC_ERR, "pWIN protocol screw-up");
	    done (1);
    }

    if (pipe (pd) == NOTOK) {
	(void) fmt2peer (RC_ERR, "no pipes");
	return NOTOK;
    }

    switch (pid = vfork ()) {
	case NOTOK: 
	    (void) fmt2peer (RC_ERR, "no forks");
	    return NOTOK;

	case OK: 
	    (void) close (0);
	    (void) open ("/dev/null", 0);
	    (void) dup2 (pd[1], 1);
	    (void) dup2 (pd[1], 2);
	    (void) close (pd[0]);
	    (void) close (pd[1]);
	    execlp ("/bin/sh", "sh", "-c", str, NULLCP);
	    write (2, "no shell\n", strlen ("no shell\n"));
	    _exit (1);

	default: 
	    (void) close (pd[1]);
	    while ((i = read (pd[0], buffer, sizeof buffer)) > 0)
		switch (rc2rc (RC_DATA, i, buffer, rc)) {
		    case RC_ACK: 
			break;

		    case RC_ERR: 
			(void) close (pd[0]);
			(void) pidwait (pid, OK);
			return NOTOK;

		    case RC_XXX: 
			adios (NULLCP, "%s", rc -> rc_data);

		    default: 
			(void) fmt2peer (RC_ERR, "pWIN protocol screw-up");
			done (1);
		}
	    if (i == OK)
		switch (rc2rc (RC_EOF, 0, NULLCP, rc)) {
		    case RC_ACK: 
			break;

		    case RC_XXX: 
			adios (NULLCP, "%s", rc -> rc_data);

		    default: 
			(void) fmt2peer (RC_ERR, "pWIN protocol screw-up");
			done (1);
		}
	    if (i == NOTOK)
		(void) fmt2peer (RC_ERR, "read from pipe lost");

	    (void) close (pd[0]);
	    (void) pidwait (pid, OK);
	    return (i != NOTOK ? OK : NOTOK);
    }
}
