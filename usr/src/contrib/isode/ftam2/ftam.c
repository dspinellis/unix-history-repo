/* ftam.c - interactive FTAM initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftam.c,v 7.4 91/02/22 09:23:52 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftam.c,v 7.4 91/02/22 09:23:52 mrose Interim $
 *
 *
 * $Log:	ftam.c,v $
 * Revision 7.4  91/02/22  09:23:52  mrose
 * Interim 6.8
 * 
 * Revision 7.3  91/01/07  12:41:49  mrose
 * update
 * 
 * Revision 7.2  90/11/21  11:30:38  mrose
 * sun
 * 
 * Revision 7.1  90/07/09  14:37:18  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:54:27  mrose
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


#include <ctype.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "ftamuser.h"
#include "tailor.h"

/*    DATA */

#ifndef	BRIDGE
static char *myname = "ftam";

static char **op = NULL;

static char *aflag = NULL;
static char *concur = NULL;
static char *oflag = NULL;
static int   fflag = 0;
static char *hflag = NULL;
static char *uflag = NULL;


static char ringring = 0x07;

int	ontty;

static int	armed;
static jmp_buf	intrenv;
#endif
int	interrupted;


void	adios (), advise ();
#ifndef	BRIDGE
SFD	intrser ();
#endif

/*    MAIN */

/* ARGSUSED */

#ifndef	BRIDGE
main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int     eof,
	    status,
            vecp;
    SFP	    istat;
    char   *bp,
	    buffer[BUFSIZ],
           *vec[NVEC + 1];
    FILE   *fp;

    arginit (argv);

    runcom = 1;
    
    /* The lower layers get a SIGPIPE if the remote end dies while
     * we are sending. The SIGPIPE is followed by a DISCONNECT Request.
     */
    (void)signal(SIGPIPE, SIG_IGN);

    rcinit ();
    (void) sprintf (buffer, "%s/.ftamrc", myhome);

    if (!fflag && (fp = fopen (buffer, "r"))) {
	while (fgets (buffer, sizeof buffer, fp)) {
	    if (bp = index (buffer, '\n'))
		*bp = NULL;

	    bzero ((char *) vec, sizeof vec);
	    if ((vecp = str2vec (buffer, vec)) < 1)
		continue;

	    if (ftamloop (vec, NOTOK) != OK && op)
		exit (1);
	}
    
	(void) fclose (fp);
    }
	
    if (hflag) {
	vecp = 0;
	vec[vecp++] = "open";
	vec[vecp++] = hflag;
	if (uflag)
	    vec[vecp++] = uflag;
	if (aflag)
	    vec[vecp++] = aflag;
	vec[vecp] = NULL;

	if (ftamloop (vec, NOTOK) != OK && op)
	    exit (1);
    }
    else {
	if (uflag)
	    user = strdup (uflag);
	if (aflag)
	    account = strdup (aflag);
    }

    if (concur) {
	vecp = 0;
	vec[vecp++] = "set";
	vec[vecp++] = "concurrency";
	vec[vecp++] = concur;
	vec[vecp] = NULL;

	if (ftamloop (vec, NOTOK) != OK && op)
	    exit (1);
    }

    if (oflag) {
	vecp = 0;
	vec[vecp++] = "set";
	vec[vecp++] = "override";
	vec[vecp++] = oflag;
	vec[vecp] = NULL;

	if (ftamloop (vec, NOTOK) != OK && op)
	    exit (1);
    }
	
    runcom = 0;

    if (op) {
	for (vecp = 0; *op; op++)
	    vec[vecp++] = *op;
	vec[vecp] = NULL;

	status = ftamfd != NOTOK ? 1 : 0;
	switch (ftamloop (vec, NOTOK)) {
	    case NOTOK: 
		status = 1;
		break;

	    case OK: 
	    case DONE: 
	    default:
		if (ftamfd != NOTOK)
		    status = 0;
		break;
	}
    }
    else {
	istat = signal (SIGINT, intrser);

	eof = 0;
	for (interrupted = 0;; interrupted = 0) {
	    if (hash && marks >= BUFSIZ) {
		marks = 0;
		printf ("\n");
	    }

	    if (getline ("%s> ", buffer) == NOTOK) {
		if (eof)
		    break;

		eof = 1;
		continue;
	    }
	    eof = 0;

	    bzero ((char *) vec, sizeof vec);
	    if ((vecp = str2vec (buffer, vec)) < 1)
		continue;

	    switch (ftamloop (vec, OK)) {
		case NOTOK: 
		    status = 1;
		    break;

		case OK: 
		default: 
		    if (bell)
			(void) putchar (ringring);
		    continue;

		case DONE: 
		    status = 0;
		    break;
	    }
	    break;
	}

	(void) signal (SIGINT, istat);
    }

    if (ftamfd != NOTOK) {
	vecp = 0;
	vec[vecp++] = "close";
	vec[vecp] = NULL;
	
	(void) ftamloop (vec, NOTOK);
    }

#ifdef	DEBUG
    set_lookup_dase (0);
#endif

    exit (status);		/* NOTREACHED */
}
#endif

/*  */

#ifndef	BRIDGE
static	ftamloop (vec, error)
char  **vec;
int	error;
{
    register struct dispatch   *ds;

    if ((ds = getds (strcmp (*vec, "?") ? *vec : "help")) == NULL)
	return error;

    if (ftamfd == NOTOK) {
	if (ds -> ds_flags & DS_OPEN) {
	    advise (NULLCP, "not associated with virtual filestore");
	    return error;
	}
    }
    else
	if (ds -> ds_flags & DS_CLOSE) {
	    advise (NULLCP, "already associated with virtual filestore");
	    return error;
	}

    if (ds -> ds_flags & DS_MODES) {
	switch (ds -> ds_class) {
	    case FCLASS_TRANSFER:
		if (class != FCLASS_TRANSFER && class != FCLASS_TM) {
		    advise (NULLCP, "need transfer service class");
		    return error;
		}
		break;

	    case FCLASS_MANAGE:
		if (class != FCLASS_MANAGE && class != FCLASS_TM) {
		    advise (NULLCP, "need management service class");
		    return error;
		}
		break;

	    default:
		break;
	}

	if ((ds -> ds_units & units) != ds -> ds_units) {
	    advise (NULLCP, "need %s functional units",
		    sprintb (ds -> ds_units & ~units, UMASK));
	    return error;
	}
    }

    switch ((*ds -> ds_fnx) (vec)) {
	case NOTOK: 
	    return error;

	case OK: 
	default: 
	    return OK;

	case DONE: 
	    return DONE;
    }
}
#endif

/*    ARGINIT */

#ifndef	BRIDGE
static	arginit (vec)
char  **vec;
{
    register char  *ap,
                   *pp;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;

    isodetailor (myname, 1);
    ftam_log -> ll_file = strdup ("./ftam.log");
    ll_hdinit (ftam_log, myname);

    storename = strdup ("filestore");
    if (ontty = isatty (fileno (stdin)))
	verbose++;

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-') {
	    while (*++ap)
		switch (*ap) {
		    case 'a': 
			if ((pp = *++vec) == NULL || *pp == NULL)
			    adios (NULLCP, "usage: %s -a acct", myname);
			aflag = pp;
			break;

		    case 'c':
			if ((pp = *++vec) == NULL || *pp == NULL)
				adios (NULLCP, "usage: %s -c mode", myname);
			concur = pp;
			break;

		    case 'd': 
			debug++;
			break;

		    case 'f':
			fflag++;
			break;

		    case 'h': 
			hash++;
			break;

		    case 'o': 
			if ((pp = *++vec) == NULL || *pp == NULL)
			    adios (NULLCP, "usage: %s -o mode", myname);
			oflag = pp;
			break;

		    case 't': 
			trace++;
			break;

		    case 'u': 
			if ((pp = *++vec) == NULL || *pp == NULL)
			    adios (NULLCP, "usage: %s -u user", myname);
			uflag = pp;
			break;

		    case 'v': 
			verbose = 1;
			break;

		    case 'w': 
			watch++;
			break;

		    default: 
			adios (NULLCP, "unknown switch -%c", *ap);
		}
	    continue;
	}

	if (hflag == NULL)
	    hflag = ap;
	else
	    if (op == NULL) {
		op = vec;
		break;
	    }
    }
}
#endif

/*    INTERACTIVE */

#ifndef	BRIDGE
int	getline (prompt, buffer)
char   *prompt,
       *buffer;
{
    register int    i;
    register char  *cp,
                   *ep;
    static int  sticky = 0;

    if (interrupted) {
	interrupted = 0;
	return NOTOK;
    }

    if (sticky) {
	sticky = 0;
	return NOTOK;
    }

    switch (setjmp (intrenv)) {
	case OK:
	    armed++;
	    break;

	case NOTOK:
	    if (ontty)
		printf ("\n");	/* and fall */
	default:
	    armed = 0;
	    return NOTOK;
    }
	
    if (ontty) {
	printf (prompt, ftamfd != NOTOK ? host : myname);
	(void) fflush (stdout);
    }

    for (ep = (cp = buffer) + BUFSIZ - 1; (i = getchar ()) != '\n';) {
	if (i == EOF) {
	    if (ontty)
		printf ("\n");
	    clearerr (stdin);
	    if (cp == buffer)
		longjmp (intrenv, DONE);

	    sticky++;
	    break;
	}

	if (cp < ep)
	    *cp++ = i;
    }
    *cp = NULL;

    armed = 0;
    
    return OK;
}
#endif

/*  */

#ifndef	BRIDGE

/* ARGSUSED */

static	SFD intrser (sig)
int	sig;
{
#ifndef	BSDSIGS
    (void) signal (SIGINT, intrser);
#endif

    if (armed)
	longjmp (intrenv, NOTOK);

    interrupted++;
}
#endif

/*  */

#ifndef	BRIDGE
#ifndef	lint
int	ask (va_alist)
va_dcl
{
    int     x,
            y,
            result;
    char    buffer[BUFSIZ];
    va_list ap;

    if (interrupted) {
	interrupted = 0;
	return NOTOK;
    }

    if (!ontty)
	return OK;

    switch (setjmp (intrenv)) {
	case OK: 
	    armed++;
	    break;

	case NOTOK: 
	default: 
	    printf ("\n");
	    armed = 0;
	    return DONE;
    }
    if (bell)
	(void) putchar (ringring);

    va_start (ap);

    _asprintf (buffer, NULLCP, ap);

    va_end (ap);
    
again: ;
    printf ("%s? (y)es, (n)o: ", buffer);

    x = y = getchar ();
    while (y != '\n' && y != EOF)
	y = getchar ();

    switch (x) {
	case 'y': 
	case '\n':
	    result = OK;
	    break;

	case 'n': 
	    result = NOTOK;
	    break;

	case EOF: 
	    result = DONE;
	    break;

	default: 
	    goto again;
    }

    armed = 0;

    return result;
}
#else
/* VARARGS */

int	ask (fmt)
char   *fmt;
{
    return ask (fmt);
}
#endif
#endif

/*    ERRORS */

#ifndef	lint
void	_advise ();


void	adios (va_alist)
va_dcl
{
    struct FTAMindication   ftis;
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);

    if (ftamfd != NOTOK)
	(void) FUAbortRequest (ftamfd, FACTION_PERM,
		(struct FTAMdiagnostic *) 0, 0, &ftis);

#ifdef	BRIDGE
    reply (550, ftam_error);
    exit (1);
#else
    _exit (1);
#endif
}
#else
/* VARARGS */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);
}


static void  _advise (ap)
va_list	ap;
{
    char    buffer[BUFSIZ];

    asprintf (buffer, ap);

#ifndef	BRIDGE
    if (hash && marks >= BUFSIZ) {
	marks = 0;
	printf ("\n");
    }

    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    (void) fputs (buffer, stderr);
    (void) fputc ('\n', stderr);

    (void) fflush (stderr);
#else
    (void) ll_log (ftam_log, LLOG_NOTICE, NULLCP, "%s", buffer);
    (void) strcpy (ftam_error, buffer);
#endif
}
#else
/* VARARGS */

void	advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif

/*    MISCELLANY */

#ifndef	lint
char   *strdup (s)
char   *s;
{
    char    *p;

    if ((p = malloc((unsigned) (strlen (s) + 1))) == NULL)
	adios (NULLCP, "out of memory");

    (void) strcpy (p, s);

    return p;
}
#endif
