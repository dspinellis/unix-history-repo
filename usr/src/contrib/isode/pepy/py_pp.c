/* py_pp.c - generic pretty-printer */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/py_pp.c,v 7.1 91/02/22 09:35:20 mrose Interim $";
#endif
    
/*
 * $Header: /f/osi/pepy/RCS/py_pp.c,v 7.1 91/02/22 09:35:20 mrose Interim $
 *
 *
 * $Log:	py_pp.c,v $
 * Revision 7.1  91/02/22  09:35:20  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:12:06  mrose
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

#include <stdio.h>
#include "psap.h"

#define	ps_advise(ps, f) \
	advise (NULLCP, "%s: %s", (f), ps_error ((ps) -> ps_errno))

/*    DATA */

static char *myname = "pp";

static enum { ps2pp, pl2pp } mode = ps2pp;


void	adios (), advise ();

/*  */

/* ARGSUSED */

int	PY_pp (argc, argv, envp, pfx)
int	argc;
char  **argv,
      **envp;
IFP	pfx;
{
    register int    status = 0;
    register char  *cp;
    register FILE  *fp;

    if (myname = rindex (argv[0], '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = argv[0];

    for (argc--, argv++; cp = *argv; argc--, argv++)
	if (*cp == '-') {
	    if (strcmp (cp + 1, "ps") == 0) {
		mode = ps2pp;
		continue;
	    }
	    if (strcmp (cp + 1, "pl") == 0) {
		mode = pl2pp;
		continue;
	    }
	    adios (NULLCP, "usage: %s [ -ps | -pl ] [ files... ]", myname);
	}
	else
	    break;

    if (argc == 0)
	status = process ("(stdin)", stdin, pfx);
    else
	while (cp = *argv++) {
	    if ((fp = fopen (cp, "r")) == NULL) {
		advise (cp, "unable to read");
		status++;
		continue;
	    }
	    status += process (cp, fp, pfx);
	    (void) fclose (fp);
	}

    return status;
}

/*  */

static int  process (file, fp, pfx)
register char *file;
register FILE *fp;
IFP	pfx;
{
    register PE	    pe;
    register PS	    ps;

    if ((ps = ps_alloc (std_open)) == NULLPS) {
	ps_advise (ps, "ps_alloc");
	return 1;
    }
    if (std_setup (ps, fp) == NOTOK) {
	advise (NULLCP, "%s: std_setup loses", file);
	return 1;
    }

    for (;;) {
	switch (mode) {
	    case ps2pp: 
		if ((pe = ps2pe (ps)) == NULLPE)
		    if (ps -> ps_errno) {
			ps_advise (ps, "ps2pe");
		you_lose: ;
			ps_free (ps);
			return 1;
		    }
		    else {
		done: 	;
			ps_free (ps);
			return 0;
		    }
		break;

	    case pl2pp: 
		if ((pe = pl2pe (ps)) == NULLPE)
		    if (ps -> ps_errno) {
			ps_advise (ps, "pl2pe");
			goto you_lose;
		    }
		    else
			goto done;
		break;
	}

	(void) (*pfx) (pe, 1, NULLIP, NULLVP, NULLCP);

	pe_free (pe);
    }
}

/*    ERRORS */

#include <varargs.h>


#ifndef	lint
void	_advise ();


static void  adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _advise (ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

static void  adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
static void  advise (va_alist)
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

    (void) fflush (stdout);

    fprintf (stderr, "%s: ", myname);
    (void) fputs (buffer, stderr);
    (void) fputc ('\n', stderr);

    (void) fflush (stderr);
}
#else
/* VARARGS */

static void  advise (what, fmt)
char   *what,
       *fmt;
{
    advise (what, fmt);
}
#endif
