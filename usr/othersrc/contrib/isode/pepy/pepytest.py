-- pepytest.py - test out PEPY

-- $Header: /f/osi/pepy/RCS/pepytest.py,v 7.1 91/02/22 09:35:11 mrose Interim $
--
--
-- $Log:	pepytest.py,v $
-- Revision 7.1  91/02/22  09:35:11  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:11:57  mrose
-- Release 6.0
-- 

--
--				  NOTICE
--
--    Acquisition, use, and distribution of this module and related
--    materials are subject to the restrictions of a license agreement.
--    Consult the Preface in the User's Manual for the full terms of
--    this agreement.
--
--


PEPYTEST DEFINITIONS   ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/pepytest.py,v 7.1 91/02/22 09:35:11 mrose Interim $";
#endif

#include <stdio.h>

/*    DATA */

#define	ps_advise(ps, f) \
	advise (NULLCP, "%s: %s", (f), ps_error ((ps) -> ps_errno))


static char *myname = "pepytest";

static enum { ps2test, pl2test } mode = ps2test;


void	adios ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    register int    status = 0;
    register char  *cp;
    register FILE  *fp;

    myname = *argv;
    for (argc--, argv++; cp = *argv; argc--, argv++)
	if (*cp == '-') {
	    if (strcmp (cp + 1, "ps") == 0) {
		mode = ps2test;
		continue;
	    }
	    if (strcmp (cp + 1, "pl") == 0) {
		mode = pl2test;
		continue;
	    }
	    adios (NULLCP, "usage: %s [ -ps | -pl ] [ files... ]",
		    myname);
	}
	else
	    break;

    if (argc == 0)
	status = process ("(stdin)", stdin);
    else
	while (cp = *argv++) {
	    if ((fp = fopen (cp, "r")) == NULL) {
		advise (cp, "unable to read");
		status++;
		continue;
	    }
	    status += process (cp, fp);
	    (void) fclose (fp);
	}

    exit (status);		/* NOTREACHED */
}

/*  */

static int  process (file, fp)
register char *file;
register FILE *fp;
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
	    case ps2test: 
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

	    case pl2test: 
		if ((pe = pl2pe (ps)) == NULLPE)
		    if (ps -> ps_errno) {
			ps_advise (ps, "pl2pe");
			goto you_lose;
		    }
		    else
			goto done;
		break;
	}

	if (parse_PEPYTEST_PersonnelRecord (pe, 1, NULLIP, NULLVP, NULLCP)
	        == NOTOK)
	    advise (NULLCP, "parse error: %s", PY_pepy);
	else
	    (void) print_PEPYTEST_PersonnelRecord (pe, 1, NULLIP, NULLVP,
						   NULLCP);

	pe_free (pe);
    }
}

/*  */

%}

BEGIN

SECTIONS none parse print

PersonnelRecord
	::=
	[APPLICATION 0]
	    IMPLICIT SET {
		    Name,

		title[0]
		    VisibleString,

		number
		    EmployeeNumber,

		dateOfHire[1]
		    Date,

		nameOfSpouse[2]
		    Name,

		children[3]
		    IMPLICIT SEQUENCE OF
			ChildInformation
    		    DEFAULT {}
	    }


ChildInformation ::=
	SET {
		Name,

	    dateofBirth[0]
		Date
	}


Name ::=
	[APPLICATION 1]
	    IMPLICIT SEQUENCE {
		givenName
		    VisibleString,

		initial
		    VisibleString,

		familyName
		    VisibleString
	    }


EmployeeNumber ::=
	[APPLICATION 2]
	    IMPLICIT INTEGER


Date ::=
	[APPLICATION 3]
	    IMPLICIT VisibleString -- YYYYMMDD

END

%{

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

%}
