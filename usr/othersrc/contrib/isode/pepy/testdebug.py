-- testdebug.py - support routine for pepy generated routines

-- $Header: /f/osi/pepy/RCS/testdebug.py,v 7.1 91/02/22 09:35:22 mrose Interim $
--
--
-- $Log:	testdebug.py,v $
-- Revision 7.1  91/02/22  09:35:22  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:12:07  mrose
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


TESTDEBUG DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/testdebug.py,v 7.1 91/02/22 09:35:22 mrose Interim $";
#endif

/* LINTLIBRARY */


#include <stdio.h>

%}

BEGIN

END

%{
int	testdebug (pe, s)
register PE	pe;
register char	*s;
{
    char  *cp;
    register PS ps;
    static int debug = OK;
    
    switch (debug) {
	case NOTOK:
	    return;

	case OK:
	    if ((debug = (cp = getenv ("PEPYDEBUG")) && *cp ? atoi (cp)
							    : NOTOK) == NOTOK)
		return;
	    (void) fflush (stdout);
	    fprintf (stderr, "testdebug made with %s\n", pepyid);
	    /* and fall... */

	default:
	    (void) fflush (stdout);
	    fprintf (stderr, "%s\n", s);

	    if ((ps = ps_alloc (std_open)) == NULLPS)
		break;
	    if (std_setup (ps, stderr) != NOTOK)
		(void) pe2pl (ps, pe);
	    fprintf (stderr, "--------\n");
	    ps_free (ps);
	    break;
    }
}

%}
