-- pp.py - generic pretty-printer

-- $Header: /f/osi/pepy/RCS/pp.py,v 7.1 91/02/22 09:35:19 mrose Interim $
--
--
-- $Log:	pp.py,v $
-- Revision 7.1  91/02/22  09:35:19  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:12:04  mrose
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


PRETTY DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/pp.py,v 7.1 91/02/22 09:35:19 mrose Interim $";
#endif

/*  */

%}


BEGIN

PRINTER print

Printer ::=
    ANY

END

%{

/*  */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    exit (PY_pp (argc, argv, envp, print_PRETTY_Printer));
}

/*  */

%}
