/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * advise.c - log error messages
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/advise.c,v 7.3 91/02/22 09:27:48 mrose Interim $
 *
 * $Log:	advise.c,v $
 * Revision 7.3  91/02/22  09:27:48  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:54:23  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:01:23  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/advise.c,v 7.3 91/02/22 09:27:48 mrose Interim $";
#endif

/*
 *                              NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include <stdio.h>
#include <varargs.h>
#include "manifest.h"
#include "logger.h"

static LLog _pgm_log = {
   "rfa.log", NULLCP, NULLCP,
   LLOG_FATAL | LLOG_EXCEPTIONS, LLOG_FATAL, 100,
   LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
LLog *pgm_log = &_pgm_log;


void initLog(myname)
    char *myname;
{
/*    if (isatty (fileno (stderr)))
        ll_dbinit (pgm_log, myname);
    else */ {

        static char  myfile[BUFSIZ];

        (void) sprintf (myfile, "%s.log", (strncmp (myname, "ros.", 4)
                                && strncmp (myname, "lpp.", 4))
                                || myname[4] == NULL
                            	? myname : myname + 4);
        pgm_log -> ll_file = myfile;
        ll_hdinit (pgm_log, myname);
    }
}


#ifndef	lint
void	adios (va_alist)
va_dcl
{
    char   *what;
    va_list ap;

    va_start (ap);

    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    cleanup ();

    _exit (1);
}
#else
/* VARARGS2 */

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
    int	    code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}

#else
/* VARARGS */

void	advise (code, what, fmt)
char   *what,
       *fmt;
int	code;
{
    advise (code, what, fmt);
}
#endif



#ifndef	lint
void	ryr_advise (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _ll_log (pgm_log, LLOG_NOTICE, ap);

    va_end (ap);
}
#else
/* VARARGS2 */
void	ryr_advise (what, fmt)
char   *what,
       *fmt;
{
    ryr_advise (what, fmt);
}
#endif

