/* report.c -- event reporting */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/callback/RCS/report.c,v 7.1 91/02/22 09:26:41 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/callback/RCS/report.c,v 7.1 91/02/22 09:26:41 mrose Interim $
 *
 *
 * $Log:	report.c,v $
 * Revision 7.1  91/02/22  09:26:41  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:58:13  mrose
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


#include <stdio.h>
#include <varargs.h>
#include "general.h"
#include "manifest.h"
#include "logger.h"


static LLog _pgm_log = {
    "callback.log", NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
static LLog *pgm_log = &_pgm_log;

/*  */

reportailor (myname)
char   *myname;
{
    isodetailor (myname, 0);
    ll_hdinit (pgm_log, myname);
}

/*  */

#ifndef	lint
void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);
    
    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    _exit (1);
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
