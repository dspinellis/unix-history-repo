/* time.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/time.c,v 7.1 91/02/22 09:20:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/time.c,v 7.1 91/02/22 09:20:33 mrose Interim $
 *
 *
 * $Log:	time.c,v $
 * Revision 7.1  91/02/22  09:20:33  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:44:38  mrose
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


/* SYNTAX

	time ::= 'yymmddhhmmssz'

	where yy   = year
	      mm   = month
	      dd   = day
	      hh   = hours
	      mm   = minutes
	      ss   = seconds
              z    = timezone

    EXAMPLE
	890602093221Z -> 09:32:21 GMT, on June 2nd 1989.

*/

#include "quipu/util.h"
#include "quipu/attr.h"
#include "psap.h"

strprint ();
sfree ();
pstrcmp();

static UTC	qstr2utct (s, len)
char   *s;
int	len;
{
    UTC	    ut;
    
    if (len > 14
	    && strncmp (s, "1989", 4) == 0
	    && (ut = str2utct (s + 2, len - 2)))
	return ut;

    return str2utct (s, len);
}

#define	str2utct	qstr2utct


static PE timeenc (x)
char *x;
{
PE ret_pe = NULLPE;

	(void) build_UNIV_UTCTime (&ret_pe,0,0,x,NULL);
	return (ret_pe);
}

static char * timedec (pe)
PE pe;
{
char * x;
	if (parse_UNIV_UTCTime (pe,0,0,&x,NULL) == NOTOK)
		return (NULLCP);
	return (x);
}

utcprint (ps,time,format)
PS ps;
char *time;
int format;
{
    UTC	    ut;

    if (format == READOUT && (ut = str2utct (time, strlen (time)))) {
	long    mtime;

	mtime = gtime (ut2tm (ut));

	ps_printf (ps, "%-24.24s", ctime (&mtime));
    }
    else
	ps_printf (ps, "%s", time);
}


static utccmp (a, b)
char   *a, *b;
{
    long    a_time,
	    mdiff;
    UTC	    ut;

    if ((ut = str2utct (a, strlen (a))) == NULL)
	return pstrcmp (a, b);
    a_time = gtime (ut2tm (ut));

    if ((ut = str2utct (b, strlen (b))) == NULL)
	return pstrcmp (a, b);

    return ((mdiff = a_time - gtime (ut2tm (ut))) == 0L ? 0
							: mdiff > 0L ? 1 : -1);
}


time_syntax ()
{
	(void) add_attribute_syntax ("UTCTime",
		(IFP) timeenc,	(IFP) timedec,
		(IFP) strdup,	utcprint,
		(IFP) strdup,	utccmp,
		sfree,		NULLCP,
		NULLIFP,	FALSE);
}

