/* ftamlose.c - FPM: you lose */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamlose.c,v 7.1 91/02/22 09:23:02 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamlose.c,v 7.1 91/02/22 09:23:02 mrose Interim $
 *
 *
 * $Log:	ftamlose.c,v $
 * Revision 7.1  91/02/22  09:23:02  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:43  mrose
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
#include <varargs.h>
#include "fpkt.h"

/*  */

#ifndef	lint
int	fpktlose (va_alist)
va_dcl
{
    int     observer,
    	    reason,
            result,
            source;
    struct FTAMindication   ftis;
    register struct ftamblk *fsb;
    register struct FTAMindication *fti;
    va_list	ap;

    va_start (ap);

    fsb = va_arg (ap, struct ftamblk *);
    fti = va_arg (ap, struct FTAMindication *);
    reason = va_arg (ap, int);

    if (fsb -> fsb_flags & FSB_INIT)
	observer = EREF_IFPM, source = EREF_RFPM;
    else
	observer = EREF_RFPM, source = EREF_IFPM;

    result = _ftamoops (fti, reason, 1, observer, source, ap);

    va_end (ap);

    if (fsb -> fsb_fd == NOTOK)
	return result;

    (void) FAbortRequestAux (fsb, type_FTAM_PDU_f__p__abort__request,
			     FACTION_PERM, fti -> fti_abort.fta_diags, 1,
			     &ftis);

    return result;
}
#else
/* VARARGS5 */

int	fpktlose (fsb, fti, reason, what, fmt)
struct ftamblk *fsb;
struct FTAMindication *fti;
int	reason;
char   *what,
       *fmt;
{
    return fpktlose (fsb, fti, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	ftamlose (va_alist)
va_dcl
{
    int     fatal,
    	    reason,
	    result;
    struct FTAMindication *fti;
    va_list	ap;

    va_start (ap);

    fti = va_arg (ap, struct FTAMindication *);
    reason = va_arg (ap, int);
    fatal = va_arg (ap, int);

    result = _ftamoops (fti, reason, fatal, EREF_NONE, EREF_NONE, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	ftamlose (fti, reason, fatal, what, fmt)
register struct FTAMindication *fti;
int	reason,
	fatal;
char   *what,
       *fmt;
{
    return ftamlose (fti, reason, fatal, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	ftamoops (va_alist)
va_dcl
{
    int	    reason,
	    result,
	    fatal,
	    observer,
	    source;
    struct FTAMindication *fti;
    va_list ap;

    va_start (ap);

    fti = va_arg (ap, struct FTAMindication *);
    reason = va_arg (ap, int);
    fatal = va_arg (ap, int);
    observer = va_arg (ap, int);
    source = va_arg (ap, int);

    result = _ftamoops (fti, reason, fatal, source, observer, ap);

    va_end (ap);

    return result;
}


static int  _ftamoops (fti, reason, fatal, observer, source, ap)
register struct FTAMindication *fti;
int	reason,
	fatal,
	observer,
	source;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    register struct FTAMabort  *fta;
    register struct FTAMdiagnostic *ftd;

    if (fti) {
	bzero ((char *) fti, sizeof *fti);
	fti -> fti_type = FTI_ABORT;
	fta = &fti -> fti_abort;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	fta -> fta_peer = 0;
	fta -> fta_action = fatal ? FACTION_PERM : FACTION_TRANS;

	ftd = &fta -> fta_diags[0];
	ftd -> ftd_type = fatal ? DIAG_PERM : DIAG_TRANS;
	ftd -> ftd_identifier = reason;
	ftd -> ftd_observer = observer;
	ftd -> ftd_source = source;
	ftd -> ftd_delay = DIAG_NODELAY;
	copyFTAMdata (buffer, bp - buffer, ftd);

	fta -> fta_ndiag = 1;
    }

    return NOTOK;
}
#else
/* VARARGS7 */

int	ftamoops (fti, reason, fatal, observer, source, what, fmt)
struct FTAMindication *fti;
int	reason,
	fatal,
	observer,
	source;
char   *what,
       *fmt;
{
    return ftamoops (fti, reason, fatal, observer, source, what, fmt);
}
#endif
