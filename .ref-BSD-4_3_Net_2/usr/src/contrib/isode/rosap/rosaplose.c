/* rosaplose.c - ROPM: you lose */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/rosaplose.c,v 7.1 91/02/22 09:41:32 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/rosaplose.c,v 7.1 91/02/22 09:41:32 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	rosaplose.c,v $
 * Revision 7.1  91/02/22  09:41:32  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:27  mrose
 * Release 5.0
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
#include "ropkt.h"
#include "tailor.h"

/*  */

#ifndef	lint
int	ropktlose (va_alist)
va_dcl
{
    int	    reason,
    	    result,
    	    value;
    register struct assocblk *acb;
    register struct RoSAPindication *roi;
    register struct RoSAPpreject *rop;
    va_list ap;

    va_start (ap);

    acb = va_arg (ap, struct assocblk *);
    roi = va_arg (ap, struct RoSAPindication *);
    reason = va_arg (ap, int);

    result = _rosaplose (roi, reason, ap);

    va_end (ap);

    if ((rop = &roi -> roi_preject) -> rop_cc > 0) {
	SLOG (rosap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("ropktlose [%s] %*.*s", RoErrString (rop -> rop_reason),
	       rop -> rop_cc, rop -> rop_cc, rop -> rop_data));
    }
    else
	SLOG (rosap_log, LLOG_EXCEPTIONS, NULLCP,
	      ("ropktlose [%s]", RoErrString (rop -> rop_reason)));

    if (acb == NULLACB
	    || acb -> acb_fd == NOTOK
	    || acb -> acb_ropktlose == NULLIFP)
	return result;

    switch (reason) {
	case ROS_PROTOCOL: 
	    value = ABORT_PROTO;
	    break;

	case ROS_CONGEST: 
	    value = ABORT_TMP;
	    break;

	default: 
	    value = ABORT_LSP;
	    break;
    }

    (*acb -> acb_ropktlose) (acb, value);

    return result;
}
#else
/* VARARGS5 */

int	ropktlose (acb, roi, reason, what, fmt)
struct assocblk *acb;
struct RoSAPindication *roi;
int     reason;
char   *what,
       *fmt;
{
    return ropktlose (acb, roi, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	rosapreject (va_alist)
va_dcl
{
    int	    reason,
    	    result;
    register struct assocblk *acb;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi;
    va_list ap;

    va_start (ap);

    acb = va_arg (ap, struct assocblk *);
    roi = va_arg (ap, struct RoSAPindication *);
    reason = va_arg (ap, int);
    
    result = _rosaplose (roi, reason, ap);

    va_end (ap);

    if (RoURejectRequestAux (acb, NULLIP, reason - REJECT_GENERAL_BASE,
		REJECT_GENERAL, 0, &rois) == NOTOK
	    && ROS_FATAL (rois.roi_preject.rop_reason)) {
	*roi = rois;		/* struct copy */
	result = NOTOK;
    }

    return result;
}
#else
/* VARARGS5 */

int	rosapreject (acb, roi, reason, what, fmt)
struct assocblk *acb;
struct RoSAPindication *roi;
int     reason;
char   *what,
       *fmt;
{
    return rosapreject (acb, roi, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
int	rosaplose (va_alist)
va_dcl
{
    int	    reason,
	    result;
    struct RoSAPindication *roi;
    va_list (ap);

    va_start (ap);

    roi = va_arg (ap, struct RoSAPindication *);
    reason = va_arg (ap, int);

    result = _rosaplose (roi, reason, ap);

    va_end (ap);

    return result;
}
#else
/* VARARGS4 */

int	rosaplose (roi, reason, what, fmt)
struct RoSAPindication *roi;
int	reason;
char   *what,
       *fmt;
{
    return rosaplose (roi, reason, what, fmt);
}
#endif

/*  */

#ifndef	lint
static int  _rosaplose (roi, reason, ap)  /* what, fmt, args ... */
register struct RoSAPindication *roi;
int     reason;
va_list	ap;
{
    register char  *bp;
    char    buffer[BUFSIZ];
    register struct RoSAPpreject *rop;

    if (roi) {
	bzero ((char *) roi, sizeof *roi);
	roi -> roi_type = ROI_PREJECT;
	rop = &roi -> roi_preject;

	asprintf (bp = buffer, ap);
	bp += strlen (bp);

	rop -> rop_reason = reason;
	copyRoSAPdata (buffer, bp - buffer, rop);
    }

    return NOTOK;
}
#endif
