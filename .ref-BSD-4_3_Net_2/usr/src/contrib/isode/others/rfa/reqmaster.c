/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * reqmaster.c : responder operation to transfer mastership for file
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/reqmaster.c,v 7.3 91/02/22 09:28:10 mrose Interim $
 *
 * $Log:	reqmaster.c,v $
 * Revision 7.3  91/02/22  09:28:10  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:54:41  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:07:02  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/reqmaster.c,v 7.3 91/02/22 09:28:10 mrose Interim $";
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
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <varargs.h>
#include "ryresponder.h"    /* for generic idempotent responders */
#include "psap.h"   /* for generic idempotent responders */
#include "RFA-ops.h"        /* operation definitions */
#include "RFA-types.h"  /* type definitions */
#include "rfa.h"
#include "rfainfo.h"

/*--------------------------------------------------------------
 *  op_reqMaster - get mastership of a file
 *-------------------------------------------------------------*/
op_requestMaster (sd, ryo, rox, in, roi)
int sd;    
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t in;
struct RoSAPindication *roi;
{
     register struct type_RFA_RequestMasterArg *rma = 
		(struct type_RFA_RequestMasterArg *) in;
     struct type_RFA_RequestMasterRes rmr;
     struct RfaInfo *rfalist, *rfa;
     char *s;
     time_t t;
     char buf[BUFSIZ];
     char *fn;
     int rc;

     if (rox -> rox_nolinked == 0) {
	 advise (LLOG_NOTICE, NULLCP,
		 "RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
		 sd, ryo -> ryo_name, rox -> rox_linkid);
	 return ureject (sd, ROS_IP_LINKED, rox, roi);
     }
     advise (LLOG_DEBUG, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	     sd, ryo -> ryo_name);

    if((s = qb2str(rma->filename)) == NULL) 
	return NOTOK_SYS;

    /*--- expand symlinks and get relative path ---*/
    if ((fn = expandSymLinks(s)) == NULL) {
	free(s);
	advise(LLOG_EXCEPTIONS,NULLCP,"reqMaster: %s", rfaErrStr);
	return strerror(sd, error_RFA_fileAccessError, rfaErrStr, rox, roi);
    }
    free(s);

    /*--- get rfainfo list for directory (begin of critical section) ---*/
    if ((rc = getLockedRfaInfoList(dirname(fn), &rfalist, basename(fn)))
	    != OK) 
    { 
	return rc;
    }

    /*--- check if file exists and user is allowed to write it ---*/
    if ((rfa = findRfaInfo(basename(fn), rfalist)) == NULL)   {
	releaseRfaInfoList(dirname(fn), rfalist);
	advise(LLOG_NOTICE,NULLCP,"reqMaster: %s not .rfainfo",
		fn);
	return statusError(sd, int_RFA_reason_notRegistered, NULL, 0L,rox,roi);
    }

    if (access(makeFN(fn), W_OK) == -1)  {
	releaseRfaInfoList(dirname(fn), rfalist);
	advise(LLOG_NOTICE,NULLCP,"reqMaster: %s not writable",
		fn);
	return statusError(sd, int_RFA_reason_notWritable, NULL, 0L,rox,roi);
    }

    /*--- check if we are master of the file ---*/
    switch(RI_STATUS(rfa->ri_status)) {
	case RI_MASTER:
		if (IS_LOCKED(rfa->ri_status)) {
		    releaseRfaInfoList(dirname(fn), rfalist);
		    return statusError(sd, int_RFA_reason_locked, 
				rfa->ri_lckname, rfa->ri_lcksince, rox, roi);
		}
		break;
	case RI_SLAVE:
		releaseRfaInfoList(dirname(fn), rfalist);
		advise(LLOG_NOTICE,NULLCP,"reqMaster: not master for %s",
			fn);
		return statusError(sd, int_RFA_reason_notMaster, NULL,
					0L, rox, roi);
		break;
	case RI_UNREGISTERED:
		releaseRfaInfoList(dirname(fn), rfalist);
		advise(LLOG_NOTICE,NULLCP,"reqMaster: %s not registered",
			fn);
		return statusError(sd, int_RFA_reason_notRegistered, NULL, 
					0L, rox, roi);
		break;
	default:
		releaseRfaInfoList(dirname(fn), rfalist);
		sprintf(buf, "unknown file status %d for %s",rfa->ri_status,fn);
		return strerror(sd, error_RFA_miscError, buf, rox, roi);
		break;
    }

    advise(LLOG_DEBUG,NULLCP,"reqMaster: allowing lock mv=%ld sv=%d", 
		rfa->ri_modTime, rma->slaveVersion);

    /*--- so we are master of the file, check the modify times ---*/
    if (rfa->ri_mode & S_IFMT & S_IFREG) {
	t = rma->slaveVersion;
	if (rfa->ri_modTime != t)  {
	    releaseRfaInfoList(dirname(fn), rfalist);
	    advise(LLOG_NOTICE,NULLCP,"reqMaster: lock %s: wrong version %s",
		    fn, ctime(&t));
	    return statusError(sd, int_RFA_reason_wrongVersion, NULL, 
				    0L, rox, roi);
	}
    }
    
    /*--- grant mastership, update rfalist ---*/
    SET_STATUS(rfa->ri_status, RI_SLAVE);
    time(&(rfa->ri_lastChange));
    rfa->ri_lcksince = NULL;
    if (rfa->ri_lckname)
	free(rfa->ri_lckname);
    rfa->ri_lckname = NULL;
    if (putRfaInfoList(dirname(fn), rfalist) != OK) {
	releaseRfaInfoList(dirname(fn), rfalist);
	advise(LLOG_EXCEPTIONS,NULLCP,"reqMaster: could not write %s/.rfainfo", 
		dirname(fn));
	return syserror(sd, error_RFA_miscError, rox, roi);
    }
    advise(LLOG_NOTICE,NULLCP,"reqMaster: sending result");

    /*--- return result ----*/
    rmr.parm = rfa->ri_modTime;
    if (RyDsResult(sd, rox->rox_id, (caddr_t) &rmr, ROS_NOPRIO, roi) != OK) {

	/*--- result failed, so assume that peer has not become master ---*/
	advise(LLOG_NOTICE,NULLCP,"reqMaster: sending result failed");
	SET_STATUS(rfa->ri_status, RI_MASTER);
	if (putRfaInfoList(dirname(fn), rfalist) != OK) 
	 advise(LLOG_EXCEPTIONS,NULLCP,"reqMaster: couldn't reset MASTER for %s"
		    , fn);
	releaseRfaInfoList(dirname(fn), rfalist);
	ros_adios (&roi -> roi_preject, "RESULT");
    }

    
    if (rfa->ri_mode & S_IFMT & S_IFREG) 
	if(makeFileReadOnly(fn, rfa) != OK)
	     advise(LLOG_EXCEPTIONS,NULLCP,"reqMaster: %s",rfaErrStr);

    releaseRfaInfoList(dirname(fn), rfalist);
    return OK;
}
