/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * fileinfo.c : responder part of GetFileInfo operation
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/fileinfo.c,v 7.3 91/02/22 09:27:59 mrose Interim $
 *
 * $Log:	fileinfo.c,v $
 * Revision 7.3  91/02/22  09:27:59  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:54:31  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:04:56  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/fileinfo.c,v 7.3 91/02/22 09:27:59 mrose Interim $";
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
#include <varargs.h>
#include <dirent.h>
#include <errno.h>
#include "ryresponder.h" 
#include "psap.h" 
#include "RFA-ops.h"        /* operation definitions */
#include "RFA-types.h"  /* type definitions */
#include "rfa.h"
#include "rfainfo.h"

/*--------------------------------------------------------------
 *  op_listDir - get list of fileinfos for directory
 *-------------------------------------------------------------*/
int  op_listDir (sd, ryo, rox, in, roi)
    int sd;
    struct RyOperation *ryo;
    struct RoSAPinvoke *rox;
    caddr_t in;
    struct RoSAPindication *roi;
{
    register struct type_RFA_FileName *arg = (struct type_RFA_FileName *) in;
    struct type_RFA_FileInfoList *fi;
    char *dir, *s;
    struct RfaInfo *rfalist;
    int rc;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_NOTICE, NULLCP,
	    "RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
	    sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_DEBUG, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    s = qb2str(arg);

    /*--- expand symlinks and get relative path ---*/
    if ((dir = expandSymLinks(s)) == NULL) {
	free(s);
	advise(LLOG_EXCEPTIONS,NULLCP,"getFileInfo: %s", rfaErrStr);
	return strerror(sd, error_RFA_fileAccessError, rfaErrStr, rox, roi);
    }
    free(s);

    if ((rc = getRfaInfoList(dir, &rfalist, NULL)) != OK) 
	return error(sd, error_RFA_fileAccessError, rc, rox, roi);

    /*--- convert to FileInfoList ---*/
    if ((fi = rfa2fil(dir, rfalist)) == NULL) 
	return syserror(sd, error_RFA_miscError, rox, roi);

    /*--- return result ----*/
    if (RyDsResult (sd, rox->rox_id, (caddr_t) fi, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "RESULT");
    free_RFA_FileInfoList(fi);
    
    return OK;
}

