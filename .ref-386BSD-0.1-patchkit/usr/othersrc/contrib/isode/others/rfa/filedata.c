/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * filedata.c : operation to transfer the content of a file
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/filedata.c,v 7.1 91/02/22 09:27:56 mrose Interim $
 *
 * $Log:	filedata.c,v $
 * Revision 7.1  91/02/22  09:27:56  mrose
 * Interim 6.8
 * 
 * Revision 7.0  91/01/14  13:52:45  mrose
 * *** empty log message ***
 * 
 * Revision 1.1  91/01/04  16:04:30  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/filedata.c,v 7.1 91/02/22 09:27:56 mrose Interim $";
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
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <varargs.h>
#include "ryresponder.h"    /* for generic idempotent responders */
#include "psap.h"   /* for generic idempotent responders */
#include "RFA-ops.h"        /* operation definitions */
#include "RFA-types.h"  /* type definitions */
#include "rfainfo.h"
#include "rfa.h"

/*--------------------------------------------------------------
 *  spawn compress
 *-------------------------------------------------------------*/
int getCompressed(fn, qbp)
   char *fn;
   struct qbuf **qbp;
{
    static int p[2];
    int rc;

    if (pipe(p) == -1) {
	advise(LLOG_EXCEPTIONS,NULLCP,"spawnCompress: pipe failed");
	return 0;
    }

    switch (fork()) {
	case -1:
		advise(LLOG_EXCEPTIONS,NULLCP,"spawnCompress: fork failed");
		return 0;
		break;
	case 0: /* son */
		close(1);
		close(p[0]);
		if (dup2(p[1], 1) == -1)
		    exit(1);
		execl("/usr/ucb/compress", "/usr/ucb/compress", "-c", fn, NULL);
		advise(LLOG_EXCEPTIONS,NULLCP,"spawnCompress: exec failed %s",
			sys_errname(errno));
		exit(1);
	default:
		break;
    }

    close (p[1]);

    rc = fd2qb(p[0], qbp);
    close (p[0]);

    return rc;
}


/*--------------------------------------------------------------
 *  fd2qb - read data from fd and create qbuf list
 *-------------------------------------------------------------*/
int fd2qb(fd, qbp)
    int fd;
    struct qbuf **qbp;
{
    struct qbuf *qb;
    int c, tot, head = 1;
    char buf[BUFSIZ * 100];

    tot = 0;
    while ( c = read(fd, buf, sizeof(buf))) {
	tot += c;
	if (head) {
	   (*qbp) = str2qb(buf, c, 1);
	   head = 0;
	} else {
	    qb = str2qb(buf, c, 0);
	    insque(qb, (*qbp)->qb_back);
	}
    }
    return tot;
}


/*--------------------------------------------------------------
 *  op_getFileData - get mastership of a file
 *-------------------------------------------------------------*/
op_getFileData (sd, ryo, rox, in, roi)
int sd;    
struct RyOperation *ryo;
struct RoSAPinvoke *rox;
caddr_t in;
struct RoSAPindication *roi;
{
    struct type_RFA_GetFileDataArg *gfa = (struct type_RFA_GetFileDataArg *)in;
    struct type_RFA_GetFileDataRes gfr;
    struct RfaInfo *rfa, *rfalist;
    char buf[BUFSIZ * 100];
    char *s, *fn;
    int bytes = 0, rc, c, fd;
    time_t v;

    if (rox -> rox_nolinked == 0) {
	advise (LLOG_NOTICE, NULLCP,
	    "RO-INVOKE.INDICATION/%d: %s, unknown linkage %d",
	    sd, ryo -> ryo_name, rox -> rox_linkid);
	return ureject (sd, ROS_IP_LINKED, rox, roi);
    }
    advise (LLOG_DEBUG, NULLCP, "RO-INVOKE.INDICATION/%d: %s",
	    sd, ryo -> ryo_name);

    if((s = qb2str(gfa->filename)) == NULL)
	return NOTOK_SYS;

    /*--- expand symlinks and get relative path ---*/
    if ((fn = expandSymLinks(s)) == NULL) {
	free(s);
	advise(LLOG_EXCEPTIONS,NULLCP,"getFileData: %s", rfaErrStr);
	return strerror(sd, error_RFA_fileAccessError, rfaErrStr, rox, roi);
    }
    free(s);

    /*--- get file info (begin of critical region) ---*/
    if ((rc = getLockedRfaInfoList(dirname(fn), &rfalist,basename(fn))) != OK) {
	return error(sd, error_RFA_fileAccessError, rc, rox, roi);
    }
    if ((rfa = findRfaInfo(basename(fn), rfalist)) == NULL) {
	releaseRfaInfoList(dirname(fn), rfalist);
	return strerror(sd, error_RFA_fileAccessError, "no such file",rox, roi);
    }

    /*--- check if regular file ---*/
    if ((rfa->ri_mode & S_IFMT) != S_IFREG) {
	releaseRfaInfoList(dirname(fn), rfalist);
	advise(LLOG_NOTICE,NULLCP,"getFileData: not regular file  %s", fn);
	return statusError(sd, int_RFA_reason_notRegularFile, NULL,0L,rox,roi);
    }

    /*--- see if we are slave for file ---*/
    if (IS_SLAVE(rfa->ri_status)) {
	releaseRfaInfoList(dirname(fn), rfalist);
	advise(LLOG_EXCEPTIONS,NULLCP,"getFileData: not master for %s", fn);
	return statusError(sd, int_RFA_reason_notMaster, NULL, 0L, rox, roi);
    }

    unlock_rfainfo(dirname(fn));

    advise (LLOG_DEBUG, NULLCP, "getFileData: mv=%ld sv=%d", 
	rfa->ri_modTime, gfa->slaveVersion);

    /*--- see if proposed version is actual ---*/
    v = gfa->slaveVersion;
    if (v > rfa->ri_modTime) {
	freeRfaInfoList(rfalist);
	advise(LLOG_EXCEPTIONS,NULLCP,"reqMaster: slave newer than master of %s"
		, fn);
	return statusError(sd, int_RFA_reason_slaveNewer, NULL,0L,rox,roi);
    }
    if (v == rfa->ri_modTime) {
	gfr.mode = int_RFA_mode_actual;
	gfr.data = NULL;
    } else {
	if (rfa->ri_size == 0) {
	    gfr.mode = int_RFA_mode_zero;
	    gfr.data = NULL;
	} else {
	    /*--- get file data ---*/
	    fd = 0;
	    if (rfa->ri_size > compLimit) {
		/*--- open fd as stdout of background compress ---*/
		advise (LLOG_DEBUG, NULL,"getFileData: size=%d,send compressed",
			    rfa->ri_size);
		gfr.mode = int_RFA_mode_compressed;
		if ((bytes = getCompressed(makeFN(fn), &(gfr.data))) == 0) {
		    advise (LLOG_EXCEPTIONS,NULL,"getFileData: compress fails");
		}
	    } 
	    if (bytes == 0) {
		advise (LLOG_DEBUG, NULLCP, "getFileData: size=%d, send orig",
			    rfa->ri_size);
		if ((fd = open (makeFN(fn), O_RDONLY)) == -1) {
		    advise(LLOG_EXCEPTIONS,NULLCP,"getFileData: can't open %s",
				fn);
		    freeRfaInfoList(rfalist);
		    return syserror(sd, error_RFA_miscError, rox, roi);
		}
		gfr.mode = int_RFA_mode_data;
		if ((bytes = fd2qb(fd, &(gfr.data))) == 0) {
		    advise (LLOG_EXCEPTIONS, NULLCP,"getFileData: read failed");
		    freeRfaInfoList(rfalist);
		    close(fd);
		    return strerror(sd, error_RFA_miscError, 
			"can't read file", rox, roi);
		}
		close(fd);
	    }
	    advise (LLOG_DEBUG, NULLCP, "getFileData: send %d bytes", bytes);
	}
    }
    gfr.fileinfo = rfa2fi(dirname(fn), rfa);

    /*--- return result ---*/
    if (RyDsResult (sd, rox->rox_id, (caddr_t)&gfr, ROS_NOPRIO,
       		roi) == NOTOK)
    { 
	if (gfr.data)
	    qb_free(gfr.data);
	freeRfaInfoList(rfalist);
	free_RFA_FileInfo(gfr.fileinfo);
	ros_adios (&roi -> roi_preject, "RESULT");
    }
    if (gfr.data)
	qb_free(gfr.data);
    freeRfaInfoList(rfalist);
    free_RFA_FileInfo(gfr.fileinfo);
     
    return OK;
}
