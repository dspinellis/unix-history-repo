/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * getfile.c - get file content from remote
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/getfile.c,v 7.3 91/02/22 09:28:02 mrose Interim $
 *
 * $Log:	getfile.c,v $
 * Revision 7.3  91/02/22  09:28:02  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:54:34  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:06:19  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/getfile.c,v 7.3 91/02/22 09:28:02 mrose Interim $";
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
#include <ctype.h>
#include <strings.h>
#include <pwd.h>
#include <grp.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "logger.h"
#include "RFA-ops.h"
#include "RFA-types.h"
#include "rfa.h"
#include "rfainfo.h"
#include "tailor.h"

extern FILE *out, *err;

/*--------------------------------------------------------------*/
/*  getfile_aux                                                 */
/*--------------------------------------------------------------*/
int getfile_aux(fn, rfa, rmode)
char *fn;
struct RfaInfo *rfa;
int *rmode;
{
    struct type_RFA_GetFileDataRes *gfr;
    struct type_RFA_GetFileDataArg *gfa;
    int num, res, rc;
    time_t ts, te;

    if (getConnection() != OK)  
	return NOTOK_REMOTE_ERROR;

    if ((gfa = (struct type_RFA_GetFileDataArg *)
	    malloc(sizeof(struct type_RFA_GetFileDataArg))) == NULL) 
    {
	fprintf(err,"*** local error : no memory ***\n");
	return NOTOK_LOCAL_ERROR;
    }
    gfa->filename = str2qb(fn, strlen(fn), 1);
    gfa->slaveVersion = rfa->ri_modTime;

    fprintf(err,"requesting from master...");
    fflush(err);
    time(&ts);
    if (invoke(operation_RFA_getFileData, (caddr_t) gfa, &gfr, &res) ==NOTOK) {
	fprintf(err, "\n\t*** remote operation invocation failed ***\n");
	free_RFA_GetFileDataArg(gfa);
	return NOTOK_REMOTE_ERROR;
    }
    free_RFA_GetFileDataArg(gfa);
    if (res != RY_RESULT) {
	fprintf(err, "failed\n");
	printError(res, gfr, &rc);
	return rc;
    }


    /*-- set file characteristics in rfa --*/
    rfa->ri_mode = gfr->fileinfo->mode;
    rfa->ri_modTime = gfr->fileinfo->modTime;
    rfa->ri_size = gfr->fileinfo->size;
    rfa->ri_owner = qb2str(gfr->fileinfo->user);
    rfa->ri_group = qb2str(gfr->fileinfo->group);
    if (IS_MASTER(gfr->fileinfo->status)) {
	SET_STATUS(rfa->ri_status, RI_SLAVE);
	SET_TRANSFER(rfa->ri_status, RI_TRANSFER(gfr->fileinfo->status));
	rfa->ri_mode &= ~0222;
    } else {
	SET_STATUS(rfa->ri_status, RI_UNREGISTERED);
	SET_TRANSFER(rfa->ri_status, default_transfer);
    }

    /*-- create the file --*/
    if ((rc = instfile(fn, gfr, rfa, &num)) != OK) {
	free_RFA_GetFileDataRes(gfr);
	return rc;
    }

    time(&te);
    te = (te - ts) ? te - ts : 1L;
    switch (gfr->mode) {
    	case int_RFA_mode_compressed:
	    fprintf(err, "transfered compressed\n\t(%2.1f Kbytes in %d sec, ",
		(float)(gfr->fileinfo->size) / 1024, te);
	    fprintf(err, "%2.1f Kbytes/s, %2.1f%% compression)\n",
		(float)(gfr->fileinfo->size) / (float)te / 1024,
		(float)((gfr->fileinfo->size - num) * 100) / 
			(float)(gfr->fileinfo->size + 1));
	    break;
    	case int_RFA_mode_actual:
	    fprintf(err, "local file up-to-date\n");
	    break;
    	case int_RFA_mode_zero:
	    fprintf(err, "file is empty\n");
	    break;
	default:
	    fprintf(err,"transfered\n\t(%2.1f Kbytes in %d sec, %2.1f Kbytes/s)\n",
		(float)(gfr->fileinfo->size) / 1024, te,
		(float)(gfr->fileinfo->size) / (float)te / 1024  );
    }

    /*--- set file access mode bits ---*/
    if (changeFileMode(fn, rfa->ri_mode, "set permissions") != OK)
	fprintf(err, "\t*** %s ***\n", rfaErrStr);
    if (changeFileOwner(fn, rfa) != OK)
	fprintf(err, "\t*** %s ***\n", rfaErrStr);

    *rmode = gfr->fileinfo->mode;
    free_RFA_GetFileDataRes(gfr);
    return OK;
}


/*--------------------------------------------------------------*
 *  instfile - install file
 *--------------------------------------------------------------*/
int instfile(fn, gfr, rfa, nump)
    char *fn;
    struct type_RFA_GetFileDataRes *gfr;
    struct RfaInfo *rfa;
    int *nump;
{
    time_t	tt[2];
    char	fnbak[512];
    char	fnz[512];
    char	buf[BUFSIZ];
    struct qbuf	*qp;
    FILE	*f;
    struct stat st;
   
    if (gfr->mode == int_RFA_mode_actual)
	return OK;
	
    /*--- save old file ---*/
    sprintf(fnbak, "%s.bak", makeFN(fn));
    rename(makeFN(fn), fnbak);

    /*--- open new file ---*/
    if ((f = fopen(makeFN(fn), "w")) == NULL) {
	fprintf(err, "failed\n\t*** local error : open of %s failed ***\n", fn);
	unlink (makeFN(fn));
	rename(fnbak, makeFN(fn));
	return NOTOK_LOCAL_ERROR;
    }

    /*--- write new file with data in result ---*/
    if (gfr->mode != int_RFA_mode_zero) {
	*nump = 0;
	for (qp = gfr->data->qb_forw; qp != gfr->data; qp = qp->qb_forw) {
	    *nump += qp->qb_len;
	    if (fwrite(qp->qb_data, 1, qp->qb_len, f) != qp->qb_len) {
		fprintf(err, "failed\n\t*** local error : write of %s failed ***\n",
			fn);
		fclose(f);
		unlink (makeFN(fn));
		rename(fnbak, makeFN(fn));
		return NOTOK_LOCAL_ERROR;
	    }
	}
    }
    fclose(f);

    /*--- see if new file is compressed ---*/
    if (gfr->mode == int_RFA_mode_compressed) {
	sprintf(fnz, "%s.Z", makeFN(fn));
	rename (makeFN(fn), fnz);
	sprintf(buf, "exec uncompress %s", fnz);
	if (system(buf) != 0) {
	    fprintf(err, "failed\n\t*** local error : can't uncompress %s ***\n"			, fn);
	    unlink (fnz);
    	    rename(fnbak, makeFN(fn));
	    return NOTOK_LOCAL_ERROR;
	}
    }

    /*--- check size of transfered file ---*/
    if (stat(makeFN(fn),&st) == -1) {
	fprintf(err, 
		"failed\n\t*** local error : can't stat transfered file ***\n");
	unlink (makeFN(fn));
	rename(fnbak, makeFN(fn));
	return NOTOK_LOCAL_ERROR;
    }
    if (st.st_size != gfr->fileinfo->size) {
	fprintf(err, "failed\n\t*** wrong size after transfer ***\n");
	unlink (makeFN(fn));
	rename(fnbak, makeFN(fn));
	return NOTOK_LOCAL_ERROR;
    }

    /*--- set time of file ---*/
    tt[0] = gfr->fileinfo->modTime;
    tt[1] = gfr->fileinfo->modTime;
    if (utime(makeFN(fn), tt) == -1) {
	fprintf(err, "failed\n\t*** local error : can't set time of  %s ***\n",
		fn);
	unlink (makeFN(fn));
	rename(fnbak, makeFN(fn));
	return NOTOK_LOCAL_ERROR;
    }

    /*--- remove old file ---*/
    if(!backup)
	unlink(fnbak);

    return OK;
}

