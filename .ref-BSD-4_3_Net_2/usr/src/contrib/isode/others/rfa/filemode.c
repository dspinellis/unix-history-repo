/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * filemode.c : manipulate file owner, groups and access mode
 *
 * contributed by: Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/filemode.c,v 7.1 91/02/22 09:28:00 mrose Interim $
 *
 * $Log:	filemode.c,v $
 * Revision 7.1  91/02/22  09:28:00  mrose
 * Interim 6.8
 * 
 * Revision 7.0  91/01/14  13:52:47  mrose
 * *** empty log message ***
 * 
 * Revision 1.1  91/01/04  16:05:06  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/filemode.c,v 7.1 91/02/22 09:28:00 mrose Interim $";
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

#include <ctype.h>
#include <strings.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <errno.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include "rfa.h"
#include "rfainfo.h"


static int runAsRoot = 0;  /* we started with effective uid of root */

/*--------------------------------------------------------------*/
/*  initUserId							*/
/*		called when rfa or rfad is started. Even if     */
/*		they are started with effective uid of root	*/
/*		they are running with eff. uid of user until    */
/*		the eff. uid of root is REALLY needed (see 	*/
/*		below)						*/
/*--------------------------------------------------------------*/
int initUserId(uid, gid, user)
    int uid, gid;
    char *user;
{
    if (geteuid() == 0) {
	runAsRoot++;
	if (setregid (gid, gid) < 0) {
	    sprintf (rfaErrStr, "Can't set group-id %d for %s", gid, user);
	    return NOTOK;
	}
	if(setreuid (-1, uid) < 0) {
	    sprintf (rfaErrStr, "Can't set user-id %d for %s", uid, user);
	    return NOTOK;
    	}
    }
    return OK;
}


/*--------------------------------------------------------------*/
/*  changeFileOwner                                             */
/*--------------------------------------------------------------*/
int changeFileOwner(fn, rfa)
    char *fn;
    struct RfaInfo *rfa;
{
    struct group *gr;
    struct passwd *pw;
    int changedUID = 0;
    int rc = OK;

    if ((doChgrp || doChown) && runAsRoot) 
	if (setreuid(-1, 0) == -1)
	    sprintf(rfaErrStr, "can't change owner/group of %s\n", fn);
	else
	    changedUID++;
		
    if (doChgrp) 
	if ((gr = getgrnam(rfa->ri_group)) == NULL) {
	    sprintf(rfaErrStr, "can't change group to %s (invalid group)", 
			rfa->ri_group); 
	    rc = NOTOK;
	} else
	   if (chown(makeFN(fn), -1, gr->gr_gid) == -1)  {
		sprintf(rfaErrStr, "can't change group to %s (%s)",
			rfa->ri_group, sys_errname(errno)); 
		rc = NOTOK;
	    }
    if (doChown && runAsRoot)
	if ((pw = getpwnam(rfa->ri_owner)) == NULL)  {
	    sprintf(rfaErrStr, "can't change owner to %s (invalid user-name)", 
			rfa->ri_owner); 
	    rc = NOTOK;
	} else
	   if (chown(makeFN(fn), pw->pw_uid, -1) == -1)  {
		sprintf(rfaErrStr, "can't change owner to %s (%s)",
			rfa->ri_owner, sys_errname(errno)); 
		rc = NOTOK;
	    }
    
    if (changedUID) 
	setreuid(-1, getuid());

    return rc;
}

/*--------------------------------------------------------------*/
/*  changeFileMode                                              */
/*--------------------------------------------------------------*/
int changeFileMode(fn, mode, errmsg)
    char *fn;
    int mode;
    char *errmsg;
{
    int changedUID = 0;
    int rc = OK;

    if (runAsRoot) 
	if (setreuid(-1, 0) == -1)  {
	    sprintf(rfaErrStr, "can't %s of file %s", errmsg, fn);
	} else
	    changedUID++;

    /*-- clear set uid on execution bit --*/
    if (doClearSUID)
	mode &= ~S_ISUID;

    if (chmod(makeFN(fn), mode & 07777) == -1) {
	sprintf(rfaErrStr, "can't %s of file %s", errmsg, fn);
	rc = NOTOK;
    }

    if (changedUID) 
	setreuid(-1, getuid());
    return rc;
}


/*--------------------------------------------------------------*/
/*  makeFileReadOnly						*/
/*--------------------------------------------------------------*/
int makeFileReadOnly(fn, rfa)
    struct RfaInfo *rfa;
    char *fn;
{
    if (doChmod && (rfa->ri_mode & 0222)) {
	rfa->ri_mode &= ~0222;
	return changeFileMode(fn, rfa->ri_mode, "clear write permission");
    }
    return OK;
}

/*--------------------------------------------------------------*/
/*  makeFileReadWrite						*/
/*--------------------------------------------------------------*/
int makeFileReadWrite(fn, rfa)
    struct RfaInfo *rfa;
    char *fn;
{
    if (doChmod) {
	rfa->ri_mode |= 0220;
	return changeFileMode(fn, rfa->ri_mode, "set write permission");
    }
    return OK;
}

/*--------------------------------------------------------------*/
/*  changeTime - change local time if eff. uid is root		*/
/*--------------------------------------------------------------*/
int changeTime (dt)
    time_t dt;
{
    struct timeval tv;
    int rc = OK;
    int changedUID = 0;

    if (runAsRoot) 
	if (setreuid(-1, 0) == -1)  {
	    sprintf(rfaErrStr, "can't switch to user ID of ROOT");
	} else
	    changedUID++;
    else {
	sprintf (rfaErrStr, "can't set time myself, using 'rfatime'");
	return NOTOK;
    }

    if (dt > 0) {
	/*--- clock "jumps" forwards ---*/
	gettimeofday(&tv, NULL);
	tv.tv_sec += dt;
	if (settimeofday(&tv, NULL) == -1) {
	    sprintf(rfaErrStr, "can't set time : %s ", sys_errname(errno));
	    rc = NOTOK;
	}
    } else {
	tv.tv_sec = dt;
	tv.tv_usec = 0L;
	if (adjtime(&tv, NULL) == -1) {
	    sprintf(rfaErrStr, "can't set time : %s", sys_errname(errno));
	    rc = NOTOK;
	}
    }

    if (changedUID) 
	setreuid(-1, getuid());

    return rc;
}

