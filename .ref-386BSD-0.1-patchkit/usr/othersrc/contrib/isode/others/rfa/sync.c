/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * sync.c  synchronize local dir with remote site
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/sync.c,v 7.3 91/02/22 09:28:34 mrose Interim $
 *
 * $Log:	sync.c,v $
 * Revision 7.3  91/02/22  09:28:34  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:55:09  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:08:27  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/sync.c,v 7.3 91/02/22 09:28:34 mrose Interim $";
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
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include "RFA-ops.h"	
#include "RFA-types.h"	
#include "rfa.h"
#include "rfainfo.h"

extern FILE *err, *out;
extern int interactive;

char *incstr = "\t*** INCONSISTENCY : ";

/*--------------------------------------------------------------*/
/*  createEmptyFile						*/
/*--------------------------------------------------------------*/
int createEmptyFile(dir, rfa)
    char *dir;
    struct RfaInfo *rfa;
{
    int tt[2], fd;
    char buf[BUFSIZ];
    char fn[BUFSIZ];

    sprintf(fn, "%s%s%s", dir, *(dir+strlen(dir)-1) == '/' ? "" : "/",
		rfa->ri_filename);
    /*-- create empty file for now --*/
    if ((fd = open(makeFN(fn), O_WRONLY | O_CREAT, rfa->ri_mode & 07555)) == -1)
    {
	fprintf(err,"\t*** can't create local SLAVE for %s (%s) ***\n"
		, fn, sys_errname(errno));
	return NOTOK;
    }
    strcpy(buf,"This file has been created by RFA as a dummy.");
    strcat(buf," Use RFA commands 'get' and 'setauto'\nto retrieve");
    strcat(buf," the actual content of this file\n");

    if (write(fd, buf, strlen(buf)) == 0) {
	fprintf(err,"\t*** can't write local SLAVE for %s (%s) ***\n"
		, fn, sys_errname(errno));
	unlink(makeFN(fn));
	close(fd);
	return NOTOK;
    }
    close(fd);
    tt[0] = rfa->ri_modTime - 60;
    tt[1] = rfa->ri_modTime - 60;
    if (utime(makeFN(fn), tt) == -1) {
	fprintf(err,"\t*** can't create local SLAVE for %s (%s) ***\n"
		, fn, sys_errname(errno));
	unlink(makeFN(fn));
	return NOTOK;
    }
    changeFileOwner(fn, rfa);
    return OK;
}

/*--------------------------------------------------------------*/
/*  removeDir - check if dir is empty and remove it		*/
/*--------------------------------------------------------------*/
int removeDir(dir)
    char *dir;
{
    struct dirent *dp;
    DIR *dirp;


    /*--- dir is a directory name, so open dir ---*/
    if ((dirp = opendir(makeFN(dir))) ==  NULL) {
	fprintf(err, "*** can't open %s - %s ***", dir, sys_errname(errno));
	return NOTOK;
    }

    for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	if (strncmp(dp->d_name, "..", 2) && strncmp(dp->d_name, ".", 1)
		&& strncmp(dp->d_name, ".rfainfo", 8))
	{
	    closedir(dirp);
	    fprintf(err, "%scan't remove %s - not empty ***\n", incstr, dir);
	    return NOTOK;
	}
    }
    closedir(dirp);
    fprintf(err,"\tremoved local SLAVE directory '%s'\n", dir);
    return OK;
}

/*--------------------------------------------------------------*/
/*  checkState - check state of files and perform actions to    */
/*               resolve inconsistencies if possible		*/
/*--------------------------------------------------------------*/
int checkState(rfa, rrfa, dir, wrp)
    struct RfaInfo *rfa, *rrfa;
    char *dir;
    int *wrp;
{
    char fn[BUFSIZ];

    if ((rfa == NULL) && (rrfa == NULL))
	return OK;

    sprintf(fn, "%s%s%s", dir, *(dir+strlen(dir)-1) == '/' ? "" : "/", 
		rfa ? rfa->ri_filename : rrfa->ri_filename);
    /*-- checks if file exists only local --*/
    if (rrfa == NULL)
	switch (RI_STATUS(rfa->ri_status)) {
	    case RI_MASTER:
	    case RI_UNREGISTERED:
		return OK;
	    case RI_SLAVE:
		fprintf(err, "%sno remote MASTER for local SLAVE '%s' ***\n"
			    , incstr, rfa->ri_filename);
		if (doRmWidows) {
		    if (rfa->ri_mode & S_IFDIR)
			removeDir(fn);
		    else {
			unlink(makeFN(fn));
			fprintf(err,"\tremoved local SLAVE file '%s'\n",
				rfa->ri_filename);
		    }
		    return OK;
		} else 
		    return NOTOK;
		
	    default:
		fprintf(err, "%sinvalid state for local '%s' ***\n",
			incstr, rfa->ri_filename);
		return NOTOK;
	}

    /*-- checks if file exists only remote --*/
    if (rfa == NULL)
	switch (RI_STATUS(rrfa->ri_status)) {
	    case RI_MASTER:
	    case RI_UNREGISTERED:
		return OK;
	    case RI_SLAVE:
		fprintf(err, "%sno local MASTER for remote SLAVE '%s' ***\n",
			incstr, rrfa->ri_filename);
		return NOTOK;
	    default:
		fprintf(err, "%sinvalid state for remote '%s' ***\n",
			incstr, rrfa->ri_filename);
		return NOTOK;
	}

		
    /*-- checks if file exists at both sides --*/
    switch (RI_STATUS(rfa->ri_status)) {
	case RI_UNREGISTERED:
	    switch (RI_STATUS(rrfa->ri_status)) {
		case RI_UNREGISTERED:
		    return OK;
		case RI_SLAVE:
		    fprintf(err, "%sUNREG/SLAVE state for '%s' ***\n",
			    incstr, rrfa->ri_filename);
		    return NOTOK;
		case RI_MASTER:
		    fprintf(err, "%sUNREG/MASTER state for '%s' ***\n", 
			    incstr, rrfa->ri_filename);
		    return NOTOK;
		default:
		    fprintf(err, "%sinvalid state for remote '%s' ***\n",
			    incstr, rrfa->ri_filename);
		    return NOTOK;
	    }
	    /* NOTREACHED */
	case RI_MASTER:
	    switch (RI_STATUS(rrfa->ri_status)) {
		case RI_UNREGISTERED:
		    fprintf(err, "%sMASTER/UNREG state for '%s' ***\n",
			    incstr, rrfa->ri_filename);
		    return NOTOK;
		case RI_SLAVE:
		    return OK;
		case RI_MASTER:
		    fprintf(err, "%sMASTER/MASTER state for '%s' ***\n", 
			    incstr, rrfa->ri_filename);
		    return NOTOK;
		default:
		    fprintf(err, "%sinvalid state for remote '%s' ***\n",
			    incstr, rrfa->ri_filename);
		    return NOTOK;
	    }
	    /* NOTREACHED */
	case RI_SLAVE:
	    switch (RI_STATUS(rrfa->ri_status)) {
		case RI_UNREGISTERED:
		    fprintf(err, "%sSLAVE/UNREG state for '%s' ***\n",
			    incstr, rrfa->ri_filename);
		    if (doRmWidows) {
			SET_STATUS(rfa->ri_status, RI_UNREGISTERED);
			SET_TRANSFER(rfa->ri_status, RI_TR_REQ);
			fprintf(err,"\tchanged status of '%s' to UNREGISTERED\n"
				, rfa->ri_filename);
			*wrp = 1;
		    }
		    return NOTOK;
		    
		case RI_SLAVE:
		    fprintf(err, "%sSLAVE/SLAVE state for '%s' ***\n", 
			    incstr, rrfa->ri_filename);
		    return NOTOK;
		case RI_MASTER:
		    return OK;
		default:
		    fprintf(err, "%sinvalid state for remote '%s' ***\n",
			    incstr, rrfa->ri_filename);
		    return NOTOK;
	    }

	    /* NOTREACHED */
	default:
	    fprintf(err, "%sinvalid state for local '%s' ***\n",
		    incstr, rrfa->ri_filename);
	    return NOTOK;
    }
    /* NOTREACHED */
}

/*--------------------------------------------------------------*/
/*  checkMasterSlave - do a consistency check on master and     */
/*		       slave file infos				*/
/*--------------------------------------------------------------*/
int checkMasterSlave(m, s, ms, ss)
    struct RfaInfo *m, *s;
    char *ms, *ss;
{
    if (m->ri_modTime < s->ri_modTime) {
	fprintf(err,"%s %s SLAVE version of '%s' is newer than %s MASTER ***\n"
		, incstr, ss, m->ri_filename, ms);
	return NOTOK;
    }
    if ((m->ri_modTime == s->ri_modTime) && (m->ri_size != s->ri_size)) {
	fprintf(err,"%s %s MASTER of '%s'", incstr, ms, m->ri_filename);
	fprintf(err," has different size than %s SLAVE ***\n", ss);
	return NOTOK;
    }
    return OK;
}

/*--------------------------------------------------------------*/
/*  handleDir							*/
/*--------------------------------------------------------------*/
int handleDir(dir, localRfaListPtr, rrfa, rec, wrp)
    char *dir;
    struct RfaInfo **localRfaListPtr;
    struct RfaInfo *rrfa;
    int rec;
    int *wrp;
{
    struct RfaInfo *rfa;
    char fn[BUFSIZ];
    int rc;

    if (!strcmp(rrfa->ri_filename, ".") || !strcmp(rrfa->ri_filename, ".."))
	return OK;

    sprintf(fn, "%s/%s", strcmp(dir,"/") ? dir : "", rrfa->ri_filename);
    rfa = findRfaInfo(rrfa->ri_filename, *localRfaListPtr);
	
    switch (RI_STATUS(rrfa->ri_status)) {
	case RI_UNREGISTERED:
	    if (rfa)
		checkState(rfa, rrfa, dir, wrp);
	    return OK;

    	case RI_SLAVE:
	    if (rfa == NULL) {
		fprintf(err,"%sno local MASTER for remote SLAVE dir '%s' ***\n",
			  incstr, rrfa->ri_filename);
		return NOTOK_INCONSISTENCY;
	    }
	    if ((rfa->ri_mode & S_IFDIR & S_IFMT) == 0) {
		fprintf(err,
		    "%slocal file '%s' conflicts with remote directory ***\n",
			  incstr, rrfa->ri_filename);
		return NOTOK_INCONSISTENCY;
	    }
	    if (checkState(rfa, rrfa, dir, wrp) == NOTOK)
		return NOTOK_INCONSISTENCY;
	    break;

	case RI_MASTER:
	    if (rfa == NULL) {
		fprintf(err, "found new MASTER sub-directory '%s' at remote\n", 
			    rrfa->ri_filename);
		if ((rfa = mallocRfaInfo(strdup(rrfa->ri_filename))) == NULL) {
		    fprintf(err,"%scant't create local SLAVE for %s ***\n", 
				incstr, rrfa->ri_filename);
		    return NOTOK_LOCAL_ERROR;
		}
		if (mkdir(makeFN(fn), rrfa->ri_mode & 07777) == -1) {
		    fprintf(err,"*** can't create subdir %s ***\n", 
				rrfa->ri_filename);
		    return NOTOK_FILEACCESS;
		}
		SET_STATUS(rfa->ri_status, RI_SLAVE);
		SET_LOCKINFO(rfa->ri_status, RI_UNLOCKED);
		SET_TRANSFER(rfa->ri_status, RI_TRANSFER(rrfa->ri_status));
		time(&(rfa->ri_lastChange));
		rfa->ri_modTime = 0L;
		rfa->ri_mode = rrfa->ri_mode;
		rfa->ri_next = *localRfaListPtr;
		*localRfaListPtr = rfa;
		changeFileOwner(fn, rrfa);
		if (putRfaInfoList(dir, *localRfaListPtr) != OK) {
		    fprintf(err,"%scan't set SLAVE status of %s ***\n", incstr, 
			    rrfa->ri_filename);
		    return NOTOK_FILEACCESS;
		}
	    } else {  /*--- local rfa found ---*/
		if((rfa->ri_mode & S_IFDIR & S_IFMT) == 0) {
		    fprintf(err,"%slocal file '%s' conflicts with",incstr,
			    rrfa->ri_filename);
		    fprintf(err," remote directory ***\n");
		    return NOTOK_INCONSISTENCY;
		}
		if (checkState(rfa, rrfa, dir, wrp) == NOTOK)
		    return NOTOK_INCONSISTENCY;
	    
	    } /* if local rfa exists */
    } /* switch rrfa->ri_status */

    /*--- now we have a local version of the dir ---*/
    if(rec && IS_TR_AUTO(rfa->ri_status))  
	if ((rc = syncDir(fn, rec)) != OK)  {
	    fprintf(err,"\t*** syncdir for %s failed ***\n", rrfa->ri_filename);
	    return rc;
	}
    
    return OK;
} 


/*--------------------------------------------------------------*/
/*  syncDir							*/
/*--------------------------------------------------------------*/
int syncDir (dir, rec)
char *dir;
    int rec;
{
    struct RfaInfo *rfa, *localRfaList, *rrfa, *remoteRfaList;
    char buf[BUFSIZ];
    int writeList, rc;
    char *l, *lp;
    struct stat st;
    int rmode;
    char fn[BUFSIZ];
    char syncfiles[BUFSIZ*10];


    /*--- get file Info ---*/
    if ((rc = getLockedRfaInfoList(dir, &localRfaList, NULL)) != OK)  {
	fprintf(err,"\t*** can't get rfainfo : %s ***\n", errMsg(rc));
	return(rc);
    }

    /*--- get remote rfa list ---*/
    if ((rc = getRemoteRfaInfoList(dir, &remoteRfaList)) != OK) {
	releaseRfaInfoList(dir, localRfaList);
	return rc;
    }
    fprintf(err, "syncing directory %s\n", dir);
    *syncfiles = '\0';

    /*-- check remote list agaist local one --*/
    for(rrfa = remoteRfaList; rrfa; rrfa = rrfa->ri_next) {
	writeList = 0;
	switch(rrfa->ri_mode & S_IFMT) {
	    case S_IFIFO:
	    case S_IFCHR:
	    case S_IFBLK:
	    case S_IFSOCK:
		continue;

	    case S_IFLNK:
		if (rrfa->ri_lnkName == NULL)
		    continue;
		    
		strcpy(buf, makeFN2(dir, rrfa->ri_filename));
		if (*(rrfa->ri_lnkName) == '/') 
		    lp = l = makeFN(rrfa->ri_lnkName);
		else {
		    l = rrfa->ri_lnkName;
		    lp = makeFN2(dir, rrfa->ri_lnkName);
		}

		if (stat(lp, &st) == -1)
		    continue;

		if ((rfa = findRfaInfo(rrfa->ri_filename,localRfaList)) == NULL)
		{
		    fprintf(err, "\tcreating link %s to %s\n", 
			    rrfa->ri_filename, l);
		    if(symlink(l, buf) == -1) {
			fprintf(err,"\t*** can't create link %s ***\n",buf);
			continue;
		    }
		    continue;
		}
		if ((rfa->ri_mode & S_IFMT) == S_IFLNK) {
		    if(strcmp(rfa->ri_lnkName, l) == 0)
			continue;
		    unlink(makeFN2(dir, rfa->ri_filename));
		    fprintf(err, "\tcreating link %s to %s\n", 
			    rrfa->ri_filename, l);
		    if(symlink(l, buf) == -1) {
			fprintf(err,"\t*** can't create link %s ***\n",buf);
			continue;
		    }
		    continue;
		}
		fprintf(err,"%slocal file %s conflicts with remote link ***\n",
			incstr, buf);
		continue;
		break;

				
	    case S_IFDIR:
		if (handleDir(dir, &localRfaList, rrfa,rec,&writeList) != OK) {
		    continue;
		}
		continue;

	    case S_IFREG:
		break;

	    default:
		continue;
	}

	/*--- rrfa->ri_filename is regular file ---*/

	rfa = findRfaInfo(rrfa->ri_filename, localRfaList);

	switch (RI_STATUS(rrfa->ri_status)) {
	    case RI_UNREGISTERED:
		checkState(rfa, rrfa, dir, &writeList);
		break;
	    case RI_SLAVE:
		if (checkState(rfa, rrfa, dir, &writeList) == NOTOK)
		    continue;
		checkMasterSlave(rfa, rrfa, "local", "remote");
		break;

	    case RI_MASTER:
		if (rfa == NULL) {

		    /*--- check if not a .rfaexec file ---*/
		    if (strcmp(rrfa->ri_filename, ".rfaexec") == 0) {
			fprintf(err,
			    "%sremote file '%s' is MASTER, not transfered ***\n"
			    ,rfa->ri_filename);
			continue;
		    }

		    fprintf(err, "\tfound new MASTER file '%s' at remote\n", 
			    rrfa->ri_filename);
		    if ((rfa=mallocRfaInfo(strdup(rrfa->ri_filename)))==NULL) {
			fprintf(err,"%scant't create local SLAVE for %s ***\n",
				incstr, rrfa->ri_filename);
			continue;
		    }
		    SET_STATUS(rfa->ri_status, RI_SLAVE);
		    SET_LOCKINFO(rfa->ri_status, RI_UNLOCKED);
		    SET_TRANSFER(rfa->ri_status, RI_TRANSFER(rrfa->ri_status));
		    time(&(rfa->ri_lastChange));
		    rfa->ri_modTime = 0L;
		    rfa->ri_mode = rrfa->ri_mode;
		    rfa->ri_next = localRfaList;
		    localRfaList = rfa;
		    writeList++;
		} else {
		    if (checkState(rfa, rrfa, dir, &writeList) == NOTOK)
			break;;
		    checkMasterSlave(rrfa, rfa, "remote", "local");
		} /* if local rfa exists */

		/*--- now we are ready to get the remote file ---*/
		if (writeList && IS_TR_REQ(rfa->ri_status)) {
		    if (createEmptyFile(dir, rrfa) == NOTOK) 
			continue;
		} else 
		    if( IS_TR_AUTO(rfa->ri_status) && 
			    (rfa->ri_modTime < rrfa->ri_modTime)) 
		    {
			sprintf(fn, "%s%s%s", dir, 
				*(dir+strlen(dir)-1) == '/' ? "" : "/",
				rfa->ri_filename);
			fprintf(err, "\t%s - ", rrfa->ri_filename);
			if ((rc = getfile_aux(fn, rfa, &rmode)) != OK)  
			    continue;
			strcat(syncfiles, rfa->ri_filename);
			strcat(syncfiles, " ");
		    } 
		break;

	    default:
		fprintf(err, "%sinvalid state for remote '%s' ***\n",
			    incstr, rrfa->ri_filename);
		continue;	
	} /* switch rrfa->ri_status */

	/*--- now we have a local slave version of the file ---*/
	if (writeList)
	    if ((rc = putRfaInfoList(dir, localRfaList)) != OK) {
		fprintf(err,"%scan't set SLAVE status of %s ***\n", incstr, 
			rfa->ri_filename);
		continue;
	    }
	
    } /* for rrfa */


    /*-- now check local list against remote list --*/
    for(rfa = localRfaList; rfa; rfa = rfa->ri_next) 
	if (findRfaInfo(rfa->ri_filename, remoteRfaList) == NULL) {
	    checkState(rfa, NULL, dir, &writeList);
	}
    if (writeList)
	if ((rc = putRfaInfoList(dir, localRfaList)) != OK) {
	    fprintf(err,"%scan't set SLAVE status of %s ***\n", incstr, 
		    rfa->ri_filename);
	}

    releaseRfaInfoList(dir, localRfaList);
    freeRfaInfoList(remoteRfaList);

    /*-- look for .rfaexec script --*/
    return rfaMake(dir, syncfiles);
}

/*--------------------------------------------------------------*/
/*  rfaMake							*/
/*--------------------------------------------------------------*/
int rfaMake(dir, fns)
    char *dir, *fns;
{
    struct RfaInfo *rfa, *rrfa, *localRfaList, *remoteRfaList;
    char parentDir[BUFSIZ];
    char execfn[BUFSIZ];
    char buf[BUFSIZ];
    struct stat st;
    int rc;

    /*-- check if files sync'ed --*/
    if (*fns == '\0' || doRfaExec == 0)
	return OK;

    /*-- check if .rfaexec exists --*/
    sprintf(execfn, "%s/.rfaexec", dir);
    if (lstat(makeFN(execfn),&st) == -1) {
        if (errno == ENOENT) 
	    return OK;
	fprintf(err, "\t*** can't stat %s - %s", execfn,sys_errname(errno));
	return NOTOK_LOCAL_ERROR;
    }
    if (access(makeFN(execfn), X_OK) == -1)  {
	fprintf(err,"\t*** can't exec '%s' (%s) ***\n", execfn, 
		sys_errname(errno));
	return NOTOK_LOCAL_ERROR;
    }

    /*-- check if dir is locked --*/
    if ((rc = getRfaInfoList(dirname(dir), &localRfaList)) != OK) {
	return rc;
    }
    if (rfa = findRfaInfo(basename(dir),localRfaList)) {
	if (IS_MASTER(rfa->ri_status) && IS_LOCKED(rfa->ri_status)) {
	    freeRfaInfoList(localRfaList);
	    return OK;
	}
    }
    if ((rfa == NULL) || (rfa && !IS_MASTER(rfa->ri_status))) {
	freeRfaInfoList(localRfaList);

	/*--- get remote rfa list ---*/
	if ((rc = getRemoteRfaInfoList(dirname(dir), &remoteRfaList)) != OK) {
	    return rc;
	}
	if ((rrfa = findRfaInfo(basename(dir),remoteRfaList)) == NULL) {
	    fprintf(err,"\t*** can't determine status of '%s' ***\n", dir);
	    freeRfaInfoList(remoteRfaList);
	    return NOTOK_REMOTE_ERROR;
	}
	if (!IS_MASTER(rrfa->ri_status)) {
	    fprintf(err,"\t*** can't find MASTER version of '%s' ***\n", dir);
	    freeRfaInfoList(remoteRfaList);
	    return NOTOK_REMOTE_ERROR;
	}
	if (IS_LOCKED(rrfa->ri_status))  {
	    freeRfaInfoList(remoteRfaList);
	    return OK;
	}
	freeRfaInfoList(remoteRfaList);
    }

    /*-- change cwd to sync'ed dir --*/
    if (chdir(makeFN(dir)) == -1) {
	fprintf(err,"\t*** can't change dir to '%s' (%s) ***\n", dir,
		sys_errname(errno));
	return NOTOK_LOCAL_ERROR;
    }

    /*-- so we are able to exec .rfaexec --*/
    fprintf(err,"\texecuting '%s'...\n", execfn);
    sprintf(buf, "%s %s", makeFN(execfn), fns);
    if (system(buf) == -1)
	return NOTOK_LOCAL_ERROR;

    return OK;
}
		    
