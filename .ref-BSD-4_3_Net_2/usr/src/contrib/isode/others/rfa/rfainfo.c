/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * rfainfo.c : functions to manipulate fileinfo structure and files
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/rfainfo.c,v 7.3 91/02/22 09:28:23 mrose Interim $
 *
 * $Log:	rfainfo.c,v $
 * Revision 7.3  91/02/22  09:28:23  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:55:00  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:07:48  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/rfainfo.c,v 7.3 91/02/22 09:28:23 mrose Interim $";
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
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <ctype.h>
#include <pwd.h>
#include <grp.h>
#include <dirent.h>
#include "psap.h"
#include "logger.h"
#include "rfainfo.h"
#include "rfa.h"

#define MAXSTAT 4
static char *statusTable[] = { "UNREGISTERED", "MASTER","MASTER_LOCKED", NULL};
static char *statusTableShort[] = { "NON", "MST","LCK", "SLV", NULL };
char rfaErrStr[512];

extern char *sys_errname();

/*------------------------------------------------------
 * str2status - convert status string to integer code
 *------------------------------------------------------*/
int str2status (stat)
    char *stat;
{
    char **sp;
    int i = 0;

    switch(*(stat++)) {
	case '-':
	    SET_STATUS(i, RI_UNREGISTERED); 
	    break;
	case 'M':
	case 'm':
	    SET_STATUS(i, RI_MASTER); 
	    break;
	case 'S':
	case 's':
	    SET_STATUS(i, RI_SLAVE); 
	    break;
	default:
	    return NOTOK;
    }
    switch(*(stat++)) {
	case '-':
	    SET_LOCKINFO(i, RI_UNLOCKED); 
	    break;
	case 'L':
	case 'l':
	    SET_LOCKINFO(i, RI_LOCKED); 
	    break;
	default:
	    return NOTOK;
    }
    switch(*(stat++)) {
	case '-':
	    SET_TRANSFER(i, RI_TR_REQ); 
	    break;
	case 'A':
	case 'a':
	    SET_TRANSFER(i, RI_TR_AUTO); 
	    break;
	default:
	    return NOTOK;
    }
    return i;
}

/*------------------------------------------------------
 * status2str - convert status to string
 *------------------------------------------------------*/
char *status2str(stat)
    int stat;
{
    static char sbuf[5];
    register char *s = sbuf;

    switch(RI_STATUS(stat)) {
	case RI_UNREGISTERED: 
		*s = '-';
		break;
	case RI_MASTER: 
		*s = 'M';
		break;
	case RI_SLAVE: 
		*s = 'S';
		break;
	default:
		sbuf[0] = '?';
    }
    s++;
    switch(RI_LOCKINFO(stat)) {
	case RI_LOCKED: 
		*s = 'L';
		break;
	case RI_UNLOCKED: 
		*s = '-';
		break;
	default:
		*s = '?';
    }
    s++;
    switch(RI_TRANSFER(stat)) {
	case RI_TR_AUTO: 
		*s = 'A';
		break;
	case RI_TR_REQ: 
		*s = '-';
		break;
	default:
		*s = '?';
    }
    *(++s) = '\0';
    return sbuf;
}

/*------------------------------------------------------
 * mallocRfaInfo - malloc list elements
 *------------------------------------------------------*/
struct RfaInfo *mallocRfaInfo (fn)
    char *fn;
{
    register struct RfaInfo *rfa;

    if ((rfa = (struct RfaInfo *) malloc(sizeof(struct RfaInfo))) == NULL) {
	sprintf(rfaErrStr, "out of memory");
	advise(LLOG_EXCEPTIONS,NULLCP,rfaErrStr);
	return NULL;
    }
    bzero(rfa, sizeof(struct RfaInfo));
    rfa->ri_filename = fn;
    return rfa;
}

	
/*------------------------------------------------------
 * freeRfaInfoList - free list elements
 *------------------------------------------------------*/
void freeRfaInfoList (rfa)
   struct RfaInfo *rfa;
{
   struct RfaInfo *r;

   for (; rfa; rfa = r) {
	if (rfa->ri_filename)
	    free(rfa->ri_filename);
	if (rfa->ri_lckname)
	    free(rfa->ri_lckname);
	if (rfa->ri_owner)
	    free(rfa->ri_owner);
	if (rfa->ri_group)
	    free(rfa->ri_group);
	r = rfa->ri_next;
	free((char *)rfa);
    }
}


/*------------------------------------------------------
 * lock_rfainfo - semaphore P operation
 *------------------------------------------------------*/
void lock_rfainfo(fn)
    char *fn;
{
    int fd;

    if ((fd =open(makeFN2(fn, ".rfainfo"), O_RDWR)) == -1) {
	advise (LLOG_EXCEPTIONS, NULLCP, "lock: can't open '%s':%s", fn, 
		sys_errname(errno));
	return;
    }
    if (flock(fd, LOCK_EX) == -1) 
	advise (LLOG_EXCEPTIONS, NULLCP, "lock: can't lock '%s':%s", fn, 
		sys_errname(errno));
    close(fd);
}

/*------------------------------------------------------
 * unlock_rfainfo - semaphore V operation
 *------------------------------------------------------*/
void unlock_rfainfo(fn)
    char *fn;
{
    int fd;

    if ((fd = open(makeFN2(fn, ".rfainfo"), O_RDWR)) == -1) {
	advise (LLOG_EXCEPTIONS, NULLCP, "unlock: can't open '%s':%s", fn, 
		sys_errname(errno));
	return;
    }
    if (flock(fd, LOCK_UN) == -1) 
	advise (LLOG_EXCEPTIONS, NULLCP, "unlock: can't lock '%s':%s", fn, 
		sys_errname(errno));
    close(fd);
}

/*------------------------------------------------------
 * releaseRfaInfoList - unlock list and free it
 *------------------------------------------------------*/
void releaseRfaInfoList(fn, rfa)
    char *fn;
    struct RfaInfo *rfa;
{
    unlock_rfainfo(fn);
    freeRfaInfoList(rfa);
}

/*------------------------------------------------------
 * statFile - return RfaInfo with results of stat(2) for "fn"
 *------------------------------------------------------*/
int statFile(fn, rfa)
    char *fn;
    struct RfaInfo *rfa;
{
    struct stat st;
    struct group *gr, *getgrgid();
    struct passwd *pw, *getpwuid();
    char   lnkbuf[BUFSIZ];
    char   *f, buf[100];
    int    rc;
    static char lastuname[100], lastgname[100];
    static int lastuid = -1, lastgid = -1;

    /*advise (LLOG_DEBUG, NULLCP, "statFile:  '%s'", fn);*/

    if (lstat(fn,&st) == -1) {
	advise (LLOG_EXCEPTIONS, NULLCP, "cant stat '%s':%s", fn,
		sys_errname(errno));
	sprintf(rfaErrStr, "%s - %s", fn, sys_errname(errno));
	return NOTOK;
    }
    rfa->ri_size = st.st_size;
    rfa->ri_accTime = st.st_atime;
    rfa->ri_modTime = st.st_mtime;
    rfa->ri_mode = st.st_mode;

    /*--- get user and group of owner ---*/
    if(st.st_uid == lastuid)
	rfa->ri_owner = strdup(lastuname);
    else 
	if ((pw = getpwuid (lastuid = st.st_uid)) == NULL) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "Unknown user-id '%d'", lastuid);
	    sprintf(lastuname, "user-id %d", lastuid);
	    rfa->ri_owner = strdup(lastuname);
	} else {
	    rfa->ri_owner = strdup(pw->pw_name);
	    strcpy(lastuname, pw->pw_name);
	}
    if(st.st_gid == lastgid)
	rfa->ri_group = strdup(lastgname);
    else 
	if ((gr = getgrgid (lastgid = st.st_gid)) == NULL) {
	    advise (LLOG_EXCEPTIONS, NULLCP, "Unknown group-id '%d'", lastgid);
	    sprintf(buf, "group-id %d", st.st_gid);
	    rfa->ri_group = strdup(buf);
	} else {
	    rfa->ri_group = strdup(gr->gr_name);
	    strcpy(lastgname, gr->gr_name);
	}

    /*--- read symbolic links ---*/
    if ((rfa->ri_mode & S_IFMT) == S_IFLNK) {
	if ((rc = readlink(fn, lnkbuf, sizeof lnkbuf)) == -1) 
	    sprintf(lnkbuf, "(error reading link)");
	else
	    lnkbuf[rc] = '\0';
	rfa->ri_lnkName = strdup(lnkbuf);
    }
    
    return OK;
}


/*------------------------------------------------------
 * getFileLockInfo - read RFA file info from ".rfainfo"
 *------------------------------------------------------*/
int getFileLockInfo(dirname, rfap)
    char *dirname;
    struct RfaInfo **rfap;
{
    FILE *f;

    register char *s, *d;
    char buf[BUFSIZ];
    char line[BUFSIZ];
    time_t version, lcksince;
    int rc = 1;
    struct RfaInfo *rfa;

    advise(LLOG_DEBUG,NULLCP,"getFileLockInfo: %s",dirname);

    if ((f = fopen(makeFN2(dirname, ".rfainfo"), "r")) == NULL) {
	if (errno != ENOENT) {
	    return NOTOK_SYS;
	}
	else
	    return OK;
    }

    while (fgets(line, sizeof(line), f)) {
	s = line;

	while (isspace(*s))
	    s++;
	for(d = buf; ! isspace(*s); s++, d++)
	    *d = *s;
	*d = '\0';
	if ((rfa = findRfaInfo(buf, *rfap)) == NULL) 
	    continue;

	while (isspace(*s))
	    s++;
	for(d = buf; ! isspace(*s); s++, d++)
	    *d = *s;
	*d = '\0';
	if ((rc = str2status(buf)) == NOTOK) {
	    sprintf(rfaErrStr, "invalid status in rfainfo '%s'",
		makeFN2(dirname, ".rfainfo"));
	    advise(LLOG_EXCEPTIONS,NULLCP,rfaErrStr);
	    continue;
	}
	rfa->ri_status = rc;

	while (isspace(*s))
	    s++;
	for(d = buf; ! isspace(*s); s++, d++)
	    *d = *s;
	*d = '\0';
	rfa->ri_lastChange = atol(buf);

	while (isspace(*s))
	    s++;
	for(d = buf; ! isspace(*s); s++, d++)
	    *d = *s;
	*d = '\0';
	if (strcmp(buf, "NONE"))
	    rfa->ri_lckname = strdup(buf);
	else
	    rfa->ri_lckname = NULL;
		

	while (isspace(*s))
	    s++;
	for(d = buf; ! isspace(*s); s++, d++)
	    *d = *s;
	*d = '\0';
	rfa->ri_lcksince = atol(buf);

    }  /*-- while fgets --*/
    fclose(f);
    return OK;
}

/*------------------------------------------------------
 * getlockedRfaInfoList - get file info list for "dir" an lock it
 *------------------------------------------------------*/
int getLockedRfaInfoList(dir, rfaHeadp, target)
    char *dir;
    struct RfaInfo **rfaHeadp;
    char *target;
{
    int rc;

    lock_rfainfo(dir);
    if ((rc = getRfaInfoList(dir, rfaHeadp, target)) != OK) {
	unlock_rfainfo(dir);
	return rc;
    }
    return OK;
}


/*------------------------------------------------------
 * getRfaInfoList - get file info list for "dir"
 *------------------------------------------------------*/
int getRfaInfoList(dir, rfaHeadp, target)
    char *dir;
    struct RfaInfo **rfaHeadp;
    char *target;
{
    struct RfaInfo *rfalist = NULL, **rfap = & rfalist;
    DIR *dirp;
    struct dirent *dp;
    struct stat st;
    int rc;

    advise (LLOG_DEBUG, NULLCP, "getRfaInfoList: '%s', target '%s'", dir, target);
    /*--- find out if directory  --*/
    if (stat(makeFN(dir),&st) == -1) {
	advise (LLOG_EXCEPTIONS, NULLCP, "cant stat '%s':%s", dir, 
		sys_errname(errno));
	sprintf(rfaErrStr, "%s - %s", dir, sys_errname(errno));
	return NOTOK;
    }
    if (st.st_mode & S_IFDIR) {

	/*--- dir is a directory name, so open dir ---*/
	if ((dirp = opendir(makeFN(dir))) ==  NULL) {
	    sprintf(rfaErrStr, "%s - %s", dir, sys_errname(errno));
	    return NOTOK;
	}

	for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
	    if (((*rfap) = mallocRfaInfo(strdup(dp->d_name))) == NULL) {
		freeRfaInfoList(rfalist);
		sprintf(rfaErrStr, "out of memory");
		return NOTOK;
	    }
	    SET_STATUS((*rfap)->ri_status, RI_UNREGISTERED);
	    SET_LOCKINFO((*rfap)->ri_status, RI_UNLOCKED);
	    SET_TRANSFER((*rfap)->ri_status, RI_TR_REQ);

	    if ((target == NULL ) || (strcmp(target, dp->d_name) == 0)) {
		if ((rc = statFile(makeFN2(dir, dp->d_name), *rfap)) != OK) {
		    freeRfaInfoList(rfalist);
		    return rc;
		}
	    }
	    rfap = &((*rfap)->ri_next);
	}
	closedir(dirp);
	if ((rc = getFileLockInfo(dir, &rfalist)) != OK) {
	    freeRfaInfoList(rfalist);
	    return rc;
	}
    } else { 
	/*--- dir is a single file ---*/

	if (((*rfap) = mallocRfaInfo(strdup(basename(dir)))) == NULL) {
	    freeRfaInfoList(rfalist);
	    return NOTOK;
	}
	SET_STATUS((*rfap)->ri_status, RI_UNREGISTERED);
	SET_LOCKINFO((*rfap)->ri_status, RI_UNLOCKED);
	SET_TRANSFER((*rfap)->ri_status, RI_TR_REQ);

	if ((rc = statFile(makeFN(dir), *rfap)) != OK) {
	    freeRfaInfoList(rfalist);
	    return rc;
	}

	/*--- get locking info ---*/
	if ((rc = getFileLockInfo(dirname(dir), &rfalist)) != OK) {
	    freeRfaInfoList(rfalist);
	    return rc;
	}
    }
    *rfaHeadp = rfalist;

    return OK;
}

/*------------------------------------------------------
 * putRfaInfoList - write RFA file info list to ".rfainfo"
 *------------------------------------------------------*/
int putRfaInfoList(dirname, rfa)
    char *dirname;
    struct RfaInfo *rfa;
{
    FILE *f;
    char buf[BUFSIZ];

    advise(LLOG_DEBUG,NULLCP,"putRfaInfo %s", makeFN(dirname));
    strcpy(buf, makeFN2(dirname,".rfainfo"));
    rename (buf, makeFN2(dirname,".rfainfo.bak"));
    if ((f = fopen(makeFN2(dirname, ".rfainfo"), "w")) == NULL) {
	sprintf(rfaErrStr, "can't write %s/.rfainfo (%s)", dirname, 
		sys_errname(errno));
	return NOTOK;
    }
    for (; rfa; rfa = rfa->ri_next) {
	if (rfa->ri_mode && (rfa->ri_mode & S_IFMT & (S_IFREG | S_IFDIR)) == 0)
	    continue;
	
	if (fprintf(f, "%s %s %ld %s %ld\n", rfa->ri_filename,
		status2str(rfa->ri_status),  rfa->ri_lastChange, 
		rfa->ri_lckname ? rfa->ri_lckname : "NONE", 
		rfa->ri_lcksince) == EOF) 
	{
	    sprintf(rfaErrStr, "can't write %s/.rfainfo (%s)", dirname, 
		    sys_errname(errno));
	    advise(LLOG_EXCEPTIONS,NULLCP,rfaErrStr);
	    fclose(f);
	    unlink(makeFN2(dirname, ".rfainfo"));
	    strcpy(buf, makeFN2(dirname,".rfainfo.bak"));
	    rename (buf, makeFN2(dirname,".rfainfo"));
	    return NOTOK;
	}
    }
    fclose(f);
    return OK;
}


/*------------------------------------------------------
 * extractRfaInfo - extract RFA file info by filename
 *------------------------------------------------------*/
struct RfaInfo *extractRfaInfo(fn, rfap)
    char *fn;
    struct RfaInfo **rfap;
{
    struct RfaInfo *h;

    for (; *rfap; rfap = &((*rfap)->ri_next))
	if (strcmp(fn, (*rfap)->ri_filename) == 0) {
	    h = *rfap;
	    *rfap = h->ri_next;
	    h->ri_next = NULL;
	    return h;
	}
    return NULL;
}

/*------------------------------------------------------
 * remRfaInfo - remove RFA file info from list
 *------------------------------------------------------*/
void remRfaInfo (fn, rfap)
    char *fn;
    struct RfaInfo **rfap;
{
    struct RfaInfo *h;

    if ((h = extractRfaInfo(fn, rfap))) {
	h->ri_next = NULL;
	freeRfaInfoList(h);
    }
}
    
/*------------------------------------------------------
 * findRfaInfo - find RFA file info by filename
 *------------------------------------------------------*/
struct RfaInfo *findRfaInfo(fn, rfa)
    char *fn;
    register struct RfaInfo *rfa;
{
    for (; rfa; rfa = rfa->ri_next)
	if (strcmp(fn, rfa->ri_filename) == 0)
		return rfa;
    return NULL;
}

/*------------------------------------------------------
 * sortRfaInfoList - sort RFA list
 *------------------------------------------------------*/
void sortRfaInfoList(rfap)
    struct RfaInfo **rfap;
{
    struct RfaInfo *tnext, *tosort, *sorted, **spp;

    sorted = NULL;
    for (tosort = *rfap; tosort; tosort = tnext) {
	tnext = tosort->ri_next;
	for (spp = &sorted; *spp ; spp = &((*spp)->ri_next)) 
	    if (strcmp((*spp)->ri_filename, tosort->ri_filename) > 0) 
		break;
	tosort->ri_next = *spp;
	*spp = tosort;
    }
    *rfap = sorted;
}
		

	
