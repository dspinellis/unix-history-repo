/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * rfa2fi.c : convert between RfaInfo and type_RFA_FileInfoList and vice versa
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/rfa2fi.c,v 7.3 91/02/22 09:28:20 mrose Interim $
 *
 * $Log:	rfa2fi.c,v $
 * Revision 7.3  91/02/22  09:28:20  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:54:56  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:07:24  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/rfa2fi.c,v 7.3 91/02/22 09:28:20 mrose Interim $";
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
#include "rfa.h"
#include "RFA-types.h"
#include "rfainfo.h"

extern char *getRelativeFN();
extern char *fsBase;

/*--------------------------------------------------------------------
 * rfa2fi  - RfaInfo to type_RFA_FileInfo
 *--------------------------------------------------------------------*/
struct type_RFA_FileInfo *rfa2fi(dir, rfa)
    char *dir;
    struct RfaInfo *rfa;
{
    struct type_RFA_FileInfo *fi;
    char *l;

    if ((fi = (struct type_RFA_FileInfo *)
		malloc(sizeof(struct type_RFA_FileInfo))) == NULL)
    {
	sprintf(rfaErrStr, "out of memory");
	return NULL;
    }
    fi->name = str2qb(rfa->ri_filename, strlen(rfa->ri_filename), 1);
    fi->mode = rfa->ri_mode;
    fi->user = str2qb(rfa->ri_owner, strlen(rfa->ri_owner), 1);
    fi->group = str2qb(rfa->ri_group, strlen(rfa->ri_group), 1);
    fi->size = rfa->ri_size;
    fi->accTime = rfa->ri_accTime;
    fi->modTime = rfa->ri_modTime;
    if (l = rfa->ri_lnkName) {
	if (*l != '/') {
	    if (getRelativeFN(realPath3(fsBase, dir, l)) == NULL) 
		l = "(link outside RFA context)";
	} else
	    if ((l = getRelativeFN(rfa->ri_lnkName)) == NULL) 
		l = "(link outside RFA context)";
	fi->lnkName = str2qb(l, strlen(l), 1);
    } else
	fi->lnkName = NULL;
    fi->status = rfa->ri_status;
    if (IS_LOCKED(rfa->ri_status)) {
	fi->lockedBy = str2qb(rfa->ri_lckname, strlen(rfa->ri_lckname), 1);
	fi->lockedSince = rfa->ri_lcksince;
    } else {
	fi->lockedBy = NULL;
	fi->lockedSince = NULL;
    }
    return fi;
}

/*--------------------------------------------------------------------
 * rfa2fil  - RfaInfo to type_RFA_FileInfoList
 *--------------------------------------------------------------------*/
struct type_RFA_FileInfoList *rfa2fil(dir, rfa)
    char *dir;
    struct RfaInfo *rfa;
{
    struct type_RFA_FileInfoList *fil, **filp;

    filp = &fil;
    for (; rfa; rfa = rfa->ri_next) {
	if (((*filp) = (struct type_RFA_FileInfoList *)
	    malloc(sizeof(struct type_RFA_FileInfoList))) == NULL)
	{
	    free_RFA_FileInfoList(fil);
	    sprintf(rfaErrStr, "out of memory");
	    return NULL;
	}
	(*filp)->next = NULL;
	if (((*filp)->FileInfo = rfa2fi(dir, rfa)) == NULL) {
	    free_RFA_FileInfoList(fil);
	    return NULL;
	}
	filp = &((*filp)->next);
    }
    return fil;
}

/*--------------------------------------------------------------------
 * fi2rfa  - type_RFA_FileInfoList to RfaInfo 
 *--------------------------------------------------------------------*/
struct RfaInfo *fi2rfa(fil)
    struct type_RFA_FileInfoList *fil;
{
    struct type_RFA_FileInfo *fi;
    struct RfaInfo *rfa, *rfaNew;
    char *l;

    rfa = NULL;
    for (; fil; fil = fil->next) {
	fi = fil->FileInfo;
	if ((rfaNew = mallocRfaInfo(qb2str(fi->name))) == NULL) {
	    freeRfaInfoList(rfa);
	    return NULL;
	}
	rfaNew->ri_next = rfa;
	rfa = rfaNew;
	
	rfa->ri_mode = fi->mode;
	rfa->ri_owner = qb2str(fi->user);
	rfa->ri_group = qb2str(fi->group);
	rfa->ri_size = fi->size;
	rfa->ri_accTime = fi->accTime;
	rfa->ri_modTime = fi->modTime;
	if (fi->lnkName) {
	    l = qb2str(fi->lnkName);
	    if (*l == '/')
		l = makeFN(l);
	    rfa->ri_lnkName = strdup(l);
	} else
	    rfa->ri_lnkName = NULL;
	rfa->ri_status = fi->status;
	if (IS_LOCKED(rfa->ri_status)) {
	    rfa->ri_lckname = qb2str(fi->lockedBy);
	    rfa->ri_lcksince = fi->lockedSince;
	} else {
	    rfa->ri_lckname = NULL;
	    rfa->ri_lcksince = NULL;
	}
    }
    return rfa;
}

    
