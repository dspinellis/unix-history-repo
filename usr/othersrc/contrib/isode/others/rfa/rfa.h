/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * rfa.h : common definitions for RFA
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/rfa.h,v 7.3 91/02/22 09:28:15 mrose Interim $
 *
 * $Log:	rfa.h,v $
 * Revision 7.3  91/02/22  09:28:15  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:54:50  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:09:45  ow
 * Initial revision
 * 
 */

/*
 *                              NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include "config.h"

#ifndef OK
#define OK 		0
#define NOTOK		1
#endif
#define NOTOK_LOCAL_LOCK		2
#define NOTOK_LOCAL_ERROR		3
#define NOTOK_REMOTE_ERROR		4
#define NOTOK_LOCKED			5
#define NOTOK_FILEACCESS		6
#define NOTOK_NOTREGULAR		7
#define NOTOK_GETMASTER			8
#define NOTOK_OUTOFSUBTREE		9
#define NOTOK_NOTLOCKED			10
#define NOTOK_ALREADY_SLAVE		11
#define NOTOK_ALREADY_MASTER		12
#define NOTOK_ALREADY_UNREG		13
#define NOTOK_REMOTE_NOT_MASTER		14
#define NOTOK_REMOTE_MASTER_OLDER	15
#define NOTOK_UNREG_LOCAL_FILE		16
#define NOTOK_IS_SLAVE			17
#define NOTOK_SYS			18
#define NOTOK_INCONSISTENCY		19
#define NOTOK_NOT_ALLOWED		20


extern char *makeFN2();
extern char *makeFN();
extern char *basename();
extern char *dirname();
extern char *expandSymLinks();
extern char *realPath(), *realPath3();

extern char rfaErrStr[];
extern char *errMsg();

/*--- tailor variables ---*/
extern int default_transfer;
extern int doChown;
extern int doChgrp;
extern int doChmod;
extern int doClearSUID;
extern int doRmWidows;
extern int doRfaExec;
extern int timeSlave;
extern int compLimit;
extern char *passwd;
extern char *user;
extern char *host;
extern int backup;
