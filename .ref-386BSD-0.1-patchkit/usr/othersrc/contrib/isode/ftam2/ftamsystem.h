/* ftamsystem.h - include file for FTAM responder */

/* 
 * $Header: /f/osi/ftam2/RCS/ftamsystem.h,v 7.2 91/02/22 09:24:10 mrose Interim $
 *
 *
 * $Log:	ftamsystem.h,v $
 * Revision 7.2  91/02/22  09:24:10  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/11/05  13:30:00  mrose
 * nist
 * 
 * Revision 7.0  89/11/23  21:54:41  mrose
 * Release 6.0
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


#include <errno.h>
#include "ftamsbr.h"
#include "logger.h"


#define	SCPYN(a,b)	strncpy ((a), (b), sizeof (a))

/*    SERVER */

extern int  ftamfd;

extern int  cflag;
extern int  debug;
extern char *myname;


void	ftam_adios (), ftam_advise (), ftam_diag ();

void	adios (), advise ();

/*    UNIX DATA */

extern int  myuid;

extern int  myhomelen;
extern char myhome[];

extern dev_t null_dev;
extern ino_t null_ino;

/*    VFS DATA */

#define	NMAX	8		/* too painful to get right! */

#ifndef	NGROUPS
#define	NACCT	32
#else
#define	NACCT	(NGROUPS + 20)
#endif


extern struct vfsmap vfs[];	/* ordering affects default action in st2vfs()
				   put preferential entries towards the end */
#define	VFS_UBF	0		/* offset to FTAM-3 */
#define	VFS_UTF	1		/*  ..       FTAM-1 */
#define	VFS_FDF	2		/*  ..       NIST-9 */

/*    REGIME DATA */

extern int level;
extern int class;
extern int units;
extern int attrs;
extern int fadusize;

/*    ACTIVITY DATA */

extern int  myfd;
extern char *myfile;
extern struct stat  myst;
extern int  statok;

extern struct vfsmap   *myvf;	/* active contents type */
extern caddr_t myparam;		/*   .. */

extern int  myaccess;		/* current access request */

extern char *initiator;		/* current initiator identity */

extern struct FADUidentity mylocation;/* current location */

extern int  mymode;		/* current processing mode */
extern int  myoperation;	/*   .. */

#ifdef	notdef
extern AEI mycalling;		/* current calling AET */
extern AEI myresponding;	/* current responding AET */
#endif

extern char *account;		/* current account */
extern int  mygid;		/* "inner" account */

extern int  mylock;		/* current concurrency control */
extern struct FTAMconcurrency myconctl;/* .. */

extern int mylockstyle;		/* current locking style */


extern int  mycontext;		/* current access context */
extern int  mylevel;		/*   .. */


#ifndef	SYS5
#define	unlock()	if (mylock) (void) flock (myfd, LOCK_UN); else
#else
#define	unlock() \
    if (mylock) { \
	struct flock fs; \
 \
	fs.l_type = F_UNLCK; \
	fs.l_whence = L_SET; \
	fs.l_start = fs.l_len = 0; \
	(void) fcntl (myfd, F_SETLK, &fs); \
    } \
    else
#endif

/*  */

extern int  errno;

/*  */

#ifdef	BRIDGE
/* FTP interface routines and variables */

extern char *ftp_error;

int	ftp_exits (), ftp_delete (), ftp_mkdir (), ftp_rename (), ftp_type (),
	ftp_write (), ftp_append (), ftp_read (), ftp_ls (), ftp_login (),
	ftp_quit (), ftp_abort (), ftp_reply ();
#endif
