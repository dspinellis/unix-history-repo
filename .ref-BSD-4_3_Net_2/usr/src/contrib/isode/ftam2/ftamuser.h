/* ftamuser.h - include file for interactive FTAM initiator */

/* 
 * $Header: /f/osi/ftam2/RCS/ftamuser.h,v 7.5 91/02/22 09:24:14 mrose Interim $
 *
 *
 * $Log:	ftamuser.h,v $
 * Revision 7.5  91/02/22  09:24:14  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/23  18:40:24  mrose
 * update
 * 
 * Revision 7.3  90/11/21  11:30:59  mrose
 * sun
 * 
 * Revision 7.2  90/09/07  11:14:13  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:37:30  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:54:45  mrose
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


#include "ftamsbr.h"

/*    MAIN */

extern int  ontty;
extern int  interrupted;

void	adios (), advise ();
#ifndef	BRIDGE
int	ask (), getline ();
#endif
char   *strdup ();

/*    DATA */

extern int  ftamfd;
#ifdef	BRIDGE
extern int  dataconn ();
#endif

extern char *host;
extern char *user;
extern char *account;
#ifndef	BRIDGE
extern char *userdn;
extern char *storename;
#endif

extern int  bell;
extern int  concurrency;	/* Olivier Dubois */
extern int  debug;
extern int  globbing;
extern int  hash;
extern int  marks;
extern int  omode;
extern int  query;
extern int  runcom;
extern int  tmode;
extern int  trace;
extern int  verbose;
extern int  watch;

extern char *myhome;
extern char *myuser;

extern int  realstore;
#define	RFS_UNKNOWN	0
#define	RFS_UNIX	1

extern char *rs_unknown;
extern char *rs_support;


extern char *rcwd;

extern struct QOStype myqos;


char   *str2file ();

/*    DISPATCH */

struct dispatch {
    char   *ds_name;
    IFP	    ds_fnx;

    int	    ds_flags;
#define	DS_NULL		0x00
#define	DS_OPEN		0x01	/* association required */
#define	DS_CLOSE	0x02	/* association avoided */
#define	DS_MODES	0x04	/* class/units meaningful */

    int	    ds_class;
    int	    ds_units;

    char   *ds_help;
};


struct dispatch *getds ();

/*    FTAM */

#define	UMASK	"\020\01READ\02WRITE\03ACCESS\04LIMITED\05ENHANCED\06GROUPING\
\07RECOVERY\08RESTART"


extern OID context;
extern int fqos;
extern int class;
extern int units;
extern int attrs;
extern int fadusize;


extern struct vfsmap vfs[];	/* ordering depends on char *tmodes[] */
#define	VFS_DEF	0		/* try to default it */
#define	VFS_UBF	1		/* offset to unstructured binary file */
#define	VFS_UTF	2		/*   ..	     unstructured text file */
#define	VFS_FDF	3		/*   ..	     file directory file */

extern struct vfsmap *myvf;


void	ftam_advise (), ftam_chrg (), ftam_diag (), ftam_watch ();

/*    FILES */

struct filent {
    char   *fi_name;
    OID	    fi_oid;

    char   *fi_entry;

    struct filent *fi_next;
};

extern int  toomany;

extern int  nfilent;
extern struct filent *filents;


int	fdffnx ();

/*    GLOB */

extern int   xglobbed;
extern char *globerr;

int	blkfree (), blklen ();
char  **blkcpy ();

char   *xglob1val ();
char  **xglob ();

/*  */

extern int  errno;
extern char *isodeversion;
#ifdef	BRIDGE
extern char ftam_error[];
#endif
