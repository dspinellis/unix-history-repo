/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * rfainfo.h : data structures to represent content of ".rfainfo" files
 *             and stat info of files
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/rfainfo.h,v 7.3 91/02/22 09:28:25 mrose Interim $
 *
 * $Log:	rfainfo.h,v $
 * Revision 7.3  91/02/22  09:28:25  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:55:02  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:09:53  ow
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


#include <sys/types.h>

struct RfaInfo {
	char		*ri_filename;
	int		 ri_status;
	time_t		 ri_lastChange;	/* time when status change was done */
	char		*ri_lckname;    /* name of user that locked file   */
	time_t		 ri_lcksince;	/* time when file has been locked  */

	char		*ri_owner;	/* file info as per stat(2) */
	char		*ri_group;
	int		 ri_mode;
	int		 ri_size;
	time_t		 ri_modTime;
	time_t		 ri_accTime;
	char		*ri_lnkName;

	struct RfaInfo *ri_next;
};

#define RI_STATUS(s)		((s) & 07)
#define SET_STATUS(v, s)	(v)=(((v) & ~07) | (s))
#define	RI_UNREGISTERED		0
#define	IS_UNREGISTERED(s)	(RI_STATUS(s) == RI_UNREGISTERED)
#define	RI_MASTER		1
#define	IS_MASTER(s)		(RI_STATUS(s) == RI_MASTER)
#define RI_SLAVE		2
#define	IS_SLAVE(s)		(RI_STATUS(s) == RI_SLAVE)

#define RI_LOCKINFO(s)		((s) & 070)
#define SET_LOCKINFO(v, s)	(v)=(((v) & ~070) | (s))
#define	RI_LOCKED		010
#define	IS_LOCKED(s)		(RI_LOCKINFO(s) == RI_LOCKED)
#define RI_UNLOCKED		020
#define	IS_UNLOCKED(s)		(RI_LOCKINFO(s) == RI_UNLOCKED)

#define RI_TRANSFER(s)		((s) & 0700)
#define SET_TRANSFER(v, s)	(v)=(((v) & ~0700) | (s))
#define RI_TR_AUTO		0100
#define	IS_TR_AUTO(s)		(RI_TRANSFER(s) == RI_TR_AUTO)
#define RI_TR_REQ		0200
#define	IS_TR_REQ(s)		(RI_TRANSFER(s) == RI_TR_REQ)

extern int getLockedRfaInfoList(), getRfaInfoList(), putRfaInfoList();
extern void remRfaInfo();
extern struct RfaInfo *mallocRfaInfo(), *findRfaInfo(), *extractRfaInfo();
extern int str2status();
extern char *status2str(), *status2sstr();

extern struct RfaInfo *fi2rfa();
extern struct type_RFA_FileInfoList *rfa2fil();
extern struct type_RFA_FileInfo *rfa2fi();


