/* sunlink7.h - site configuration file for SunLink OSI and X.25 7.0 on
	SunOS 4 */

/* 
 * $Header: /f/osi/config/RCS/sunlink7.h,v 7.1 91/02/22 09:16:57 mrose Interim $
 *
 *
 * $Log:	sunlink7.h,v $
 * Revision 7.1  91/02/22  09:16:57  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/11/20  15:39:48  mrose
 * *** empty log message ***
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


#ifndef	_CONFIG_
#define	_CONFIG_

#define	BSD42			/* Berkeley UNIX */
#define	SUNOS4			/*   with Sun's enhancements */
#define	WRITEV			/*   real Berkeley UNIX */
#define	BSD43			/*   4.3BSD or later */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define	X25			/* has X.25 */
#define	SUN_X25			/*   provided by SunLink X.25 */

#define TP4			/* has TP4 */
#define SUN_TP4			/*   provided by SunLink OSI */
#define	SUNLINK_5_2		/*     define if using SunLink OSI release 5.2
				       or greater */
#define	SUNLINK_6_0		/*     define if using SunLink OSI release 6.0
				       or greater */
#define	SUNLINK_7_0		/*     define if using SunLink OSI release 7.0
				       or greater */

#define	GETDENTS		/* has getdirent(2) call */
#define	NFS			/* network filesystem -- has getdirentries() */

#endif
