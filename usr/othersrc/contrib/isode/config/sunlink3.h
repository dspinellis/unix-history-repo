/* sunlink3.h - site configuration file for SunLink OSI 5.2 on SunOS 3 */

/* 
 * $Header: /f/osi/config/RCS/sunlink3.h,v 7.1 91/02/22 09:16:53 mrose Interim $
 *
 *
 * $Log:	sunlink3.h,v $
 * Revision 7.1  91/02/22  09:16:53  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:26:19  mrose
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


#ifndef	_CONFIG_
#define	_CONFIG_

#define	BSD42			/* Berkeley UNIX */
#define	WRITEV			/*   real Berkeley UNIX */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define	X25			/* has X.25 */
#define	SUN_X25			/*   provided by SunLink X.25 */

#define TP4			/* has TP4 */
#define SUN_TP4			/*   provided by SunLink OSI */
#define	SUNLINK_5_2		/*     define if using SunLink OSI release 5.2
				       or greater */

#define	NFS			/* network filesystem -- has getdirentries() */

#endif
