/* solbourne.h - site configuration file for solbournes */

/* 
 * $Header: /f/osi/config/RCS/solbourne.h,v 7.2 91/02/22 09:16:50 mrose Interim $
 *
 *
 * $Log:	solbourne.h,v $
 * Revision 7.2  91/02/22  09:16:50  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:32:55  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:26:17  mrose
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
#define	SUNOS4			/*   with Sun's enhancements */
#define	WRITEV			/*   (sort of) real Berkeley UNIX */
#define	BSD43			/*   4.3BSD or later */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define	GETDENTS		/* has getdirent(2) call */
#define	NFS			/* network filesystem -- has getdirentries */

#endif
