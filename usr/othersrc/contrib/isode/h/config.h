/* bsd44.h - site configuration file for 4.4BSD UNIX */

/* 
 * $Header: /f/osi/config/RCS/bsd44.h,v 7.3 91/02/22 09:16:30 mrose Interim $
 *
 *
 * $Log:	bsd44.h,v $
 * Revision 7.3  91/02/22  09:16:30  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/10  16:13:03  mrose
 * x25
 * 
 * Revision 7.1  90/07/09  14:32:41  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:26:00  mrose
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
#define	BSD43			/*   4.3BSD or later */
#define	BSD44			/*   4.4BSD to be precise! */

#define X25
#define UBC_X25
#define UBC_X25_WRITEV

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define	TP4			/* has TP4 */
#define	BSD_TP4			/*   provided by UCB/UWisc */

#define	GETDENTS		/* has getdirent(2) call */
#define	NFS			/* network file system -- has getdirentries */

#endif
