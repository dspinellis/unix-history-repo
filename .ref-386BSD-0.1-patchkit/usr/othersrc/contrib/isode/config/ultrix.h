/* ultrix.h - site configuration file for Ultrix 3.1 */

/* 
 * $Header: /f/osi/config/RCS/ultrix.h,v 7.3 91/02/22 09:17:15 mrose Interim $
 *
 *
 * $Log:	ultrix.h,v $
 * Revision 7.3  91/02/22  09:17:15  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  14:32:31  mrose
 * update
 * 
 * Revision 7.1  90/07/09  14:33:09  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:26:32  mrose
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
#define BSD43			/* 4.3BSD networking or later */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */
#define BIND			/* has h_addr_list in netdb.h */

#define	GETDENTS		/* has getdirent(2) call */

#endif
