/* sunos3.h - site configuration file for SunOS release 3 */

/* 
 * $Header: /f/osi/config/RCS/sunos3.h,v 7.1 91/02/22 09:16:59 mrose Interim $
 *
 *
 * $Log:	sunos3.h,v $
 * Revision 7.1  91/02/22  09:16:59  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:26:22  mrose
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

#define	NFS			/* network filesystem -- has getdirentries() */

#endif
