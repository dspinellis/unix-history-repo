/* sys52-sun.h - site configuration file for SUN's SVR2 Compatiblity Package */

/* 
 * $Header: /f/osi/config/RCS/sys52-sun.h,v 7.1 91/02/22 09:17:07 mrose Interim $
 *
 *
 * $Log:	sys52-sun.h,v $
 * Revision 7.1  91/02/22  09:17:07  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:26:29  mrose
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

#define	SYS5			/* AT&T UNIX emulation */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP */
#define	SOCKETS			/*   provided by sockets */

#define	NFS			/* network filesystem -- has getdirentries() */

#endif
