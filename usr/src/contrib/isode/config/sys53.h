/* sys53.h - site configuration file for generic SVR3 */

/* 
 * $Header: /f/osi/config/RCS/sys53.h,v 7.1 91/02/22 09:17:13 mrose Interim $
 *
 *
 * $Log:	sys53.h,v $
 * Revision 7.1  91/02/22  09:17:13  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:28:02  mrose
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

#define	SYS5			/* AT&T UNIX */
#define	SVR3			/*   Systems V Release 3 */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP */
#define	SOCKETS			/*   provided by sockets */

#endif
