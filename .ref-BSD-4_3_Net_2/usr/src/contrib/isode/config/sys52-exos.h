/* sys52-exos.h - site configuration file for SVR2 with EXOS 8044 card */

/* 
 * $Header: /f/osi/config/RCS/sys52-exos.h,v 7.1 91/02/22 09:17:03 mrose Interim $
 *
 *
 * $Log:	sys52-exos.h,v $
 * Revision 7.1  91/02/22  09:17:03  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:26:26  mrose
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

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP */
#define	EXOS			/*   provided by EXOS 8044 card */

#ifdef	sgi			/* screwy compiler! */
#define	register
#endif

#endif
