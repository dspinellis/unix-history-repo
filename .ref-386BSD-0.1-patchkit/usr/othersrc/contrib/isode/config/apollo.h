/* apollo.h - site configuration file for Apollo */

/*
 * $Header: /f/osi/config/RCS/apollo.h,v 7.1 91/02/22 09:16:19 mrose Interim $
 *
 * Contributed by John Brezak, Apollo Computer, Inc.
 *
 *
 * $Log:	apollo.h,v $
 * Revision 7.1  91/02/22  09:16:19  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:25:37  mrose
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

#define VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#ifdef	notdef			/* Don Preuss at Apollo says no longer needed*/
#ifdef __STDC__                 /* thinks it's ANSI C, but it isn't! */
#undef __STDC__
#endif
#endif

#endif
