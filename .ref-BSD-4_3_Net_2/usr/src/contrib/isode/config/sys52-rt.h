/* sys52-rt.h - site configuration file for RT PC running AIX */

/*
 * $Header: /f/osi/config/RCS/sys52-rt.h,v 7.1 91/02/22 09:17:05 mrose Interim $
 *
 * Contributed by by Jacob Rekhter, T.J. Watson Research Center, IBM Corp.
 *
 *
 * $Log:	sys52-rt.h,v $
 * Revision 7.1  91/02/22  09:17:05  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:26:27  mrose
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
#define	AIX			/*   with IBMs running AIX */
#define	RT			/*   on the RT/PC */

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP */
#define	SOCKETS			/*   provided by sockets */

#endif
