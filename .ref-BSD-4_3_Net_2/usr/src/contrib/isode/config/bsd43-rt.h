/* bsd43-rt.h - site configuration file for RT PC running 4.3BSD UNIX */

/* 
 * $Header: /f/osi/config/RCS/bsd43-rt.h,v 7.2 91/02/22 09:16:26 mrose Interim $
 *
 * Contributed by Jacob Rekhter, T.J. Watson Research Center, IBM Corp.
 *
 *
 * $Log:	bsd43-rt.h,v $
 * Revision 7.2  91/02/22  09:16:26  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/01/27  10:25:59  mrose
 * touch-up
 * 
 * Revision 7.0  89/11/23  21:25:57  mrose
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

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define	RT			/* RT/PC */

#if   defined(__STDC__) && defined(__HIGHC__)
				/* hc thinks it's ANSI C, but it isn't! */
#undef	__STDC__
#endif

#endif
