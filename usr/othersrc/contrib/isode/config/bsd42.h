/* bsd42.h - site configuration file for 4.2BSD UNIX */

/* 
 * $Header: /f/osi/config/RCS/bsd42.h,v 7.1 91/02/22 09:16:24 mrose Interim $
 *
 *
 * $Log:	bsd42.h,v $
 * Revision 7.1  91/02/22  09:16:24  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:25:40  mrose
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

#endif
