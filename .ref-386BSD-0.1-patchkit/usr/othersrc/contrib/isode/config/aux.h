/* aux.h - config file for A/UX running 1.1 */

/* 
 * $Header: /f/osi/config/RCS/aux.h,v 7.1 91/02/22 09:16:22 mrose Interim $
 *
 * Contributed by Julian Onions, Nottingham University
 *
 *
 * $Log:	aux.h,v $
 * Revision 7.1  91/02/22  09:16:22  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:25:39  mrose
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
#define AUX			/*   with Apple's enhancements */
#define	WRITEV			/*   that include the writev call */

#define VSPRINTF

#define	TCP			/* has TCP/IP */
#define	SOCKETS			/*   provided by sockets */

#define	NFS			/* network file system -- has getdirentries */

#define	RFINGER		"/usr/ucb/finger"

#endif
