/* osi.h - site configuration file for osi (SunOS) */

/* 
 * $Header: /f/osi/config/RCS/osi.h,v 7.2 91/02/22 09:16:43 mrose Interim $
 *
 *
 * $Log:	osi.h,v $
 * Revision 7.2  91/02/22  09:16:43  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/15  18:17:59  mrose
 * zap-AET
 * 
 * Revision 7.0  89/11/23  21:26:11  mrose
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

#define VSPRINTF		/* libc has vsprintf */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define X25			/* support for X.25 */
#define SUN_X25			/* using SunLink X.25 V5.2 */

#define	NFS			/* network filesystem -- has getdirentries() */

#define	ANON	"anon"		/* guest login for ftam */

#endif
