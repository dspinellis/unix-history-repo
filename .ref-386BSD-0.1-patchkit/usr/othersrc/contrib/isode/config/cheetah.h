/* cheetah.h - site configuration file for cheetah (SunOS) */

/* 
 * $Header: /f/osi/config/RCS/cheetah.h,v 7.2 91/02/22 09:16:32 mrose Interim $
 *
 *
 * $Log:	cheetah.h,v $
 * Revision 7.2  91/02/22  09:16:32  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/15  18:17:56  mrose
 * zap-AET
 * 
 * Revision 7.0  89/11/23  21:26:01  mrose
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

#define	VSPRINTF		/* has vprintf(3s) routines */

#define	TCP			/* has TCP/IP (of course) */
#define	SOCKETS			/*   provided by sockets */

#define	NFS			/* network filesystem -- has getdirentries() */

#define	ANON	"anon"		/* guest login for ftam */

#endif
