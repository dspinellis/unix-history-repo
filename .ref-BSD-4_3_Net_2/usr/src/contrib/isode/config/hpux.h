/* hpux.h - site configuration file for HP-UX */

/*
 * $Header: /f/osi/config/RCS/hpux.h,v 7.2 91/02/22 09:16:35 mrose Interim $
 *
 *
 * $Log:	hpux.h,v $
 * Revision 7.2  91/02/22  09:16:35  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:32:47  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:26:04  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifndef _CONFIG_
#define _CONFIG_

#define SYS5                    /* AT&T UNIX */
#define HPUX                    /*   with HP's enhancements */
#define VSPRINTF                /* libc includes vsprintf and vfprintf */

#define TCP                     /* has TCP/IP */
#define SOCKETS                 /*   provided by sockets */

#define	GETDENTS		/* has getdirent(2) call */

#endif
