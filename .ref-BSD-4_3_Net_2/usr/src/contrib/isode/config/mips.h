/* mips.h - site configuration file for RISC/OS */

/* 
 * $Header: /f/osi/config/RCS/mips.h,v 7.2 91/02/22 09:16:40 mrose Interim $
 *
 *
 * $Log:	mips.h,v $
 * Revision 7.2  91/02/22  09:16:40  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/03/26  09:57:04  mrose
 * MIPS
 * 
 * Revision 7.0  89/11/23  21:26:06  mrose
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

#define	MIPS			/* RISC/OS */

#define	NFS			/* network file system -- has getdirentries */

#define	BIND

#endif
