/* osx.h - site configuration file for Olivetti LSX 30xx */

/* 
 * $Header: /f/osi/config/RCS/osx.h,v 7.1 91/02/22 09:16:45 mrose Interim $
 *
 *
 * $Log:	osx.h,v $
 * Revision 7.1  91/02/22  09:16:45  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:26:12  mrose
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


#ifndef _CONFIG_
#define _CONFIG_

#define BSD42                   /* Berkeley UNIX */
#define WRITEV                  /* real Berkeley UNIX (sort of) */
#define XOS_2			/*  Olivetti's version */

#define VSPRINTF

#define TCP                     /* has TCP/IP (of course) */
#define SOCKETS                 /* 4.2BSD sockets */
#define NOGOSIP
#define TSBRIDGE

#define NFS
#endif
