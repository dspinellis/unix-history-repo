/* rtf.h - definitions for RT-file transfer utility */

/* 
 * $Header: /f/osi/others/rtf/RCS/rtf.h,v 7.1 91/02/22 09:34:17 mrose Interim $
 *
 *
 * $Log:	rtf.h,v $
 * Revision 7.1  91/02/22  09:34:17  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:10:47  mrose
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


#include <stdio.h>
#include "rtsap.h"
#include "logger.h"
#include "RTF-types.h"
#ifdef	NULL
#undef	NULL
#endif
#include <sys/param.h>
#ifndef	NULL
#define	NULL	0
#endif
#ifndef	SYS5
#include <sys/file.h>
#else
#ifndef	AIX
#include <sys/fcntl.h>
#else
#include <fcntl.h>
#endif
#endif
#include <sys/stat.h>


extern LLog *pgm_log;

/*  */

char   *SReportString ();

void	rts_adios (), rts_advise ();
void	adios (), advise (), ryr_advise ();
