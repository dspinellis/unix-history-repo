/*
 * Copyright (c) 1983, 1985, 1991 Peter J. Nicklin.
 * Copyright (c) 1991 Version Technology.
 * All Rights Reserved.
 *
 * $License: VT.1.1 $
 * Redistribution and use in source and binary forms,  with or without
 * modification,  are permitted provided that the following conditions
 * are met:  (1) Redistributions of source code must retain the  above
 * copyright  notice,  this  list  of  conditions  and  the  following
 * disclaimer.  (2) Redistributions in binary form must reproduce  the
 * above  copyright notice,  this list of conditions and the following
 * disclaimer in the  documentation  and/or other  materials  provided
 * with  the  distribution.  (3) All advertising materials  mentioning
 * features or  use  of  this  software  must  display  the  following
 * acknowledgement:  ``This  product  includes  software  developed by
 * Version Technology.''  Neither the name of Version  Technology  nor
 * the  name  of  Peter J. Nicklin  may  be used to endorse or promote
 * products derived from this software without specific prior  written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY VERSION TECHNOLOGY ``AS IS''  AND  ANY
 * EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO, THE
 * IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL  VERSION  TECHNOLOGY  BE
 * LIABLE  FOR ANY DIRECT,  INDIRECT,  INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR  CONSEQUENTIAL DAMAGES   (INCLUDING,   BUT   NOT   LIMITED   TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT LIABILITY,  OR  TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE,  EVEN  IF  ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 * Report problems and direct questions to nicklin@netcom.com
 *
 * $Header: path.h,v 4.2 91/11/25 19:45:47 nicklin Exp $
 *
 * Pathname definitions
 *
 * Author: Peter J. Nicklin
 */
#ifndef PATH_H
#define PATH_H

/*
 * Buffer sizes
 */
#define ALIASSIZE	30		/* project directory alias size */
#define TYPESIZE	30		/* project directory type label size */
#define DIRDESCSIZE	128		/* project directory description size */
#define P_BUFSIZE	1024		/* pathname buffer size */
#define	PATHSIZE	1024		/* maximum pathname length */
#define PPATHSIZE	256		/* maximum project pathname length */
#define TYPBUFSIZE	256		/* directory type labels buffer */

/* 
 * Nomenclature (WARNING: Check definition usage BEFORE changing)
 */
#define _CDIRC		'.'		/* current directory character */
#define _HDIRC		'~'		/* home directory character */
#define _PDIRC		'^'		/* project root directory character */
#define _PDTSC		'/'		/* project dir type labels sep char */
#define _PPSC		'^'		/* project path separator character */
#define _PSC		'/'		/* pathname separator character */
#define _RDIRC		'/'		/* root directory character */
#define CURDIR		"."		/* current directory */
#define PARENTDIR	".."		/* parent directory */
#define PATHSEP		"/"		/* pathname separator */
#define PPATHSEP	"^"		/* project pathname separator */
#define ROOTDIR		"/"		/* root directory */
#define ROOTPROJECT	"^"		/* root project */
#define USERPROJECT	"~"		/* user's root project */

/*
 * Pathname types
 */
#define P_IFMT		0xf0000		/* project pathname mask */
#define	P_IFNEW		0x00000		/* new directory or file */
#define P_IFREG		0x10000		/* regular directory or file */
#define	P_IFHOME	0x20000		/* root project root directory */
#define P_IFPDIR	0x30000		/* project directory */
#define	P_IFPROOT	0x40000		/* project root directory */

/*
 * Pathname struct
 */
typedef struct _path
	{
	unsigned long p_mode;		/* type of pathname */
	char *p_alias;			/* pathname alias */
	char *p_path;			/* pathname */
	char *p_type;			/* project directory type labels */
	char *p_desc;			/* project directory description */
	char p_buf[P_BUFSIZE];		/* pathname buffer */
	char p_project[PATHSIZE];	/* pathname's project */
	} PATH;

#endif /* PATH_H */
