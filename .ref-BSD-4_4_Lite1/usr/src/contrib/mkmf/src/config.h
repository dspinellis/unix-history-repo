/*
 * Copyright (c) 1991, 1993 Peter J. Nicklin.
 * Copyright (c) 1991, 1993 Version Technology.
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
 * $Header: config.h,v 4.8 93/05/26 00:27:01 nicklin Exp $
 *
 * System configuration definitions
 *
 * Author: Peter J. Nicklin
 *
 * The following definitions control the conditional compilation of the
 * mkmf source code for variants of the the UNIX operating system. New
 * definitions should be added to the applicable system profiles below.
 *
 * _HasRename			rename() system call	(BSD)
 * _HasStrDcl			str???() declared in #include (ANSI)
 * _HasStrchr			strchr() & strrchr() library routines (SYSV)
 * _HasNetRoot			// network root 	(Domain/OS only)
 * _HasOpenMode			optional mode parameter in open() system call
 * _HasSymLinks			symbolic links 		(BSD file systems)
 * _HasSystemProfile		custom system configuration profile exists
 * _HasIncludeDirent		#include <dirent.h>	(SYSV)
 * _HasIncludeSysDir		#include <sys/dir.h>	(BSD)
 * _HasIncludeSysNdir		#include <sys/ndir.h>	(ndir library)
 * _HasIncludeStrings		#include <strings.h>	(BSD)
 * _HasCompileSysType		COMPILESYSTYPE environment var (Domain/OS only)
 * _HasEnxioReadlinkReturn	Readlink returns ENXIO (Domain/OS 10.2 bug only)
 */
#ifndef CONFIG_H
#define CONFIG_H

#if defined(hpux) || defined(__hpux)
#  define _HasRename
#  define _HasStrDcl
#  define _HasStrchr
#  define _HasOpenMode
#  define _HasSymLinks
#  define _HasIncludeDirent
#  define _HasSystemProfile
#endif

#if defined(apollo)
#  if defined(SYSV)
#    define _HasRename
#    define _HasStrDcl
#    define _HasStrchr
#    define _HasNetRoot
#    define _HasOpenMode
#    define _HasSymLinks
#    define _HasIncludeDirent
#    define _HasCompileSysType
#    define _HasEnxioReadlinkReturn
#    define _HasSystemProfile
#  else
#    define _HasRename
#    define _HasStrDcl
#    define _HasNetRoot
#    define _HasOpenMode
#    define _HasSymLinks
#    define _HasIncludeSysDir
#    define _HasIncludeStrings
#    define _HasCompileSysType
#    define _HasEnxioReadlinkReturn
#    define _HasSystemProfile
#  endif
#endif

#if defined(sun)
#  define _HasRename
#  define _HasStrDcl
#  define _HasOpenMode
#  define _HasSymLinks
#  define _HasIncludeSysDir
#  define _HasIncludeStrings
#  define _HasSystemProfile
#endif

#if defined(vax)
#  define _HasRename
#  define _HasStrDcl
#  define _HasOpenMode
#  define _HasSymLinks
#  define _HasIncludeSysDir
#  define _HasIncludeStrings
#  define _HasSystemProfile
#endif

#if defined(aix) || defined(_AIX)
#  define _HasRename
#  define _HasStrDcl
#  define _HasStrchr
#  define _HasOpenMode
#  define _HasSymLinks
#  define _HasIncludeDirent
#  define _HasSystemProfile
#endif

#if defined(_AUX_SOURCE)
#  define _HasRename
#  define _HasStrchr
#  define _HasOpenMode
#  define _HasSymLinks
#  define _HasIncludeDirent
#  define _HasSystemProfile
#endif

#if defined(cray) || defined(CRAY) || defined(_CRAY)
#  define _HasRename
#  define _HasStrDcl
#  define _HasStrchr
#  define _HasOpenMode
#  define _HasIncludeDirent
#  define _HasSystemProfile
#  if !defined(UNICOS5)		/* exclude with -DUNICOS5 compiler option */
#    define _HasSymLinks
#  endif
#endif

#if defined(BSD4X)		/* generic BSD system */
#  define _HasRename
#  define _HasStrDcl
#  define _HasOpenMode
#  define _HasSymLinks
#  define _HasIncludeSysDir
#  define _HasIncludeStrings
#  define _HasSystemProfile
#endif

#if defined(M_XENIX)
#  define _HasStrchr
#  define _IncludeSysNdir
#  define _HasSystemProfile
#endif

#if defined(__STDC__)
#  if !defined(_HasStrDcl)
#   define _HasStrDcl
#  endif
#endif

#if defined(_POSIX_SOURCE) || !defined(_HasSystemProfile)
#  if !defined(_HasRename)
#    define _HasRename
#  endif
#  if !defined(_HasStrDcl)
#   define _HasStrDcl
#  endif
#  if !defined(_HasStrchr)
#    define _HasStrchr
#  endif
#  if !defined(_HasOpenMode)
#    define _HasOpenMode
#  endif
#  if !defined(_HasIncludeDirent)
#    define _HasIncludeDirent
#  endif
#endif

#if defined(_HasOpenMode)
#  define OPEN(name,flags,mode) open(name,flags,mode)
#else
#  define OPEN(name,flags,mode) open(name,flags)
#endif

#if defined(_HasRename)
#  define RENAME(from,to)	rename(from,to)
#else
#  define RENAME(from,to) \
   (unlink(to), (link(from,to) != -1 && unlink(from) != -1))
#endif

#define FILEXIST(file)		((access(file,0) == 0) ? 1 : 0)
#define FILEWRITE(file)		((access(file,6) == 0) ? 1 : 0)

#endif /* CONFIG_H */
