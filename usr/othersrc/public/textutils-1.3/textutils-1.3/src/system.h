/* system-dependent definitions for textutils programs.
   Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Include sys/types.h before this file.  */

#include <sys/stat.h>
#ifndef S_ISREG			/* Doesn't have POSIX.1 stat stuff. */
#define mode_t unsigned short
#endif
#if !defined(S_ISBLK) && defined(S_IFBLK)
#define	S_ISBLK(m) (((m) & S_IFMT) == S_IFBLK)
#endif
#if !defined(S_ISCHR) && defined(S_IFCHR)
#define	S_ISCHR(m) (((m) & S_IFMT) == S_IFCHR)
#endif
#if !defined(S_ISDIR) && defined(S_IFDIR)
#define	S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif
#if !defined(S_ISREG) && defined(S_IFREG)
#define	S_ISREG(m) (((m) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISFIFO) && defined(S_IFIFO)
#define	S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#endif
#if !defined(S_ISLNK) && defined(S_IFLNK)
#define	S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif
#if !defined(S_ISSOCK) && defined(S_IFSOCK)
#define	S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#endif
#if !defined(S_ISMPB) && defined(S_IFMPB) /* V7 */
#define S_ISMPB(m) (((m) & S_IFMT) == S_IFMPB)
#define S_ISMPC(m) (((m) & S_IFMT) == S_IFMPC)
#endif
#if !defined(S_ISNWK) && defined(S_IFNWK) /* HP/UX */
#define S_ISNWK(m) (((m) & S_IFMT) == S_IFNWK)
#endif
#if defined(MKFIFO_MISSING)
#define mkfifo(path, mode) (mknod ((path), (mode) | S_IFIFO, 0))
#endif

#ifdef HAVE_UNISTD_H
#ifdef __386BSD__
/* For 386bsd: the declarations of re_comp in unistd.h conflicts with
   the GNU implementation provided here, so hide the one in unistd.h. */
#define re_comp re_comp_hide
#endif /* __386BSD__ */
#include <unistd.h>
#ifdef __386BSD__
#undef re_comp
#endif /* __386BSD__ */
#endif
#ifndef _POSIX_VERSION
off_t lseek ();
#endif

#if defined(USG) || defined(STDC_HEADERS)
#if !defined(STDC_HEADERS)
#include <memory.h>
#endif
#include <string.h>
#define index strchr
#define rindex strrchr
#define bzero(s, n) memset ((s), 0, (n))
#else
#include <strings.h>
char *memchr ();
#endif

#include <errno.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#else
char *getenv ();
extern int errno;
#endif

#if defined(USG) || defined(_POSIX_VERSION)
#include <fcntl.h>
#else
#include <sys/file.h>
#endif

#if !defined(SEEK_SET)
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif

/* Extract or fake data from a `struct stat'.
   ST_BLKSIZE: Optimal I/O blocksize for the file, in bytes. */
#ifdef _POSIX_SOURCE
#define ST_BLKSIZE(statbuf) 1024
#else /* not _POSIX_SOURCE */
#include <sys/param.h>
#ifdef ST_BLKSIZE_MISSING
#define ST_BLKSIZE(statbuf) BSIZE
#else /* not ST_BLKSIZE_MISSING */
/* Some systems, like Sequents, return st_blksize of 0 on pipes. */
#define ST_BLKSIZE(statbuf) ((statbuf).st_blksize > 0 \
			     ? (statbuf).st_blksize : DEV_BSIZE)
#endif /* ST_BLKSIZE_MISSING */
#endif /* _POSIX_SOURCE */

#ifndef S_ISLNK
#define lstat stat
#endif

#ifndef SIGTYPE
#define SIGTYPE void
#endif
