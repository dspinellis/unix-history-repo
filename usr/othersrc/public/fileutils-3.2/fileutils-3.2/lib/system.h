/* system-dependent definitions for fileutils programs.
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
#include <unistd.h>
#endif

#ifndef _POSIX_VERSION
off_t lseek ();
#ifdef USG
#include <sys/times.h>
#else /* !USG */
#include <sys/time.h>
#endif /* !USG */
#endif /* !_POSIX_VERSION */

#if defined(_POSIX_VERSION) || defined(STDC_HEADERS) || defined(HAVE_LIMITS_H)
#include <limits.h>
#endif /* !_POSIX_VERSION && !STDC_HEADERS && !HAVE_LIMITS_H */

#ifndef _POSIX_SOURCE
#include <sys/param.h>
#endif

#ifndef _POSIX_PATH_MAX
#define _POSIX_PATH_MAX 255
#endif

#ifndef PATH_MAX
#ifdef _PC_PATH_MAX
#define PATH_MAX pathconf ("/", _PC_PATH_MAX)
#else /* !_PC_PATH_MAX */
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else /* not MAXPATHLEN */
#define PATH_MAX _POSIX_PATH_MAX
#endif /* MAXPATHLEN */
#endif /* !_PC_PATH_MAX */
#endif /* !PATH_MAX */

#ifdef MAJOR_IN_MKDEV
#include <sys/mkdev.h>
#endif
#ifdef MAJOR_IN_SYSMACROS
#include <sys/sysmacros.h>
#endif
#ifndef major
#define major(dev)  (((dev) >> 8) & 0xff)
#define minor(dev)  ((dev) & 0xff)
#define makedev(maj, min)  (((maj) << 8) | (min))
#endif /* major */

#ifdef _POSIX_VERSION
#include <utime.h>
#else /* not _POSIX_VERSION */
struct utimbuf
{
  long actime;
  long modtime;
};
#endif /* _POSIX_VERSION */

#if defined(USG) || defined(STDC_HEADERS)
#include <string.h>
#define index strchr
#define rindex strrchr
#define bcopy(from, to, len) memcpy ((to), (from), (len))
#define bzero(s, n) memset ((s), 0, (n))
#else /* not (USG or STDC_HEADERS) */
#include <strings.h>
#endif /* USG or STDC_HEADERS */

#include <errno.h>
#ifdef STDC_HEADERS
#include <stdlib.h>
#else /* not STDC_HEADERS */
char *getenv ();
extern int errno;
#endif /* STDC_HEADERS */

#if defined(USG) || defined(_POSIX_VERSION)
#include <fcntl.h>
#else /* not (USG or _POSIX_VERSION) */
#include <sys/file.h>
#endif /* USG or _POSIX_VERSION */

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif
#ifndef F_OK
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
#endif

#if defined(DIRENT) || defined(_POSIX_VERSION)
#include <dirent.h>
#ifdef direct
#undef direct
#endif
#define direct dirent
#define NLENGTH(direct) (strlen((direct)->d_name))
#else /* not (DIRENT or _POSIX_VERSION) */
#define NLENGTH(direct) ((direct)->d_namlen)
#ifdef USG
#ifdef SYSNDIR
#include <sys/ndir.h>
#else /* not SYSNDIR */
#include <ndir.h>
#endif /* SYSNDIR */
#else /* not USG */
#include <sys/dir.h>
#endif /* USG */
#endif /* DIRENT or _POSIX_VERSION */

#ifdef VOID_CLOSEDIR
/* Fake a return value. */
#define CLOSEDIR(d) (closedir (d), 0)
#else
#define CLOSEDIR(d) closedir (d)
#endif

/* Extract or fake data from a `struct stat'.
   ST_BLKSIZE: Optimal I/O blocksize for the file, in bytes.
   ST_NBLOCKS: Number of 512-byte blocks in the file
   (including indirect blocks). */
#ifdef ST_BLOCKS_MISSING
# if defined(_POSIX_SOURCE) || !defined(BSIZE)
#  define ST_BLKSIZE(statbuf) 1024
#  define ST_NBLOCKS(statbuf) (((statbuf).st_size + 512 - 1) / 512)
# else /* !_POSIX_SOURCE && BSIZE */
#  define ST_BLKSIZE(statbuf) BSIZE
#  define ST_NBLOCKS(statbuf) (st_blocks ((statbuf).st_size))
# endif /* !_POSIX_SOURCE && BSIZE */
#else /* !ST_BLOCKS_MISSING */
/* Some systems, like Sequents, return st_blksize of 0 on pipes. */
# define ST_BLKSIZE(statbuf) ((statbuf).st_blksize > 0 \
			       ? (statbuf).st_blksize : DEV_BSIZE)
# if defined(hpux) || defined(__hpux__)
/* HP-UX, perhaps uniquely, counts st_blocks in 1024-byte units.
   This loses when mixing HP-UX and 4BSD filesystems, though. */
#  define ST_NBLOCKS(statbuf) ((statbuf).st_blocks * 2)
# else /* !hpux */
#  define ST_NBLOCKS(statbuf) ((statbuf).st_blocks)
# endif /* !hpux */
#endif /* !ST_BLOCKS_MISSING */

/* Convert B 512-byte blocks to kilobytes if K is nonzero,
   otherwise return it unchanged. */
#define convert_blocks(b, k) ((k) ? ((b) + 1) / 2 : (b))

#ifndef S_ISLNK
#define lstat stat
#endif

#ifndef SIGTYPE
#define SIGTYPE void
#endif

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef sparc
#include <alloca.h>
#else
#ifndef _AIX
/* AIX alloca decl has to be the first thing in the file, bletch! */
char *alloca ();
#endif
#endif
#endif
