/* fsusage.c -- return space usage of mounted filesystems
   Copyright (C) 1991, 1992 Free Software Foundation, Inc.

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

#include <sys/types.h>
#include "fsusage.h"

int statfs ();

#ifdef STAT_STATFS2_BSIZE	/* 4.3BSD, SunOS 4, HP-UX.  */
#include <sys/vfs.h>
#endif

#ifdef STAT_STATFS2_FSIZE	/* 4.4BSD.  */
#include <sys/mount.h>
#endif

#ifdef STAT_STATFS2_FS_DATA	/* Ultrix.  */
#include <sys/param.h>
#include <sys/mount.h>
#endif

#ifdef STAT_READ		/* SVR2.  */
#include <sys/param.h>
#include <sys/filsys.h>
#include <fcntl.h>
#endif

#ifdef STAT_STATFS4		/* SVR3, Dynix, Irix.  */
#include <sys/statfs.h>
#endif

#ifdef STAT_STATVFS		/* SVR4.  */
#include <sys/statvfs.h>
int statvfs ();
#endif

/* Return the number of NEWSIZE-byte blocks used by
   BLOCKS OLDSIZE-byte blocks, rounding up.  */

static long
adjust_blocks (blocks, oldsize, newsize)
     long blocks;
     int oldsize, newsize;
{
  if (oldsize == newsize)	/* E.g., from 1024 to 1024.  */
    return blocks;
  else if (oldsize < newsize)	/* E.g., from 512 to 1024.  */
    return (blocks + 1) / (newsize / oldsize);
  else				/* E.g., from 2048 to 1024.  */
    return blocks * (oldsize / newsize);
}

/* Fill in the fields of FSP with information about space usage for
   the filesystem on which PATH resides.
   Return 0 if successful, -1 if not. */

int
get_fs_usage (path, fsp)
     char *path;
     struct fs_usage *fsp;
{
#ifdef STAT_STATFS2_FS_DATA	/* Ultrix.  */
  struct fs_data fsd;

  if (statfs (path, &fsd) != 1)
    return -1;
#define blocks_to_k(b) adjust_blocks ((b), 1024, 1024)
  fsp->fsu_blocks = blocks_to_k (fsd.fd_req.btot);
  fsp->fsu_bfree = blocks_to_k (fsd.fd_req.bfree);
  fsp->fsu_bavail = blocks_to_k (fsd.fd_req.bfreen);
  fsp->fsu_files = fsd.fd_req.gtot;
  fsp->fsu_ffree = fsd.fd_req.gfree;
#endif

#ifdef STAT_READ		/* SVR2.  */
#ifndef SUPERBOFF
#define SUPERBOFF (SUPERB * 512)
#endif
  struct filsys fsd;
  int fd;

  fd = open (path, O_RDONLY);
  if (fd < 0)
    return -1;
  lseek (fd, (long) SUPERBOFF, 0);
  if (read (fd, (char *) &fsd, sizeof fsd) != sizeof fsd)
    {
      close (fd);
      return -1;
    }
  close (fd);
#define blocks_to_k(b) adjust_blocks ((b), (fsd.s_type == Fs2b ? 1024 : 512), 1024)
  fsp->fsu_blocks = blocks_to_k (fsd.s_fsize);
  fsp->fsu_bfree = blocks_to_k (fsd.s_tfree);
  fsp->fsu_bavail = blocks_to_k (fsd.s_tfree);
  fsp->fsu_files = fsd.s_isize * INOPB;
  fsp->fsu_ffree = fsd.s_tinode;
#endif

#ifdef STAT_STATFS2_BSIZE	/* 4.3BSD, SunOS 4, HP-UX.  */
  struct statfs fsd;

  if (statfs (path, &fsd) < 0)
    return -1;
#define blocks_to_k(b) adjust_blocks ((b), fsd.f_bsize, 1024)
#endif

#ifdef STAT_STATFS2_FSIZE	/* 4.4BSD.  */
  struct statfs fsd;

  if (statfs (path, &fsd) < 0)
    return -1;
#define blocks_to_k(b) adjust_blocks ((b), fsd.f_fsize, 1024)
#endif

#ifdef STAT_STATFS4		/* SVR3, Dynix, Irix.  */
  struct statfs fsd;

  if (statfs (path, &fsd, sizeof fsd, 0) < 0)
    return -1;
  /* Don't use fsd.f_bsize; empirically, the block counts on SVR3 and
     SVR3-derived systems seem to always be in terms of 512-byte blocks,
     no matter what value f_bsize has.  */
#define blocks_to_k(b) adjust_blocks ((b), 512, 1024)
#define f_bavail f_bfree
#endif

#ifdef STAT_STATVFS		/* SVR4.  */
  struct statvfs fsd;

  if (statvfs (path, &fsd) < 0)
    return -1;
#define blocks_to_k(b) adjust_blocks ((b), fsd.f_frsize, 1024)
#endif

#if !defined(STAT_STATFS2_FS_DATA) && !defined(STAT_READ) /* !Ultrix && !SVR2.  */
  fsp->fsu_blocks = blocks_to_k (fsd.f_blocks);
  fsp->fsu_bfree = blocks_to_k (fsd.f_bfree);
  fsp->fsu_bavail = blocks_to_k (fsd.f_bavail);
  fsp->fsu_files = fsd.f_files;
  fsp->fsu_ffree = fsd.f_ffree;
#endif

  return 0;
}
