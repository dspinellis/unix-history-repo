/* Remote tape drive defines for tar.
   Copyright (C) 1988 Free Software Foundation

This file is part of GNU Tar.

GNU Tar is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Tar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Tar; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifdef NO_REMOTE
#define _isrmt(f)	0
#define rmtopen		open
#define rmtaccess	access
#define rmtstat		stat
#define rmtcreat	creat
#define rmtlstat	lstat
#define rmtread		read
#define rmtwrite	write
#define rmtlseek	lseek
#define rmtclose	close
#define rmtioctl	ioctl
#define rmtdup		dup
#define rmtfstat	fstat
#define rmtfcntl	fcntl
#define rmtisatty	isatty

extern long lseek();
#else
#ifndef USG
#define strchr index
#endif

#define __REM_BIAS	128
#define RMTIOCTL

#ifndef O_CREAT
#define O_CREAT	01000
#endif
extern char *__rmt_path;
extern char *strchr();

#define _remdev(path)	((__rmt_path=strchr(path, ':')) && strncmp(__rmt_path, ":/dev/", 6)==0)
#define _isrmt(fd)		((fd) >= __REM_BIAS)

#define rmtopen(path,oflag,mode) (_remdev(path) ? __rmt_open(path, oflag, mode, __REM_BIAS) : open(path, oflag, mode))
#define rmtaccess(path, amode)	(_remdev(path) ? 0 : access(path, amode))
#define rmtstat(path, buf)	(_remdev(path) ? (errno = EOPNOTSUPP), -1 : stat(path, buf))
#define rmtcreat(path, mode)	(_remdev(path) ? __rmt_open (path, 1 | O_CREAT, mode, __REM_BIAS) : creat(path, mode))
#define rmtlstat(path,buf)	(_remdev(path) ? (errno = EOPNOTSUPP), -1 : lstat(path,buf))

#define rmtread(fd, buf, n)	(_isrmt(fd) ? __rmt_read(fd - __REM_BIAS, buf, n) : read(fd, buf, n))
#define rmtwrite(fd, buf, n)	(_isrmt(fd) ? __rmt_write(fd - __REM_BIAS, buf, n) : write(fd, buf, n))
#define rmtlseek(fd, off, wh)	(_isrmt(fd) ? __rmt_lseek(fd - __REM_BIAS, off, wh) : lseek(fd, off, wh))
#define rmtclose(fd)		(_isrmt(fd) ? __rmt_close(fd - __REM_BIAS) : close(fd))
#ifdef RMTIOCTL
#define rmtioctl(fd,req,arg)	(_isrmt(fd) ? __rmt_ioctl(fd - __REM_BIAS, req, arg) : ioctl(fd, req, arg))
#else
#define rmtioctl(fd,req,arg)	(_isrmt(fd) ? (errno = EOPNOTSUPP), -1 : ioctl(fd, req, arg))
#endif
#define rmtdup(fd)		(_isrmt(fd) ? (errno = EOPNOTSUPP), -1 : dup(fd))
#define rmtfstat(fd, buf)	(_isrmt(fd) ? (errno = EOPNOTSUPP), -1 : fstat(fd, buf))
#define rmtfcntl(fd,cmd,arg)	(_isrmt(fd) ? (errno = EOPNOTSUPP), -1 : fcntl (fd, cmd, arg))
#define rmtisatty(fd)		(_isrmt(fd) ? 0 : isatty(fd))

#undef RMTIOCTL
extern long lseek(),__rmt_lseek();
#endif
