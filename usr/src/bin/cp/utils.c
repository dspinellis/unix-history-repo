/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)utils.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/time.h>

#include <errno.h>
#include <fcntl.h>
#include <fts.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "extern.h"

void
copy_file(entp, dne)
	FTSENT *entp;
	int dne;
{
	static char buf[MAXBSIZE];
	register int from_fd, to_fd, rcount, wcount;
	struct stat to_stat, *fs;
	char *p;
	

	if ((from_fd = open(entp->fts_path, O_RDONLY, 0)) == -1) {
		err("%s: %s", entp->fts_path, strerror(errno));
		return;
	}

	fs = entp->fts_statp;

	/*
	 * If the file exists and we're interactive, verify with the user.
	 * If the file DNE, set the mode to be the from file, minus setuid
	 * bits, modified by the umask; arguably wrong, but it makes copying
	 * executables work right and it's been that way forever.  (The
	 * other choice is 666 or'ed with the execute bits on the from file
	 * modified by the umask.)
	 */
	if (!dne) {
		if (iflag) {
			int checkch, ch;

			(void)fprintf(stderr, "overwrite %s? ", to.p_path);
			checkch = ch = getchar();
			while (ch != '\n' && ch != EOF)
				ch = getchar();
			if (checkch != 'y') {
				(void)close(from_fd);
				return;
			}
		}
		to_fd = open(to.p_path, O_WRONLY|O_TRUNC, 0);
	} else
		to_fd = open(to.p_path, O_WRONLY|O_CREAT|O_TRUNC,
		    fs->st_mode & ~(S_ISUID|S_ISGID));

	if (to_fd == -1) {
		err("%s: %s", to.p_path, strerror(errno));
		(void)close(from_fd);
		return;
	}

	/*
	 * Mmap and write if less than 8M (the limit is so we don't totally
	 * trash memory on big files.  This is really a minor hack, but it
	 * wins some CPU back.
	 */
#ifdef VM_AND_BUFFER_CACHE_FIXED
	if (fs->st_size <= 8 * 1048576) {
		if ((p = mmap(NULL, (size_t)fs->st_size, PROT_READ,
		    0, from_fd, (off_t)0)) == (char *)-1)
			err("%s: %s", entp->fts_path, strerror(errno));
		if (write(to_fd, p, fs->st_size) != fs->st_size)
			err("%s: %s", to.p_path, strerror(errno));
		/* Some systems don't unmap on close(2). */
		if (munmap(p, fs->st_size) < 0)
			err("%s: %s", entp->fts_path, strerror(errno));
	} else
#endif
	{
		while ((rcount = read(from_fd, buf, MAXBSIZE)) > 0) {
			wcount = write(to_fd, buf, rcount);
			if (rcount != wcount || wcount == -1) {
				err("%s: %s", to.p_path, strerror(errno));
				break;
			}
		}
		if (rcount < 0)
			err("%s: %s", entp->fts_path, strerror(errno));
	}
	if (pflag)
		setfile(fs, to_fd);
	/*
	 * If the source was setuid or setgid, lose the bits unless the
	 * copy is owned by the same user and group.
	 */
	else if (fs->st_mode & (S_ISUID|S_ISGID) && fs->st_uid == myuid)
		if (fstat(to_fd, &to_stat))
			err("%s: %s", to.p_path, strerror(errno));
#define	RETAINBITS	(S_ISUID|S_ISGID|S_ISVTX|S_IRWXU|S_IRWXG|S_IRWXO)
		else if (fs->st_gid == to_stat.st_gid && fchmod(to_fd,
		    fs->st_mode & RETAINBITS & ~myumask))
			err("%s: %s", to.p_path, strerror(errno));
	(void)close(from_fd);
	if (close(to_fd))
		err("%s: %s", to.p_path, strerror(errno));
}

void
copy_link(p, exists)
	FTSENT *p;
	int exists;
{
	int len;
	char link[MAXPATHLEN];

	if ((len = readlink(p->fts_path, link, sizeof(link))) == -1) {
		err("readlink: %s: %s", p->fts_path, strerror(errno));
		return;
	}
	link[len] = '\0';
	if (exists && unlink(to.p_path)) {
		err("unlink: %s: %s", to.p_path, strerror(errno));
		return;
	}
	if (symlink(link, to.p_path)) {
		err("symlink: %s: %s", link, strerror(errno));
		return;
	}
}

void
copy_fifo(from_stat, exists)
	struct stat *from_stat;
	int exists;
{
	if (exists && unlink(to.p_path)) {
		err("unlink: %s: %s", to.p_path, strerror(errno));
		return;
	}
	if (mkfifo(to.p_path, from_stat->st_mode)) {
		err("mkfifo: %s: %s", to.p_path, strerror(errno));
		return;
	}
	if (pflag)
		setfile(from_stat, 0);
}

void
copy_special(from_stat, exists)
	struct stat *from_stat;
	int exists;
{
	if (exists && unlink(to.p_path)) {
		err("unlink: %s: %s", to.p_path, strerror(errno));
		return;
	}
	if (mknod(to.p_path, from_stat->st_mode,  from_stat->st_rdev)) {
		err("mknod: %s: %s", to.p_path, strerror(errno));
		return;
	}
	if (pflag)
		setfile(from_stat, 0);
}


void
setfile(fs, fd)
	register struct stat *fs;
	int fd;
{
	static struct timeval tv[2];

	fs->st_mode &= S_ISUID|S_ISGID|S_IRWXU|S_IRWXG|S_IRWXO;

	TIMESPEC_TO_TIMEVAL(&tv[0], &fs->st_atimespec);
	TIMESPEC_TO_TIMEVAL(&tv[1], &fs->st_mtimespec);
	if (utimes(to.p_path, tv))
		err("utimes: %s: %s", to.p_path, strerror(errno));
	/*
	 * Changing the ownership probably won't succeed, unless we're root
	 * or POSIX_CHOWN_RESTRICTED is not set.  Set uid/gid before setting
	 * the mode; current BSD behavior is to remove all setuid bits on
	 * chown.  If chown fails, lose setuid/setgid bits.
	 */
	if (fd ? fchown(fd, fs->st_uid, fs->st_gid) :
	    chown(to.p_path, fs->st_uid, fs->st_gid)) {
		if (errno != EPERM)
			err("chown: %s: %s", to.p_path, strerror(errno));
		fs->st_mode &= ~(S_ISUID|S_ISGID);
	}
	if (fd ? fchmod(fd, fs->st_mode) : chmod(to.p_path, fs->st_mode))
		err("chown: %s: %s", to.p_path, strerror(errno));

	if (fd ? fchflags(fd, fs->st_flags) : chflags(to.p_path, fs->st_flags))
		err("chflags: %s: %s", to.p_path, strerror(errno));
}

void
usage()
{
	(void)fprintf(stderr,
"usage: cp [-HRfhip] src target;\n       cp [-HRfhip] src1 ... srcN directory\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "%s: ", progname);
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit_val = 1;
}
