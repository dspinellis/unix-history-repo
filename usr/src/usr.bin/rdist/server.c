#ifndef lint
static	char *sccsid = "@(#)server.c	4.3 (Berkeley) 83/10/10";
#endif

#include "defs.h"

#define	ga() 	(void) write(rem, "", 1)

char	buf[BUFSIZ];		/* general purpose buffer */
char	target[BUFSIZ];		/* target/source directory name */
char	*tp;			/* pointer to end of target name */
int	catname;		/* cat name to target name */
char	*stp[32];		/* stack of saved tp's for directories */
int	sumask;			/* saved umask for creating files */

static struct passwd *p = NULL;
static struct group *g = NULL;

extern	FILE *lfp;		/* log file for mailing changes */

extern char *exptilde();

/*
 * Server routine to read requests and process them.
 * Commands are:
 *	Tname	- Transmit file if out of date
 *	Vname	- Verify if file out of date or not
 *	Qname	- Query if file exists. Return mtime & size if it does.
 */
server()
{
	char cmdbuf[BUFSIZ];
	register char *cp;
	register struct block *bp, *last = NULL;
	static struct block cmdblk = { EXCEPT };

	sumask = umask(0);
	ga();

	for (;;) {
		cp = cmdbuf;
		if (read(rem, cp, 1) <= 0)
			return;
		if (*cp++ == '\n') {
			error("expected control record\n");
			continue;
		}
		do {
			if (read(rem, cp, 1) != 1)
				lostconn();
		} while (*cp++ != '\n' && cp < &cmdbuf[BUFSIZ]);
		*--cp = '\0';
		cp = cmdbuf;
		switch (*cp++) {
		case 'X':  /* add name to list of files to exclude */
			if (*cp == '\0')
				continue;
			bp = ALLOC(block);
			if (bp == NULL)
				fatal("ran out of memory\n");
			bp->b_type = NAME;
			bp->b_next = bp->b_args = NULL;
			bp->b_name = cp = malloc(strlen(cp) + 1);
			if (cp == NULL)
				fatal("ran out of memory\n");
			strcpy(cp, &cmdbuf[1]);
			if (last == NULL) {
				except = &cmdblk;
				cmdblk.b_args = last = bp;
			} else {
				last->b_next = bp;
				last = bp;
			}
			continue;

		case 'T':  /* init target file/directory name */
			catname = 1;	/* target should be directory */
			goto dotarget;

		case 't':  /* init target file/directory name */
			catname = 0;
		dotarget:
			(void) exptilde(target, cp);
			tp = target;
			while (*tp)
				tp++;
			continue;

		case 'S':  /* Send. Transfer file if out of date. */
			sendf(cp, NULL, 0);
			continue;

		case 'V':  /* Verify. See if file is out of date. */
			sendf(cp, NULL, VERIFY);
			continue;

		case 'R':  /* Receive. Transfer file. */
			recvf(cp, 0);
			continue;

		case 'D':  /* Directory. Transfer file. */
			recvf(cp, 1);
			continue;

		case 'E':  /* End. (of directory) */
			*tp = '\0';
			if (--catname < 0) {
				error("too many 'E's\n");
				continue;
			}
			tp = stp[catname];
			*tp = '\0';
			ga();
			continue;

		case 'Q':  /* Query. Does directory exist? */
			query(cp, 1);
			continue;

		case 'q':  /* query. Does file exist? */
			query(cp, 0);
			continue;

		case 'L':  /* Log. save message in log file */
			log(lfp, cp);
			continue;

		case '\1':
			errs++;
			continue;

		case '\2':
			return;

		default:
			error("unknown command type %s\n", cp);
		case '\0':
			continue;
		}
	}
}

/*
 * Transfer the file or directory 'name'.
 */
sendf(lname, rname, options)
	char *lname, *rname;
	int options;
{
	register char *cp;
	struct stat stb;
	int sizerr, f, u;
	off_t i;

	if (debug)
		printf("sendf(%s, %s, %x)\n", lname,
			rname != NULL ? rname : "NULL", options);

	if (exclude(lname))
		return;

	/*
	 * First time sendf() is called?
	 */
	if (rname == NULL) {
		rname = exptilde(target, lname);
		if (rname == NULL)
			return;
		tp = lname = target;
		while (*tp)
			tp++;
		/*
		 * If we are renaming a directory and we want to preserve
		 * the directory heirarchy (-w), we must strip off the first
		 * directory name and preserve the rest.
		 */
		if (options & STRIP) {
			options &= ~STRIP;
			rname = index(rname, '/');
			if (rname == NULL)
				rname = tp;
			else
				rname++;
		} else if (!(options & WHOLE)) {
			rname = rindex(lname, '/');
			if (rname == NULL)
				rname = lname;
			else
				rname++;
		}
	}
	if (access(lname, 4) < 0 || stat(lname, &stb) < 0) {
		error("%s: %s\n", lname, sys_errlist[errno]);
		return;
	}
	if ((u = update(lname, rname, options, &stb)) == 0)
		return;

	if (p == NULL || p->pw_uid != stb.st_uid)
		if ((p = getpwuid(stb.st_uid)) == NULL) {
			error("no password entry for uid %d\n", stb.st_uid);
			return;
		}
	if (g == NULL || g->gr_gid != stb.st_gid)
		if ((g = getgrgid(stb.st_gid)) == NULL) {
			error("no name for group %d\n", stb.st_gid);
			return;
		}

	switch (stb.st_mode & S_IFMT) {
	case S_IFREG:
		break;

	case S_IFDIR:
		rsendf(lname, rname, options, &stb, p->pw_name, g->gr_name);
		return;

	default:
		error("%s: not a plain file\n", lname);
		return;
	}

	log(lfp, "%s: %s\n", u == 2 ? "updating" : "installing", lname);

	if ((options & VERIFY) || vflag)
		return;

	if ((f = open(lname, 0)) < 0) {
		error("%s: %s\n", lname, sys_errlist[errno]);
		return;
	}
	(void) sprintf(buf, "R%04o %D %D %s %s %s\n", stb.st_mode & 07777,
		stb.st_size, stb.st_mtime, p->pw_name, g->gr_name, rname);
	if (debug)
		printf("buf = %s", buf);
	(void) write(rem, buf, strlen(buf));
	if (response() < 0) {
		(void) close(f);
		return;
	}
	sizerr = 0;
	for (i = 0; i < stb.st_size; i += BUFSIZ) {
		int amt = BUFSIZ;
		if (i + amt > stb.st_size)
			amt = stb.st_size - i;
		if (sizerr == 0 && read(f, buf, amt) != amt)
			sizerr = 1;
		(void) write(rem, buf, amt);
	}
	(void) close(f);
	if (sizerr)
		error("%s: file changed size\n", lname);
	else
		ga();
	(void) response();
}

rsendf(lname, rname, options, st, owner, group)
	char *lname, *rname;
	int options;
	struct stat *st;
	char *owner, *group;
{
	DIR *d;
	struct direct *dp;
	char *otp, *cp;
	int len;

	if (debug)
		printf("rsendf(%s, %s, %x, %x, %s, %s)\n", lname, rname,
			options, st, owner, group);

	if ((d = opendir(lname)) == NULL) {
		error("%s: %s\n", lname, sys_errlist[errno]);
		return;
	}
	(void) sprintf(buf, "D%04o 0 0 %s %s %s\n", st->st_mode & 07777,
		owner, group, rname);
	if (debug)
		printf("buf = %s", buf);
	(void) write(rem, buf, strlen(buf));
	if (response() < 0) {
		closedir(d);
		return;
	}
	otp = tp;
	len = tp - target;
	while (dp = readdir(d)) {
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		if (len + 1 + strlen(dp->d_name) >= BUFSIZ - 1) {
			error("%s/%s: Name too long\n", target, dp->d_name);
			continue;
		}
		tp = otp;
		*tp++ = '/';
		cp = dp->d_name;
		while (*tp++ = *cp++)
			;
		tp--;
		sendf(target, dp->d_name, options);
	}
	closedir(d);
	(void) write(rem, "E\n", 2);
	(void) response();
	tp = otp;
	*tp = '\0';
}

/*
 * Check to see if file needs to be updated on the remote machine.
 * Returns 0 if no update, 1 if remote doesn't exist, and 2 if out of date.
 */
update(lname, rname, options, st)
	char *lname, *rname;
	int options;
	struct stat *st;
{
	register char *cp;
	register off_t size;
	register time_t mtime;

	if (debug) 
		printf("update(%s, %s, %x, %x)\n", lname, rname, options, st);

	/*
	 * Check to see if the file exists on the remote machine.
	 */
	(void) sprintf(buf, "%c%s\n",
		(st->st_mode & S_IFMT) == S_IFDIR ? 'Q' : 'q', rname);
	if (debug)
		printf("buf = %s", buf);
	(void) write(rem, buf, strlen(buf));
	cp = buf;
	do {
		if (read(rem, cp, 1) != 1)
			lostconn();
	} while (*cp++ != '\n' && cp < &buf[BUFSIZ]);

	switch (buf[0]) {
	case 'Y':
		break;

	case 'N':  /* file doesn't exist so install it */
		return(1);

	case '\1':
		errs++;
		if (cp > &buf[2]) {
			if (!iamremote) {
				fflush(stdout);
				(void) write(2, cp, cp - buf);
			}
			if (lfp != NULL)
				(void) fwrite(cp, 1, cp - buf, lfp);
		}
		return(0);

	default:
		error("unexpected response '%c' to query\n", buf[0]);
		return(0);
	}

	cp = &buf[1];
	if (*cp == '\n')
		return(2);

	size = 0;
	while (isdigit(*cp))
		size = size * 10 + (*cp++ - '0');
	if (*cp++ != ' ') {
		error("size not delimited\n");
		return(0);
	}
	mtime = 0;
	while (isdigit(*cp))
		mtime = mtime * 10 + (*cp++ - '0');
	if (*cp != '\n') {
		error("mtime not delimited\n");
		return(0);
	}
	/*
	 * File needs to be updated?
	 */
	if (yflag || (options & YOUNGER)) {
		if (st->st_mtime == mtime)
			return(0);
		if (st->st_mtime < mtime) {
			log(lfp, "Warning: %s older than remote copy\n", lname);
			return(0);
		}
	} else if (st->st_mtime == mtime && st->st_size == size)
		return(0);
	return(2);
}

/*
 * Query. Check to see if file exists. Return one of the following:
 *	N\n		- doesn't exist
 *	Ysize mtime\n	- exists and its a regular file (size & mtime of file)
 *	Y\n		- exists and its a directory
 *	^Aerror message\n
 */
query(name, isdir)
	char *name;
{
	struct stat stb;

	if (catname)
		(void) sprintf(tp, "/%s", name);

again:
	if (stat(target, &stb) < 0) {
		(void) write(rem, "N\n", 2);
		*tp = '\0';
		return;
	}

	switch (stb.st_mode & S_IFMT) {
	case S_IFREG:
		(void) sprintf(buf, "Y%D %D\n", stb.st_size, stb.st_mtime);
		(void) write(rem, buf, strlen(buf));
		break;

	case S_IFDIR:
		/*
		 * If file -> directory, need to cat name to target and stat.
		 */
		if (!isdir && !catname) {
			isdir = 1;
			(void) sprintf(tp, "/%s", name);
			goto again;
		}
		(void) write(rem, "Y\n", 2);
		break;

	default:
		error("%s: not a plain file\n", name);
		break;
	}
	*tp = '\0';
}

recvf(cmd, isdir)
	char *cmd;
	int isdir;
{
	register char *cp;
	int f, mode, wrerr, olderrno;
	off_t i, size;
	time_t mtime;
	struct stat stb;
	struct timeval tvp[2];
	char *owner, *group, *dir;
	char new[BUFSIZ];
	extern char *tmpname;

	mode = 0;
	for (cp = cmd; cp < cmd+4; cp++) {
		if (*cp < '0' || *cp > '7') {
			error("bad mode\n");
			return;
		}
		mode = (mode << 3) | (*cp - '0');
	}
	if (*cp++ != ' ') {
		error("mode not delimited\n");
		return;
	}
	size = 0;
	while (isdigit(*cp))
		size = size * 10 + (*cp++ - '0');
	if (*cp++ != ' ') {
		error("size not delimited\n");
		return;
	}
	mtime = 0;
	while (isdigit(*cp))
		mtime = mtime * 10 + (*cp++ - '0');
	if (*cp++ != ' ') {
		error("mtime not delimited\n");
		return;
	}
	owner = cp;
	while (*cp && *cp != ' ')
		cp++;
	if (*cp != ' ') {
		error("owner name not delimited\n");
		return;
	}
	*cp++ = '\0';
	group = cp;
	while (*cp && *cp != ' ')
		cp++;
	if (*cp != ' ') {
		error("group name not delimited\n");
		return;
	}
	*cp++ = '\0';

	new[0] = '\0';
	if (isdir) {
		if (catname >= sizeof(stp)) {
			error("%s: too many directory levels\n", target);
			return;
		}
		stp[catname] = tp;
		if (catname++) {
			*tp++ = '/';
			while (*tp++ = *cp++)
				;
			tp--;
		}
		if (vflag) {
			ga();
			return;
		}
		if (stat(target, &stb) == 0) {
			if ((stb.st_mode & S_IFMT) != S_IFDIR) {
				errno = ENOTDIR;
				goto bad;
			}
		} else {
			if (chkparent(target) < 0)
				goto bad;
			if (mkdir(target, mode) < 0)
				goto bad;
			if (chog(target, owner, group, mode) < 0)
				return;
		}
		ga();
		return;
	}

	if (catname)
		(void) sprintf(tp, "/%s", cp);
	if (stat(target, &stb) == 0) {
		switch (stb.st_mode & S_IFMT) {
		case S_IFREG:
			break;

		case S_IFDIR:
			if (!catname) {
				(void) sprintf(tp, "/%s", cp);
				break;
			}

		default:
			error("%s: not a regular file\n", target);
			return;
		}
	}
	if (chkparent(target) < 0)
		goto bad;
	cp = rindex(target, '/');
	if (cp == NULL)
		dir = ".";
	else if (cp == target) {
		dir = "/";
		cp = NULL;
	} else {
		dir = target;
		*cp = '\0';
	}
	(void) sprintf(new, "%s/%s", dir, tmpname);
	if (cp != NULL)
		*cp = '/';
	if ((f = creat(new, mode)) < 0)
		goto bad1;
	if (chog(new, owner, group, mode) < 0) {
		(void) close(f);
		(void) unlink(new);
		return;
	}
	ga();

	wrerr = 0;
	for (i = 0; i < size; i += BUFSIZ) {
		int amt = BUFSIZ;

		cp = buf;
		if (i + amt > size)
			amt = size - i;
		do {
			int j = read(rem, cp, amt);

			if (j <= 0) {
				(void) close(f);
				(void) unlink(new);
				cleanup();
			}
			amt -= j;
			cp += j;
		} while (amt > 0);
		amt = BUFSIZ;
		if (i + amt > size)
			amt = size - i;
		if (wrerr == 0 && write(f, buf, amt) != amt) {
			olderrno = errno;
			wrerr++;
		}
	}
	(void) response();
	if (wrerr) {
		error("%s: %s\n", cp, sys_errlist[olderrno]);
		(void) close(f);
		(void) unlink(new);
		return;
	}

	/*
	 * Set last modified time
	 */
	(void) fstat(f, &stb);
	(void) close(f);
	tvp[0].tv_sec = stb.st_atime;
	tvp[0].tv_usec = 0;
	tvp[1].tv_sec = mtime;
	tvp[1].tv_usec = 0;
	if (utimes(new, tvp) < 0) {
bad1:
		error("%s: %s\n", new, sys_errlist[errno]);
		if (new[0])
			(void) unlink(new);
		return;
	}
	
	if (rename(new, target) < 0) {
bad:
		error("%s: %s\n", target, sys_errlist[errno]);
		if (new[0])
			(void) unlink(new);
		return;
	}
	ga();
}

/*
 * Check parent directory for write permission and create if it doesn't
 * exist.
 */
chkparent(name)
	char *name;
{
	register char *cp, *dir;
	extern int userid, groupid;

	cp = rindex(name, '/');
	if (cp == NULL)
		dir = ".";
	else if (cp == name) {
		dir = "/";
		cp = NULL;
	} else {
		dir = name;
		*cp = '\0';
	}
	if (access(dir, 2) == 0) {
		if (cp != NULL)
			*cp = '/';
		return(0);
	}
	if (errno == ENOENT) {
		if (rindex(dir, '/') != NULL && chkparent(dir) < 0)
			goto bad;
		if (!strcmp(dir, ".") || !strcmp(dir, "/"))
			goto bad;
		if (mkdir(dir, 0777 & ~sumask) < 0)
			goto bad;
		if (chown(dir, userid, groupid) < 0) {
			(void) unlink(dir);
			goto bad;
		}
		if (cp != NULL)
			*cp = '/';
		return(0);
	}

bad:
	if (cp != NULL)
		*cp = '/';
	return(-1);
}

/*
 * Change owner and group of file.
 */
chog(file, owner, group, mode)
	char *file, *owner, *group;
	int mode;
{
	extern int userid, groupid;
	extern char user[];
	register int i;
	int uid, gid;

	uid = userid;
	if (userid == 0) {
		if (p == NULL || strcmp(owner, p->pw_name) != 0) {
			if ((p = getpwnam(owner)) == NULL) {
				if (mode & 04000) {
					error("%s: unknown login name\n", owner);
					return(-1);
				}
			} else
				uid = p->pw_uid;
		} else
			uid = p->pw_uid;
	}
	gid = groupid;
	if (g == NULL || strcmp(group, g->gr_name) != 0) {
		if ((g = getgrnam(group)) == NULL) {
			if (mode & 02000) {
				error("%s: unknown group\n", group);
				return(-1);
			}
		} else
			gid = g->gr_gid;
	} else
		gid = g->gr_gid;
	if (userid && groupid != gid) {
		for (i = 0; g->gr_mem[i] != NULL; i++)
			if (!(strcmp(user, g->gr_mem[i])))
				goto ok;
		gid = groupid;
	}
ok:
	if (chown(file, uid, gid) < 0) {
		error("%s: %s\n", file, sys_errlist[errno]);
		return(-1);
	}
	return(0);
}

/*VARARGS2*/
log(fp, fmt, a1, a2, a3)
	FILE *fp;
	char *fmt;
	int a1, a2, a3;
{
	/* Print changes locally if not quiet mode */
	if (!qflag)
		printf(fmt, a1, a2, a3);

	/* Save changes (for mailing) if really updating files */
	if (!vflag && fp != NULL)
		fprintf(fp, fmt, a1, a2, a3);
}

/*VARARGS1*/
error(fmt, a1, a2, a3)
	char *fmt;
	int a1, a2, a3;
{
	errs++;
	strcpy(buf, "\1rdist: ");
	(void) sprintf(buf+8, fmt, a1, a2, a3);
	(void) write(rem, buf, strlen(buf));
	if (!iamremote) {
		fflush(stdout);
		(void) write(2, buf+1, strlen(buf+1));
	}
	if (lfp != NULL)
		(void) fwrite(buf+1, 1, strlen(buf+1), lfp);
}

/*VARARGS1*/
fatal(fmt, a1, a2,a3)
	char *fmt;
	int a1, a2, a3;
{
	errs++;
	strcpy(buf, "\2rdist: ");
	(void) sprintf(buf+8, fmt, a1, a2, a3);
	(void) write(rem, buf, strlen(buf));
	if (!iamremote) {
		fflush(stdout);
		(void) write(2, buf+1, strlen(buf+1));
	}
	if (lfp != NULL)
		(void) fwrite(buf+1, 1, strlen(buf+1), lfp);
	cleanup();
}

response()
{
	char resp, *cp = buf;

	if (debug)
		printf("response()\n");

	if (read(rem, &resp, 1) != 1)
		lostconn();

	switch (resp) {
	case '\0':
		return(0);

	default:
		*cp++ = resp;
		/* fall into... */
	case '\1':
	case '\2':
		errs++;
		do {
			if (read(rem, cp, 1) != 1)
				lostconn();
		} while (*cp++ != '\n' && cp < &buf[BUFSIZ]);
		if (buf[0] != '\n') {
			if (!iamremote) {
				fflush(stdout);
				(void) write(2, buf, cp - buf);
			}
			if (lfp != NULL)
				(void) fwrite(buf, 1, cp - buf, lfp);
		}
		if (resp == '\1')
			return(-1);
		cleanup();
	}
	/*NOTREACHED*/
}

lostconn()
{
	if (!iamremote)
		fprintf(stderr, "rdist: lost connection\n");
	cleanup();
}
