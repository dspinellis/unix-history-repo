#ifndef lint
static	char *sccsid = "@(#)docmd.c	4.2 (Berkeley) 83/09/27";
#endif

#include "defs.h"

FILE	*lfp;		/* log file for recording files updated */

/*
 * Process commands for sending files to other machines.
 */
dohcmds(files, hosts, cmds)
	struct block *files, *hosts, *cmds;
{
	register struct block *h, *f, *c;
	register char *cp, **cpp;
	int n, ddir;

	if (debug)
		printf("dohcmds(%x, %x, %x)\n", files, hosts, cmds);

	files = expand(files, 0);
	hosts = expand(hosts, 1);
	if (files == NULL)
		fatal("no files to be updated\n");
	if (hosts == NULL)
		fatal("empty list of hosts to be updated\n");
	except = cmds;
	ddir = files->b_next != NULL;

	for (h = hosts; h != NULL; h = h->b_next) {
		if (!qflag)
			printf("updating host %s\n", h->b_name);
		if (!nflag) {
			if (!makeconn(h->b_name))
				continue;
			if ((lfp = fopen(tmpfile, "w")) == NULL) {
				fatal("cannot open %s\n", tmpfile);
				exit(1);
			}
		}
		for (f = files; f != NULL; f = f->b_next) {
			if (filec) {
				for (cpp = filev; *cpp; cpp++)
					if (!strcmp(f->b_name, *cpp))
						goto found;
				if (!nflag) {
					(void) fclose(lfp);
				}
				continue;
			}
		found:
			n = 0;
			for (c = cmds; c != NULL; c = c->b_next)
				if (c->b_type == INSTALL) {
					install(f->b_name, c->b_name, ddir, 0);
					n++;
				} else if (c->b_type == VERIFY) {
					install(f->b_name, c->b_name, ddir, 1);
					n++;
				}
			if (n == 0)
				install(f->b_name, f->b_name, 0, 0);
		}
		if (!nflag) {
			/* signal end of connection */
			(void) write(rem, "\2\n", 2);
			(void) close(rem);
			(void) fclose(lfp);
		}
		for (c = cmds; c != NULL; c = c->b_next)
			if (c->b_type == NOTIFY)
				notify(tmpfile, h->b_name, c->b_args);
	}
	if (!nflag)
		(void) unlink(tmpfile);
}

/*
 * Create a connection to the rdist server on the machine rhost.
 */
makeconn(rhost)
	char *rhost;
{
	register char *ruser;
	extern char user[];

	(void) sprintf(buf, "/usr/local/rdist -Server%s%s%s%s%s",
		vflag ? " -v" : "", qflag ? " -q" : "", nflag ? " -n" : "",
		yflag ? " -y" : "", debug ? " -D" : "");

	ruser = rindex(rhost, '.');
	if (ruser != NULL) {
		*ruser++ = '\0';
		if (!okname(ruser))
			return(0);
	} else
		ruser = user;

	if (debug) {
		printf("makeconn(%s)\n", rhost);
		printf("luser = %s, ruser = %s\n", user, ruser);
		printf("buf = %s\n", buf);
	}

	fflush(stdout);
	rem = rcmd(&rhost, IPPORT_CMDSERVER, user, ruser, buf, 0);
	if (rem < 0)
		return(0);
	if (response() < 0)
		return(0);
	return(1);
}

extern char target[], *tp;

/*
 * Update the file(s) if they are different.
 * destdir = 1 if destination should be a directory
 * (i.e., more than one source is being copied to the same destination).
 */
install(src, dest, destdir, verify)
	char *src, *dest;
	int destdir, verify;
{
	if (exclude(src))
		return;

	if (nflag) {
		printf("%s %s %s\n", verify ? "verify" : "install", src, dest);
		return;
	}
	/*
	 * Pass the destination file/directory name to remote.
	 */
	(void) sprintf(buf, "%c%s\n", destdir ? 'T' : 't', dest);
	if (debug)
		printf("buf = %s", buf);
	(void) write(rem, buf, strlen(buf));
	tp = NULL;
	sendf(src, verify);
}

struct tstamp {
	time_t	lastmod;
	FILE	*tfp;
} ts[NSTAMPS];

int	nstamps;

/*
 * Process commands for comparing files to time stamp files.
 */
dofcmds(files, stamps, cmds)
	struct block *files, *stamps, *cmds;
{
	register struct block *b;
	register struct tstamp *t;
	register char **cpp;
	struct stat stb;
	extern char *tmpinc;
	int n;

	if (debug)
		printf("dofcmds()\n");

	files = expand(files, 0);
	stamps = expand(stamps, 1);
	if (files == NULL)
		fatal("no files to be updated\n");
	if (stamps == NULL)
		fatal("empty time stamp file list\n");
	except = cmds;

	t = ts;
	nstamps = 0;
	for (b = stamps; b != NULL; b = b->b_next) {
		if (stat(b->b_name, &stb) < 0) {
			error("%s: %s\n", b->b_name, sys_errlist[errno]);
			continue;
		}
		if (++nstamps > NSTAMPS)
			fatal("too many time stamp files in one command\n");
		if (debug)
			printf("%s: %d\n", b->b_name, stb.st_mtime);
		t->lastmod = stb.st_mtime;
		if (!nflag && !vflag) {
			if ((t->tfp = fopen(tmpfile, "w")) == NULL)
				error("%s: %s\n", b->b_name, sys_errlist[errno]);
			(*tmpinc)++;
		} else
			t->tfp = NULL;
		t++;
	}
	for (b = files; b != NULL; b = b->b_next) {
		if (filec) {
			for (cpp = filev; *cpp; cpp++)
				if (!strcmp(b->b_name, *cpp))
					goto found;
			continue;
		}
	found:
		tp = NULL;
		cmptime(b->b_name);
	}
	if (!nflag && !vflag)
		for (t = ts; t < &ts[n]; t++)
			if (t->tfp != NULL)
				(void) fclose(t->tfp);
	*tmpinc = 'A';
	while (n--) {
		for (b = cmds; b != NULL; b = b->b_next)
			if (b->b_type == NOTIFY)
				notify(tmpfile, NULL, b->b_args);
		if (!nflag && !vflag)
			(void) unlink(tmpfile);
		(*tmpinc)++;
	}
}

/*
 * Compare the mtime of file to the list of time stamps.
 */
cmptime(name)
	char *name;
{
	register struct tstamp *t;
	struct stat stb;

	if (debug)
		printf("cmptime(%s)\n", name);

	if (exclude(name))
		return;

	/*
	 * first time cmptime() is called?
	 */
	if (tp == NULL) {
		exptilde(target, name);
		tp = name = target;
		while (*tp)
			tp++;
	}
	if (access(name, 4) < 0 || stat(name, &stb) < 0) {
		error("%s: %s\n", name, sys_errlist[errno]);
		return;
	}

	switch (stb.st_mode & S_IFMT) {
	case S_IFREG:
		break;

	case S_IFDIR:
		rcmptime(&stb);
		return;

	default:
		error("%s: not a plain file\n", name);
		return;
	}

	for (t = ts; t < &ts[nstamps]; t++) {
		if (stb.st_mtime <= t->lastmod)
			return;
		log(t->tfp, "updating: %s\n", name);
	}
}

rcmptime(st)
	struct stat *st;
{
	register DIR *d;
	register struct direct *dp;
	register char *cp;
	char *otp;
	int len;

	if (debug)
		printf("rcmptime(%x)\n", st);

	if ((d = opendir(target)) == NULL) {
		error("%s: %s\n", target, sys_errlist[errno]);
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
		cmptime(target);
	}
	closedir(d);
	tp = otp;
	*tp = '\0';
}

/*
 * Notify the list of people the changes that were made.
 */
notify(file, host, to)
	char *file, *host;
	register struct block *to;
{
	register int fd, len;
	FILE *pf, *popen();
	struct stat stb;

	if (vflag)
		return;
	if (!qflag) {
		printf("notify ");
		if (host)
			printf("@%s ", host);
		prnames(to);
	}
	if (nflag)
		return;

	if ((fd = open(file, 0)) < 0) {
		error("%s: %s\n", file, sys_errlist[errno]);
		return;
	}
	if (fstat(fd, &stb) < 0) {
		error("%s: %s\n", file, sys_errlist[errno]);
		(void) close(fd);
		return;
	}
	if (stb.st_size == 0) {
		(void) close(fd);
		return;
	}
	/*
	 * Create a pipe to mailling program.
	 */
	pf = popen(MAILCMD, "w");
	if (pf == NULL)
		fatal("notify: \"%s\" failed\n", MAILCMD);
	/*
	 * Output the proper header information.
	 */
	fprintf(pf, "From: rdist (Remote distribution program)\n");
	fprintf(pf, "To:");
	while (to != NULL) {
		if (!any('@', to->b_name))
			fprintf(pf, " %s@%s", to->b_name, host);
		else
			fprintf(pf, " %s", to->b_name);
		to = to->b_next;
	}
	putc('\n', pf);
	fprintf(pf, "Subject: files updated by rdist\n");
	putc('\n', pf);

	while ((len = read(fd, buf, BUFSIZ)) > 0)
		(void) fwrite(buf, 1, len, pf);
	(void) close(fd);
	(void) pclose(pf);
}

struct	block *except;		/* list of files to exclude */

/*
 * Return true if name is in list.
 */
exclude(file)
	char *file;
{
	register struct block *b, *c;

	for (c = except; c != NULL; c = c->b_next) {
		if (c->b_type != EXCEPT)
			continue;
		for (b = c->b_args; b != NULL; b = b->b_next)
			if (!strcmp(file, b->b_name))
				return(1);
	}
	return(0);
}

okname(name)
	register char *name;
{
	register char *cp = name;
	register int c;

	do {
		c = *cp;
		if (c & 0200)
			goto bad;
		if (!isalpha(c) && !isdigit(c) && c != '_' && c != '-')
			goto bad;
		cp++;
	} while (*cp);
	return(1);
bad:
	error("invalid user name %s\n", name);
	return(0);
}

char *
colon(cp)
	register char *cp;
{

	while (*cp) {
		if (*cp == ':')
			return(cp);
		if (*cp == '/')
			return(0);
		cp++;
	}
	return(0);
}
