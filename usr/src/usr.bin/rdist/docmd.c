#ifndef lint
static	char *sccsid = "@(#)docmd.c	4.6 (Berkeley) 83/10/26";
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
	register char **cpp;
	static struct block excpt = { EXCEPT };
	char *cp;
	int n, ddir;

	if (debug)
		printf("dohcmds(%x, %x, %x)\n", files, hosts, cmds);

	files = expand(files, 0);
	if (files == NULL) {
		error("no files to be updated\n");
		return;
	}
	hosts = expand(hosts, 1);
	if (hosts == NULL) {
		error("empty list of hosts to be updated\n");
		return;
	}
	ddir = files->b_next != NULL;
	f = NULL;
	except = NULL;
	for (c = cmds; c != NULL; c = c->b_next) {
		if (c->b_type != EXCEPT)
			continue;
		if (except == NULL)
			except = &excpt;
		for (h = c->b_args; h != NULL; h = h->b_next) {
			cp = h->b_name;
			if (*cp == '~') {
				(void) exptilde(buf, cp);
				cp = buf;
			}
			if (f == NULL)
				except->b_args = f = expand(makeblock(NAME, cp), 0);
			else {
				f->b_next = expand(makeblock(NAME, cp), 0);
				f = f->b_next;
			}
		}
	}

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
				if (!nflag)
					(void) fclose(lfp);
				continue;
			}
		found:
			n = 0;
			for (c = cmds; c != NULL; c = c->b_next) {
				if (c->b_type != INSTALL)
					continue;
				n++;
				if (c->b_name == NULL)
					install(f->b_name, f->b_name, 0, c->b_options);
				else
					install(f->b_name, c->b_name, ddir, c->b_options);
			}
			if (n == 0)
				install(f->b_name, f->b_name, 0, options);
		}
		if (!nflag) {
			/* signal end of connection */
			(void) write(rem, "\2\n", 2);
			(void) close(rem);
			(void) fclose(lfp);
		}
		for (c = cmds; c != NULL; c = c->b_next)
			if (c->b_type == NOTIFY)
				notify(tmpfile, h->b_name, c->b_args, 0);
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
	register struct block *c;
	register char *ruser;
	extern char user[];

	(void) sprintf(buf, "/usr/local/rdist -Server%s%s",
		nflag ? " -n" : "", qflag ? " -q" : "");

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

/*
 * Update the file(s) if they are different.
 * destdir = 1 if destination should be a directory
 * (i.e., more than one source is being copied to the same destination).
 */
install(src, dest, destdir, opts)
	char *src, *dest;
	int destdir, opts;
{
	register char *cp;

	if (exclude(src))
		return;

	if (nflag || debug) {
		printf("%s%s%s%s%s %s %s\n", opts & VERIFY ? "verify":"install",
			opts & WHOLE ? " -w" : "",
			opts & YOUNGER ? " -y" : "",
			opts & COMPARE ? " -b" : "",
			opts & REMOVE ? " -r" : "", src, dest);
		if (nflag)
			return;
	}
	/*
	 * Pass the destination file/directory name to remote.
	 */
	(void) sprintf(buf, "%c%s\n", destdir ? 'T' : 't', dest);
	if (debug)
		printf("buf = %s", buf);
	(void) write(rem, buf, strlen(buf));

	if (!destdir && (opts & WHOLE))
		opts |= STRIP;
	if (opts & REMOVE) {
		opts &= ~REMOVE;
		rmchk(src, NULL, opts);
	}
	sendf(src, NULL, opts);
}

struct tstamp {
	time_t	lastmod;
	FILE	*tfp;
} ts[NSTAMPS];

int	nstamps;

extern char target[], *tp;

/*
 * Process commands for comparing files to time stamp files.
 */
dofcmds(files, stamps, cmds)
	struct block *files, *stamps, *cmds;
{
	register struct block *b;
	register struct tstamp *t;
	register char **cpp;
	struct timeval tv[2];
	struct timezone tz;
	struct stat stb;
	extern char *tmpinc;

	if (debug)
		printf("dofcmds()\n");

	files = expand(files, 0);
	if (files == NULL){
		error("no files to be updated\n");
		return;
	}
	stamps = expand(stamps, 0);
	if (stamps == NULL) {
		error("empty time stamp file list\n");
		return;
	}
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
		(void) gettimeofday(&tv[0], &tz);
		tv[1] = tv[0];
		(void) utimes(b->b_name, tv);
		if (!nflag && !(options & VERIFY)) {
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

	*tmpinc = 'A';
	for (t = ts; t < &ts[nstamps]; t++) {
		if (t->tfp != NULL)
			(void) fclose(t->tfp);
		for (b = cmds; b != NULL; b = b->b_next)
			if (b->b_type == NOTIFY)
				notify(tmpfile, NULL, b->b_args, t->lastmod);
		if (!nflag && !(options & VERIFY))
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
		if (stb.st_mtime > t->lastmod)
			log(t->tfp, "new: %s\n", name);
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
 * rhost == NULL if we are mailing a list of changes compared to at time
 * stamp file.
 */
notify(file, rhost, to, lmod)
	char *file, *rhost;
	register struct block *to;
	time_t lmod;
{
	register int fd, len;
	FILE *pf, *popen();
	struct stat stb;

	if (options & VERIFY)
		return;
	if (!qflag) {
		printf("notify ");
		if (rhost)
			printf("@%s ", rhost);
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
	if (pf == NULL) {
		error("notify: \"%s\" failed\n", MAILCMD);
		(void) close(fd);
		return;
	}
	/*
	 * Output the proper header information.
	 */
	fprintf(pf, "From: rdist (Remote distribution program)\n");
	fprintf(pf, "To:");
	if (!any('@', to->b_name) && host != NULL)
		fprintf(pf, " %s@%s", to->b_name, rhost);
	else
		fprintf(pf, " %s", to->b_name);
	to = to->b_next;
	while (to != NULL) {
		if (!any('@', to->b_name) && host != NULL)
			fprintf(pf, ", %s@%s", to->b_name, rhost);
		else
			fprintf(pf, ", %s", to->b_name);
		to = to->b_next;
	}
	putc('\n', pf);
	if (rhost != NULL)
		fprintf(pf, "Subject: files updated by rdist from %s to %s\n",
			host, rhost);
	else
		fprintf(pf, "Subject: files updated after %s\n", ctime(&lmod));
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
