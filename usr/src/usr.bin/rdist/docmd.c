#ifndef lint
static	char *sccsid = "@(#)docmd.c	4.11 (Berkeley) 83/12/19";
#endif

#include "defs.h"

FILE	*lfp;		/* log file for recording files updated */
struct	block *special;	/* list of special commands */

/*
 * Process commands for sending files to other machines.
 */
dohcmds(files, hosts, cmds)
	struct block *files, *hosts, *cmds;
{
	register struct block *h, *f, *c;
	register char **cpp;
	int n, ddir;

	if (debug)
		printf("dohcmds(%x, %x, %x)\n", files, hosts, cmds);

	files = expand(files, E_VARS|E_SHELL);
	if (files == NULL) {
		error("no files to be updated\n");
		return;
	}
	hosts = expand(hosts, E_VARS|E_SHELL);
	if (hosts == NULL) {
		error("empty list of hosts to be updated\n");
		return;
	}
	if (!mkexceptlist(cmds))
		return;
	special = cmds;

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
				install(f->b_name, c->b_name,
					c->b_name == NULL ? 0 : ddir,
					c->b_options);
			}
			if (n == 0)
				install(f->b_name, NULL, 0, options);
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
	register char *ruser, *cp;
	int n;
	extern char user[];

	(void) sprintf(buf, "/usr/local/rdist -Server%s", qflag ? " -q" : "");

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
	cp = buf;
	if (read(rem, cp, 1) != 1)
		lostconn();
	if (*cp == 'V') {
		do {
			if (read(rem, cp, 1) != 1)
				lostconn();
		} while (*cp++ != '\n' && cp < &buf[BUFSIZ]);
		*--cp = '\0';
		cp = buf;
		n = 0;
		while (*cp >= '0' && *cp <= '9')
			n = (n * 10) + (*cp++ - '0');
		if (*cp == '\0' && n == VERSION)
			return(1);
	}
	error("connection failed: version numbers don't match\n");
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

time_t	lastmod;
FILE	*tfp;
extern	char target[], *tp;

/*
 * Process commands for comparing files to time stamp files.
 */
dofcmds(files, stamp, cmds)
	struct block *files, *stamp, *cmds;
{
	register struct block *b;
	register char **cpp;
	struct timeval tv[2];
	struct timezone tz;
	struct stat stb;

	if (debug)
		printf("dofcmds()\n");

	files = expand(files, E_ALL);
	if (files == NULL) {
		error("no files to be updated\n");
		return;
	}
	stamp = expand(stamp, E_ALL);
	if (stamp == NULL || stamp->b_next != NULL) {
		error("Only one time stamp file allowed\n");
		return;
	}
	if (!mkexceptlist(cmds))
		return;

	if (stat(stamp->b_name, &stb) < 0) {
		error("%s: %s\n", stamp->b_name, sys_errlist[errno]);
		return;
	}
	if (debug)
		printf("%s: %d\n", stamp->b_name, stb.st_mtime);
	if (!nflag) {
		lastmod = stb.st_mtime;
		(void) gettimeofday(&tv[0], &tz);
		tv[1] = tv[0];
		(void) utimes(stamp->b_name, tv);
		if (options & VERIFY)
			tfp = NULL;
		else if ((tfp = fopen(tmpfile, "w")) == NULL) {
			error("%s: %s\n", stamp->b_name, sys_errlist[errno]);
			return;
		}
	} else
		tfp = NULL;

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

	if (tfp != NULL)
		(void) fclose(tfp);
	for (b = cmds; b != NULL; b = b->b_next)
		if (b->b_type == NOTIFY)
			notify(tmpfile, NULL, b->b_args, lastmod);
	if (!nflag && !(options & VERIFY))
		(void) unlink(tmpfile);
}

/*
 * Compare the mtime of file to the list of time stamps.
 */
cmptime(name)
	char *name;
{
	struct stat stb;

	if (debug)
		printf("cmptime(%s)\n", name);

	if (inlist(except, name))
		return;

	if (nflag) {
		printf("comparing dates: %s\n", name);
		return;
	}

	/*
	 * first time cmptime() is called?
	 */
	if (tp == NULL) {
		if (exptilde(target, name) == NULL)
			return;
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

	if (stb.st_mtime > lastmod)
		log(tfp, "new: %s\n", name);
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

	if ((options & VERIFY) || to == NULL)
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
 * Return true if name is in the list.
 */
inlist(list, file)
	struct block *list;
	char *file;
{
	register struct block *c;

	for (c = list; c != NULL; c = c->b_next)
		if (!strcmp(file, c->b_name))
			return(1);
	return(0);
}

/*
 * Build the exception list from an unexpanded list of commands.
 */
mkexceptlist(cmds)
	struct block *cmds;
{
	register struct block *f, *a, *c;

	if (debug)
		printf("mkexceptlist()\n");

	except = f = NULL;
	for (c = cmds; c != NULL; c = c->b_next) {
		if (c->b_type != EXCEPT)
			continue;
		for (a = c->b_args; a != NULL; a = a->b_next) {
			if (f == NULL)
				except = f = expand(makeblock(NAME, a->b_name), E_ALL);
			else
				f->b_next = expand(makeblock(NAME, a->b_name), E_ALL);
			while (f->b_next != NULL)
				f = f->b_next;
		}
	}
	if (debug)
		prnames(except);
	return(1);
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
