#ifndef lint
static	char *sccsid = "@(#)docmd.c	4.1 (Berkeley) 83/09/07";
#endif

#include "defs.h"

FILE	*lfp;		/* log file for recording files updated */

/*
 * Routines to process commands.
 */
docmd(files, hosts, cmds)
	struct block *files, *hosts, *cmds;
{
	register struct block *h, *f, *c;
	register char *cp, **cpp;
	int n;

	if (debug)
		printf("docmd()\n");

	files = expand(files);
	hosts = expand(hosts);
	if (files == NULL)
		fatal("no files to be updated\n");
	if (hosts == NULL)
		fatal("empty list of hosts to be updated\n");
	except = cmds;

	for (h = hosts; h != NULL; h = h->b_next) {
		if (!nflag) {
			if ((lfp = fopen(tmpfile, "w")) == NULL) {
				fatal("cannot open %s\n", tmpfile);
				exit(1);
			}
			if (!makeconn(h->b_name))
				continue;
		}
		for (f = files; f != NULL; f = f->b_next) {
			if (filec) {
				for (cpp = filev; *cpp; cpp++)
					if (!strcmp(f->b_name, *cpp))
						goto found;
				continue;
			}
		found:
			n = 0;
			for (c = cmds; c != NULL; c = c->b_next)
				if (c->b_type == INSTALL) {
					install(f->b_name, c->b_name, 0);
					n++;
				} else if (c->b_type == VERIFY) {
					install(f->b_name, c->b_name, 1);
					n++;
				}
			if (n == 0)
				install(f->b_name, f->b_name, 0);
		}
		if (!nflag) {
			(void) fclose(lfp);
			(void) close(rem);
		}
		for (c = cmds; c != NULL; c = c->b_next)
			if (c->b_type == NOTIFY)
				notify(h->b_name, c->b_args);
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
		yflag ? " -y" : "", debug ? " -d" : "");

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

	rem = rcmd(&rhost, IPPORT_CMDSERVER, user, ruser, buf, 0);
	if (rem < 0)
		return(0);
	if (response() < 0)
		return(0);
	return(1);
}

/*
 * Update the file(s) if they are different.
 */
install(src, dest, verify)
	char *src, *dest;
	int verify;
{
	register char *cp;
	extern char *tp;
	char lbuf[BUFSIZ];

	if (!qflag)
		printf("%s %s %s\n", verify ? "verify" : "install", src, dest);
	if (nflag)
		return;
	/*
	 * Pass the destination file/directory name to remote.
	 */
	(void) sprintf(buf, "T%s\n", dest);
	if (debug)
		printf("buf = %s", buf);
	(void) write(rem, buf, strlen(buf));
	tp = NULL;
	shexpand(lbuf, src);
	sendf(lbuf, verify);
}

/*
 * Notify the list of people the changes that were made.
 */
notify(host, to)
	char *host;
	register struct block *to;
{
	register int fd, len;
	FILE *pf, *popen();
	struct stat stb;

	if (vflag)
		return;
	if (!qflag) {
		printf("notify @%s ", host);
		prnames(to);
	}
	if (nflag)
		return;

	if ((fd = open(tmpfile, 0)) < 0) {
		error("%s: %s\n", tmpfile, sys_errlist[errno]);
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
		fprintf(pf, " %s@%s", to->b_name, host);
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
