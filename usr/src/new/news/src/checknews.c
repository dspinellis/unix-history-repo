/*
 * This software is Copyright (c) 1986 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 * checknews - news checking program
 */

#ifdef SCCSID
static char	*SccsId = "@(#)checknews.c	2.30	10/15/87";
#endif /* SCCSID */

char *Progname = "checknews";		/* used by xerror */

#include "params.h"

char	bfr[LBUFLEN];			/* general-use scratch area	*/
char	optbuf[BUFLEN];			/* NEWSOPTS buffer		*/
int	y, e, n, q;
int	verbose;			/* For debugging.		*/
int	nflag;				/* for spec. newsgroup		*/
char	narggrp[BUFLEN];		/* spec newsgroup		*/
FILE	*rcfp, *actfp;
char	newsrc[BUFLEN],*rcline[LINES],rcbuf[LBUFLEN],*argvrc[LINES];
int	mode = 1;
extern int line;
#ifndef SHELL
char	*SHELL;
#endif

main(argc, argv)
int argc;
register char **argv;
{
	register char *ptr;	/* pointer to rest of buffer		*/
	char *user, *home;
	struct passwd *pw;
	struct group *gp;
	int sflag = 0, optflag = FALSE, space = FALSE;
	int i;

	y = 0;
	n = 0;
	e = 0;
	q = 0;
	nflag = 0;
	pathinit();
	if (--argc > 0) {
		for (argv++; **argv; ++*argv) {
			switch(**argv) {
			case 'y':
				y++;
				break;
			case 'q':
				q++;
				break;
			case 'v':
				verbose++;
				break;
			case 'n':
				n++;
				break;
			case 'N':
				nflag++;
				if (argc <= 1)
					xerror("No newsgroup specified with -N");
				strcpy(narggrp,argv[1]);
				strcat(narggrp,",");
				break;
			case 'e':
			case 'f':
				e++;
				break;
			}
		}
	}
	if (!n && !e && !y && !q)
		y++;
	if (nflag)
		argv++;

#ifndef V6
	if ((user = getenv("USER")) == NULL)
		user = getenv("LOGNAME");
	if ((home = getenv("HOME")) == NULL)
		home = getenv("LOGDIR");
	if (user == NULL || home == NULL)
		getuser();
	else {
		username = AllocCpy(user);
		userhome = AllocCpy(home);
	}
	if (ptr = getenv("NEWSOPTS"))
		strcpy(rcbuf, ptr);
	else
		*rcbuf = '\0';
	if (*rcbuf) {
		strcat(rcbuf, " \1");
		ptr = rcbuf;
		while (*++ptr)
			if (isspace(*ptr))
				*ptr = '\0';
		for (ptr = rcbuf;; ptr++) {
			if (!*ptr)
				continue;
			if (*ptr == '\1')
				break;
			if (++line > LINES)
				xerror("Too many options.");
			if ((rcline[line] = malloc(strlen(ptr) + 1)) == NULL)
				xerror("Not enough memory.");
			argvrc[line] = rcline[line];
			strcpy(rcline[line], ptr);
			while (*ptr)
				ptr++;
		}
	}
#else
	getuser();
#endif
#ifdef SERVER
	if (open_server() < 0)
		xerror("NNTP connection failed.");
#endif /* SERVER */
	ptr = getenv("NEWSRC");
	if (ptr == NULL)
		sprintf(newsrc, "%s/%s", userhome, NEWSRC);
	else
		strcpy(newsrc, ptr);
	if ((rcfp = fopen(newsrc, "r")) != NULL) {
		while (fgets(rcbuf, LBUFLEN, rcfp) != NULL) {
			if (!(space = isspace(*rcbuf)))
				optflag = FALSE;
			if (!strncmp(rcbuf, "options ", 8))
				optflag = TRUE;
			if (optflag) {
				strcat(rcbuf, "\1");
				if (space)
					ptr = rcbuf - 1;
				else
					ptr = &rcbuf[7];
				while (*++ptr)
					if (isspace(*ptr))
						*ptr = '\0';
				if (space)
					ptr = rcbuf;
				else
					ptr = &rcbuf[8];
				for (;; ptr++) {
					if (!*ptr)
						continue;
					if (*ptr == '\1')
						break;
					if (++line > LINES)
						xerror("Too many options.");
					if ((rcline[line] = malloc(strlen(ptr) + 1)) == NULL)
						xerror("Not enough memory.");
					argvrc[line] = rcline[line];
					strcpy(rcline[line], ptr);
					while (*ptr)
						ptr++;
				}
			}
		}
		fclose(rcfp);
	}
	header.nbuf[0] = 0;
	if (line != -1) {
#ifdef DEBUG
		for (i = 0; i <= line; i++)
			fprintf(stderr, "options:  %s\n", rcline[i]);
#endif
		process(line+2, argvrc);
		do {
#ifdef DEBUG
			fprintf(stderr, "Freeing %d\n", line);
#endif
			free(rcline[line]);
		} while (line--);
	}

	if (!*header.nbuf) {
		strcpy(header.nbuf, DFLTSUB);
		ngcat(header.nbuf);
	}
	strcat(header.nbuf, ADMSUB);
	ngcat(header.nbuf);
	if (*header.nbuf)
		lcase(header.nbuf);
	makehimask(header.nbuf, "junk");
	makehimask(header.nbuf, "control");
	makehimask(header.nbuf, "test");
	if (access(newsrc, 0)) {
		if (verbose > 1)
			printf("No newsrc\n");
		yep(argv);
	}
	if ((rcfp = fopen(newsrc, "r")) == NULL)
		xerror("Cannot open .newsrc file");
	while (fgets(rcbuf, LBUFLEN, rcfp) != NULL) {
		if (!nstrip(rcbuf))
			xerror(".newsrc line too long");
		if (++line >= LINES)
			xerror("Too many .newsrc lines");
		if ((rcline[line] = malloc(strlen(rcbuf)+1)) == NULL)
			xerror("Not enough memory");
		strcpy(rcline[line], rcbuf);
	}
#ifdef SERVER
	if ((actfp = open_active(ACTIVE, "r")) == NULL)
#else /* !SERVER */
	if ((actfp = fopen(ACTIVE, "r")) == NULL)
#endif /* !SERVER */
		xerror("Cannot open active newsgroups file");
#ifdef DEBUG
	fprintf(stderr, "header.nbuf = %s\n", header.nbuf);
#endif
	nchk(argv);
	exit(0);
}

nchk(argv)
char **argv;
{
	register int i;
	register char *ptr;
	long l;
	long narts;
	char saveptr;
	int isnews = 0;
	char aline[BUFLEN];

#ifdef DEBUG
	fprintf(stderr, "nchk()\n");
#endif
	while (fgets(aline, sizeof aline, actfp) != NULL) {
		sscanf(aline, "%s %ld", bfr, &narts);
#ifdef DEBUG
		fprintf(stderr, "bfr = '%s'\n", bfr);
#endif
		if (narts == 0)
			continue;
		ngcat(bfr);
		if (!ngmatch(bfr, nflag ? narggrp : header.nbuf))
			continue;
		ngdel(bfr);
		i = findrcline(bfr);
		if (i < 0) {
			if (verbose>1)
				printf("No newsrc line for newsgroup %s\n", bfr);
			strcpy(rcbuf, " 0");
		} else
			strcpy(rcbuf, rcline[i]);
		ptr = rcbuf;

		if (index(rcbuf, '!') != NULL)
			continue;
		if (index(rcbuf, ',') != NULL) {
			if (verbose > 1)
				printf("Comma in %s newsrc line\n", bfr);
			else {
				isnews++;
				continue;
			}
		}
		while (*ptr)
			ptr++;
		while (!isdigit(*--ptr) && *ptr != ':' && ptr >= rcbuf)
			;
		if (*ptr == ':')
			continue;
		if (ptr < rcbuf) {
			if (verbose > 1)
				printf("Ran off beginning of %s newsrc line.\n", bfr);
			yep(argv);
		}
		while (isdigit(*--ptr))
			;
		sscanf(++ptr, "%ld", &l);
		if (narts > l) {
			if (verbose) {
				printf("News: %s ...\n", bfr);
				if (verbose < 2)
					y = 0;
			}
			yep(argv);
		}
contin:;
	}
	if (isnews)
		yep(argv);
	if (n)
		printf("No news is good news.\n");
}

yep(argv)
char **argv;
{
	if (y) {
		if (verbose)
			printf("There is probably news");
		else
			printf("There is news");
		if (nflag) {
			narggrp[strlen(narggrp)-1] = '.';
			printf(" in %s\n",narggrp);
		}
		else
			printf(".\n");
	}
	if (e) {
#ifdef V6
		execv("/usr/bin/readnews", argv);
#else
		execvp("readnews", argv);
#endif
		perror("Cannot exec readnews.");
	}
	if (q)
		exit(1);
	else
		exit(0);
}

xerror(message, arg1, arg2)
char *message;
int arg1, arg2;
{
	char buffer[128];

	sprintf(buffer, message, arg1, arg2);
	fprintf(stderr, "checknews: %s.\n", buffer);
	exit(1);
}

/*
 * Append NGDELIM to string.
 */
ngcat(s)
register char *s;
{
	if (*s) {
		while (*s++);
		s -= 2;
		if (*s++ == NGDELIM)
			return;
	}
	*s++ = NGDELIM;
	*s = '\0';
}

/*
 * News group matching.
 *
 * nglist is a list of newsgroups.
 * sublist is a list of subscriptions.
 * sublist may have "meta newsgroups" in it.
 * All fields are NGDELIM separated,
 * and there is an NGDELIM at the end of each argument.
 *
 * Currently implemented glitches:
 * sublist uses 'all' like shell uses '*', and '.' like shell '/'.
 * If subscription X matches Y, it also matches Y.anything.
 */
ngmatch(nglist, sublist)
register char *nglist, *sublist;
{
	register char *n, *s;
	register int rc;

	rc = FALSE;
	for (n = nglist; *n != '\0' && rc == FALSE;) {
		for (s = sublist; *s != '\0';) {
			if (*s != NEGCHAR)
				rc |= ptrncmp(s, n);
			else
				rc &= ~ptrncmp(s+1, n);
			while (*s++ != NGDELIM);
		}
		while (*n++ != NGDELIM);
	}
	return(rc);
}

/*
 * Compare two newsgroups for equality.
 * The first one may be a "meta" newsgroup.
 */
ptrncmp(ng1, ng2)
register char *ng1, *ng2;
{
	while (*ng1 != NGDELIM) {
		if (ng1[0]=='a' && ng1[1]=='l' && ng1[2]=='l') {
			ng1 += 3;
			while (*ng2 != NGDELIM && *ng2 != '.')
				if (ptrncmp(ng1, ng2++))
					return(TRUE);
			return (ptrncmp(ng1, ng2));
		} else if (*ng1++ != *ng2++)
			return(FALSE);
	}
	return (*ng2 == '.' || *ng2 == NGDELIM);
}

/*
 * Get user name and home directory.
 */
getuser()
{
	static int flag = TRUE;
	register struct passwd *p;

	if (flag) {
		if ((p = getpwuid(getuid())) == NULL)
			xerror("Cannot get user's name");
		if (username == NULL || *username == '\0')
			username = AllocCpy(p->pw_name);
		userhome = AllocCpy(p->pw_dir);
		flag = FALSE;
	}
}

/*
 * Strip trailing newlines, blanks, and tabs from 's'.
 * Return TRUE if newline was found, else FALSE.
 */
nstrip(s)
register char *s;
{
	register char *p;
	register int rc;

	rc = FALSE;
	p = s;
	while (*p)
		if (*p++ == '\n')
			rc = TRUE;
	while (--p >= s && (*p == '\n' || *p == ' ' || *p == '\t'));
	*++p = '\0';
	return(rc);
}

/*
 * Delete trailing NGDELIM.
 */
ngdel(s)
register char *s;
{
	if (*s++) {
		while (*s++);
		s -= 2;
		if (*s == NGDELIM)
			*s = '\0';
	}
}

lcase(s)
register char *s;
{
	register char *ptr;

	for (ptr = s; *ptr; ptr++)
		if (isupper(*ptr))
			*ptr = tolower(*ptr);
}

/*
 * finds the line in your .newsrc file (actually the in-core "rcline"
 * copy of it) and returns the index into the array where it was found.
 * -1 means it didn't find it.
 *
 * We play clever games here to make this faster.  It's inherently
 * quadratic - we spend lots of CPU time here because we search through
 * the whole .newsrc for each line.  The "prev" variable remembers where
 * the last match was found; we start the search there and loop around
 * to the beginning, in the hopes that the calls will be roughly in order.
 */
int
findrcline(name)
char *name;
{
	register char *p, *ptr;
	register int cur;
	register int i;
	register int top;
	static int prev = 0;

	top = line; i = prev;
loop:
	for (; i <= top; i++) {
		for (p = name, ptr = rcline[i]; (cur = *p++); ) {
			if (cur != *ptr++)
				goto contin2;
		}
		if (*ptr != ':' && *ptr != '!')
			continue;
		prev = i;
		return i;
contin2:
		;
	}
	if (i > line && line > prev-1) {
		i = 0;
		top = prev-1;
		goto loop;
	}
	return -1;
}

/*
 * Forbid newsgroup ng, unless he asked for it in nbuf.
 */
makehimask(nbuf, ng)
char *nbuf, *ng;
{
	if (!findex(nbuf, ng)) {
		ngcat(nbuf);
		strcat(nbuf, "!");
		strcat(nbuf, ng);
		ngcat(nbuf);
	}
}

/*
 * Return true if the string searchfor is in string, but not if preceded by !.
 */
findex(string, searchfor)
char *string, *searchfor;
{
	register char first;
	register char *p;

	first = *searchfor;
	for (p=index(string, first); p; p = index(p+1, first)) {
		if (p>string && p[-1] != '!' && strncmp(p, searchfor, strlen(searchfor)) == 0)
			return TRUE;
	}
	return FALSE;
}

xxit(i)
{
#ifdef SERVER
	close_server();
#endif /* SERVER */
	exit(i);
}
