/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ruserpass.c	5.1 (Berkeley) 3/1/89";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <errno.h>

char	*malloc(), *index(), *getenv(), *getpass(), *getlogin();
char	*strcpy();
static	FILE *cfile;

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64
#endif

#define	DEFAULT	1
#define	LOGIN	2
#define	PASSWD	3
#define	ACCOUNT 4
#define MACDEF  5
#define	ID	10
#define	MACH	11

static int token();
static char tokval[100];

static struct toktab {
	char *tokstr;
	int tval;
} toktab[]= {
	"default",	DEFAULT,
	"login",	LOGIN,
	"password",	PASSWD,
	"passwd",	PASSWD,
	"account",	ACCOUNT,
	"machine",	MACH,
	"macdef",	MACDEF,
	0,		0
};

ruserpass(host, aname, apass)
     char *host, **aname, **apass;
{
	char *hdir, buf[BUFSIZ], *tmp;
	char myname[MAXHOSTNAMELEN], *mydomain;
	int t, i, c, usedefault = 0;
	struct stat stb;
	extern int errno;

	hdir = getenv("HOME");
	if (hdir == NULL)
		hdir = ".";
	(void) sprintf(buf, "%s/.netrc", hdir);
	cfile = fopen(buf, "r");
	if (cfile == NULL) {
		if (errno != ENOENT)
			perror(buf);
		goto done;
	}

	while ((t = token())) switch(t) {
	case DEFAULT:
		usedefault = 1;
		/* FALL THROUGH */

	case MACH:
		if (!usedefault) {
			if (token() != ID)
				continue;
			/*
			 * Allow match either for user's host name.
			 */
			if (strcasecmp(host, tokval) == 0)
				goto match;
			continue;
		}
	match:
		while ((t = token()) && t != MACH && t != DEFAULT) switch(t) {

		case LOGIN:
			if (token() && *aname == 0) {
				*aname = malloc((unsigned) strlen(tokval) + 1);
				(void) strcpy(*aname, tokval);
			}
			break;
		case PASSWD:
			if (fstat(fileno(cfile), &stb) >= 0 &&
			    (stb.st_mode & 077) != 0) {
	fprintf(stderr, "Error - .netrc file not correct mode.\n");
	fprintf(stderr, "Remove password or correct mode.\n");
				goto bad;
			}
			if (token() && *apass == 0) {
				*apass = malloc((unsigned) strlen(tokval) + 1);
				(void) strcpy(*apass, tokval);
			}
			break;
		case ACCOUNT:
			break;

		case MACDEF:
			goto done_close;
			break;
		default:
	fprintf(stderr, "Unknown .netrc keyword %s\n", tokval);
			break;
		}
		goto done;
	}

done_close:
	(void) fclose(cfile);

done:
	if (!*aname) {
		char tmp[80];
		char *myname;

		if ((myname = getlogin()) == NULL) {
			struct passwd *pp;

			if ((pp = getpwuid (getuid())) != NULL)
				myname = pp->pw_name;
		}
		printf("Name (%s:%s): ", host, myname);

		(void) fgets(tmp, sizeof(tmp) - 1, stdin);
		tmp[strlen(tmp) - 1] = '\0';
		if (*tmp != '\0') {
			myname = tmp;
		}

		*aname = malloc((unsigned) strlen(myname) + 1);
		strcpy (*aname, myname);
	}

	if (!*apass) {
		char prompt[256];
		char *mypass;

		sprintf(prompt, "Password (%s:%s): ", host, *aname);
		mypass = getpass (prompt);
	
		if (*mypass == '\0') {
			mypass = *aname;
		}

		*apass = malloc((unsigned) strlen(mypass) + 1);
		strcpy (*apass, mypass);
	}

	return(0);
bad:
	(void) fclose(cfile);
	return(-1);
}

static int
token()
{
	char *cp;
	int c;
	struct toktab *t;

	if (feof(cfile))
		return (0);
	while ((c = getc(cfile)) != EOF &&
	    (c == '\n' || c == '\t' || c == ' ' || c == ','))
		continue;
	if (c == EOF)
		return (0);
	cp = tokval;
	if (c == '"') {
		while ((c = getc(cfile)) != EOF && c != '"') {
			if (c == '\\')
				c = getc(cfile);
			*cp++ = c;
		}
	} else {
		*cp++ = c;
		while ((c = getc(cfile)) != EOF
		    && c != '\n' && c != '\t' && c != ' ' && c != ',') {
			if (c == '\\')
				c = getc(cfile);
			*cp++ = c;
		}
	}
	*cp = 0;
	if (tokval[0] == 0)
		return (0);
	for (t = toktab; t->tokstr; t++)
		if (!strcmp(t->tokstr, tokval))
			return (t->tval);
	return (ID);
}
