/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)field.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <pwd.h>
#include <grp.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "chpass.h"
#include "pathnames.h"

/* ARGSUSED */
p_login(p, pw, ep)
	char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	if (!*p) {
		(void)fprintf(stderr, "chpass: empty login field.\n");
		return(1);
	}
	if (*p == '-') {
		(void)fprintf(stderr,
		    "chpass: login names may not begin with a hyphen.\n");
		return(1);
	}
	if (!(pw->pw_name = strdup(p))) {
		(void)fprintf(stderr, "chpass: can't save entry.\n");
		return(1);
	}
	if (index(p, '.'))
		(void)fprintf(stderr,
		    "chpass: \'.\' is dangerous in a login name.\n");
	for (; *p; ++p)
		if (isupper(*p)) {
			(void)fprintf(stderr,
			    "chpass: upper-case letters are dangerous in a login name.\n");
			break;
		}
	return(0);
}

/* ARGSUSED */
p_passwd(p, pw, ep)
	char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	if (!*p)
		pw->pw_passwd = "";	/* "NOLOGIN"; */
	else if (!(pw->pw_passwd = strdup(p))) {
		(void)fprintf(stderr, "chpass: can't save password entry.\n");
		return(1);
	}
	
	return(0);
}

/* ARGSUSED */
p_uid(p, pw, ep)
	register char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	int id;

	if (!*p) {
		(void)fprintf(stderr, "chpass: empty uid field.\n");
		return(1);
	}
	if (!isdigit(*p)) {
		(void)fprintf(stderr, "chpass: illegal uid.\n");
		return(1);
	}
	id = atoi(p);
	if ((u_int)id > USHRT_MAX) {
		(void)fprintf(stderr, "chpass: %d > max uid value (%d).\n",
		    id, USHRT_MAX);
		return(1);
	}
	pw->pw_uid = id;
	return(0);
}

/* ARGSUSED */
p_gid(p, pw, ep)
	register char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	struct group *gr;
	int id;

	if (!*p) {
		(void)fprintf(stderr, "chpass: empty gid field.\n");
		return(1);
	}
	if (!isdigit(*p)) {
		if (!(gr = getgrnam(p))) {
			(void)fprintf(stderr,
			    "chpass: unknown group %s.\n", p);
			return(1);
		}
		pw->pw_gid = gr->gr_gid;
		return(0);
	}
	id = atoi(p);
	if ((u_int)id > USHRT_MAX) {
		(void)fprintf(stderr, "chpass: %d > max gid value (%d).\n",
		    id, USHRT_MAX);
		return(1);
	}
	pw->pw_gid = id;
	return(0);
}

/* ARGSUSED */
p_class(p, pw, ep)
	char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	if (!*p)
		pw->pw_class = "";
	else if (!(pw->pw_class = strdup(p))) {
		(void)fprintf(stderr, "chpass: can't save entry.\n");
		return(1);
	}
	
	return(0);
}

/* ARGSUSED */
p_change(p, pw, ep)
	char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	if (!atot(p, &pw->pw_change))
		return(0);
	(void)fprintf(stderr, "chpass: illegal date for change field.\n");
	return(1);
}

/* ARGSUSED */
p_expire(p, pw, ep)
	char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	if (!atot(p, &pw->pw_expire))
		return(0);
	(void)fprintf(stderr, "chpass: illegal date for expire field.\n");
	return(1);
}

/* ARGSUSED */
p_gecos(p, pw, ep)
	char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	if (!*p)
		ep->save = "";
	else if (!(ep->save = strdup(p))) {
		(void)fprintf(stderr, "chpass: can't save entry.\n");
		return(1);
	}
	return(0);
}

/* ARGSUSED */
p_hdir(p, pw, ep)
	char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	if (!*p) {
		(void)fprintf(stderr, "chpass: empty home directory field.\n");
		return(1);
	}
	if (!(pw->pw_dir = strdup(p))) {
		(void)fprintf(stderr, "chpass: can't save entry.\n");
		return(1);
	}
	return(0);
}

/* ARGSUSED */
p_shell(p, pw, ep)
	register char *p;
	struct passwd *pw;
	ENTRY *ep;
{
	char *t, *ok_shell();

	if (!*p) {
		pw->pw_shell = _PATH_BSHELL;
		return(0);
	}
	/* only admin can change from or to "restricted" shells */
	if (uid && pw->pw_shell && !ok_shell(pw->pw_shell)) {
		(void)fprintf(stderr,
		    "chpass: %s: current shell non-standard.\n", pw->pw_shell);
		return(1);
	}
	if (!(t = ok_shell(p))) {
		if (uid) {
			(void)fprintf(stderr,
			    "chpass: %s: non-standard shell.\n", p);
			return(1);
		}
	}
	else
		p = t;
	if (!(pw->pw_shell = strdup(p))) {
		(void)fprintf(stderr, "chpass: can't save entry.\n");
		return(1);
	}
	return(0);
}
