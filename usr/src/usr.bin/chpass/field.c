/*
 * Copyright (c) 1988 The Regents of the University of California.
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
static char sccsid[] = "@(#)field.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include </usr/src/include/pwd.h>
#include <grp.h>
#include <strings.h>
#include <stdio.h>
#include <ctype.h>
#include <chpass.h>

/* ARGSUSED */
p_login(p, pw, ep)
	char *p;
	struct passwd *pw;
	struct entry *ep;
{
	if (!*p) {
		fprintf(stderr, "chpass: empty field");
		return(1);
	}
	if (!(pw->pw_name = strdup(p))) {
		fprintf(stderr, "chpass: can't save entry");
		return(1);
	}
	return(0);
}

/* ARGSUSED */
p_uid(p, pw, ep)
	register char *p;
	struct passwd *pw;
	struct entry *ep;
{
	int id;

	if (!*p) {
		fprintf(stderr, "chpass: empty field");
		return(1);
	}
	if (!isdigit(*p)) {
		fprintf(stderr, "chpass: illegal uid");
		return(1);
	}
	id = atoi(p);
	if ((u_int)id > USHRT_MAX) {
		fprintf(stderr, "chpass: %d > max uid value (%d)",
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
	struct entry *ep;
{
	struct group *gr;
	int id;

	if (!*p) {
		fprintf(stderr, "chpass: empty field");
		return(1);
	}
	if (!isdigit(*p)) {
		if (!(gr = getgrnam(p))) {
			fprintf(stderr, "chpass: unknown group %s", p);
			return(1);
		}
		pw->pw_gid = gr->gr_gid;
		return(0);
	}
	id = atoi(p);
	if ((u_int)id > USHRT_MAX) {
		fprintf(stderr, "chpass: %d > max gid value (%d)",
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
	struct entry *ep;
{
	if (!*p) {
		fprintf(stderr, "chpass: empty field");
		return(0);
	}
	if (!(pw->pw_class = strdup(p))) {
		fprintf(stderr, "chpass: can't save entry");
		return(1);
	}
	return(0);
}

/* ARGSUSED */
p_change(p, pw, ep)
	char *p;
	struct passwd *pw;
	struct entry *ep;
{
	if (!atot(p, &pw->pw_change))
		return(0);
	fprintf(stderr, "chpass: illegal date for change field");
	return(1);
}

/* ARGSUSED */
p_expire(p, pw, ep)
	char *p;
	struct passwd *pw;
	struct entry *ep;
{
	if (!atot(p, &pw->pw_expire))
		return(0);
	fprintf(stderr, "chpass: illegal date for expire field");
	return(1);
}

/* ARGSUSED */
p_save(p, pw, ep)
	char *p;
	struct passwd *pw;
	struct entry *ep;
{
	if (!*p) {
		fprintf(stderr, "chpass: empty field.");
		return(1);
	}
	if (!(ep->save = strdup(p))) {
		fprintf(stderr, "chpass: can't save entry");
		return(1);
	}
	return(0);
}

/* ARGSUSED */
p_hdir(p, pw, ep)
	char *p;
	struct passwd *pw;
	struct entry *ep;
{
	if (!*p) {
		fprintf(stderr, "chpass: empty field");
		return(1);
	}
	if (!(pw->pw_dir = strdup(p))) {
		fprintf(stderr, "chpass: can't save entry");
		return(1);
	}
	return(0);
}

/* ARGSUSED */
p_shell(p, pw, ep)
	register char *p;
	struct passwd *pw;
	struct entry *ep;
{
	register char *sh, *t;
	char *getusershell();

	if (!*p) {
		pw->pw_shell = "/bin/sh";
		return(0);
	}
	setusershell();
	for (;;) {
		if (!(sh = getusershell())) {
			/* only admin can set "restricted" shells */
			if (!uid)
				break;
			fprintf(stderr, "chpass: %s: non-standard shell", p);
			return(1);
		}
		if (!strcmp(p, sh))
			break;
		/* allow just shell name */
		if ((t = rindex(sh, '/')) && !strcmp(p, t)) {
			p = t;
			break;
		}
	}
	if (!(pw->pw_shell = strdup(p))) {
		fprintf(stderr, "chpass: can't save entry");
		return(1);
	}
	return(0);
}
