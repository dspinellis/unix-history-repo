/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getnetgrent.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <strings.h>

#define _PATH_NETGROUP "/etc/netgroup"

/*
 * Static Variables and functions used by setnetgrent(), getnetgrent() and
 * endnetgrent().
 * There are two linked lists:
 * - linelist is just used by setnetgrent() to parse the net group file via.
 *   parse_netgrp()
 * - netgrp is the list of entries for the current netgroup
 */
struct linelist {
	struct linelist	*l_next;	/* Chain ptr. */
	int		l_parsed;	/* Flag for cycles */
	char		*l_groupname;	/* Name of netgroup */
	char		*l_line;	/* Netgroup entrie(s) to be parsed */
};

struct netgrp {
	struct netgrp	*ng_next;	/* Chain ptr */
	char		*ng_host;	/* Host name */
	char		*ng_user;	/* User name */
	char		*ng_dom;	/* and Domain name */
};

static struct linelist	*linehead = (struct linelist *)0;
static struct netgrp	*nextgrp = (struct netgrp *)0;
static struct {
	struct netgrp	*gr;
	char		*grname;
} grouphead = {
	(struct netgrp *)0,
	(char *)0,
};
static FILE *netf = (FILE *)0;
static int parse_netgrp();
static struct linelist *read_for_group();
void setnetgrent(), endnetgrent();
int getnetgrent(), innetgr();

#define	LINSIZ	1024	/* Length of netgroup file line */

/*
 * setnetgrent()
 * Parse the netgroup file looking for the netgroup and build the list
 * of netgrp structures. Let parse_netgrp() and read_for_group() do
 * most of the work.
 */
void
setnetgrent(group)
	char *group;
{

	if (grouphead.gr == (struct netgrp *)0 ||
		strcmp(group, grouphead.grname)) {
		endnetgrent();
		if (netf = fopen(_PATH_NETGROUP, "r")) {
			if (parse_netgrp(group))
				endnetgrent();
			else {
				grouphead.grname = (char *)
					malloc(strlen(group) + 1);
				strcpy(grouphead.grname, group);
			}
			fclose(netf);
		}
	}
	nextgrp = grouphead.gr;
}

/*
 * Get the next netgroup off the list.
 */
int
getnetgrent(hostp, userp, domp)
	char **hostp, **userp, **domp;
{

	if (nextgrp) {
		*hostp = nextgrp->ng_host;
		*userp = nextgrp->ng_user;
		*domp = nextgrp->ng_dom;
		nextgrp = nextgrp->ng_next;
		return (1);
	}
	return (0);
}

/*
 * endnetgrent() - cleanup
 */
void
endnetgrent()
{
	register struct linelist *lp, *olp;
	register struct netgrp *gp, *ogp;

	lp = linehead;
	while (lp) {
		olp = lp;
		lp = lp->l_next;
		free(olp->l_groupname);
		free(olp->l_line);
		free((char *)olp);
	}
	linehead = (struct linelist *)0;
	if (grouphead.grname) {
		free(grouphead.grname);
		grouphead.grname = (char *)0;
	}
	gp = grouphead.gr;
	while (gp) {
		ogp = gp;
		gp = gp->ng_next;
		if (ogp->ng_host)
			free(ogp->ng_host);
		if (ogp->ng_user)
			free(ogp->ng_user);
		if (ogp->ng_dom)
			free(ogp->ng_dom);
		free((char *)ogp);
	}
	grouphead.gr = (struct netgrp *)0;
}

/*
 * Search for a match in a netgroup.
 */
int
innetgr(group, host, user, dom)
	char *group, *host, *user, *dom;
{
	char *hst, *usr, *dm;

	setnetgrent(group);
	while (getnetgrent(&hst, &usr, &dm))
		if ((host == (char *)0 || !strcmp(host, hst)) &&
		    (user == (char *)0 || !strcmp(user, usr)) &&
		    (dom == (char *)0 || !strcmp(dom, dm))) {
			endnetgrent();
			return (1);
		}
	endnetgrent();
	return (0);
}

/*
 * Parse the netgroup file setting up the linked lists.
 */
static int
parse_netgrp(group)
	char *group;
{
	register char *pos, *spos;
	register int len;
	struct netgrp *grp;
	struct linelist *lp = linehead;

	/*
	 * First, see if the line has already been read in.
	 */
	while (lp) {
		if (!strcmp(group, lp->l_groupname))
			break;
		lp = lp->l_next;
	}
	if (lp == (struct linelist *)0 &&
	    (lp = read_for_group(group)) == (struct linelist *)0)
		return (1);
	if (lp->l_parsed) {
		fprintf(stderr, "Cycle in netgroup %s\n", lp->l_groupname);
		return (1);
	} else
		lp->l_parsed = 1;
	pos = lp->l_line;
	while (*pos != '\0') {
		if (*pos == '(') {
			grp = (struct netgrp *)malloc(sizeof (struct netgrp));
			bzero((char *)grp, sizeof (struct netgrp));
			grp->ng_next = grouphead.gr;
			grouphead.gr = grp;
			spos = ++pos;
			while (*pos != ',' && *pos != '\0')
				pos++;
			if (*pos == '\0')
				goto errout;
			len = pos - spos;
			if (len > 0) {
				grp->ng_host = (char *)malloc(len + 1);
				bcopy(spos, grp->ng_host, len);
				*(grp->ng_host + len) = '\0';
			}
			spos = ++pos;
			while (*pos != ',' && *pos != '\0')
				pos++;
			if (*pos == '\0')
				goto errout;
			len = pos - spos;
			if (len > 0) {
				grp->ng_user = (char *)malloc(len + 1);
				bcopy(spos, grp->ng_user, len);
				*(grp->ng_user + len) = '\0';
			}
			spos = ++pos;
			while (*pos != ')' && *pos != '\0')
				pos++;
			if (*pos == '\0')
				goto errout;
			len = pos - spos;
			if (len > 0) {
				grp->ng_dom = (char *)malloc(len + 1);
				bcopy(spos, grp->ng_dom, len);
				*(grp->ng_dom + len) = '\0';
			}
			if (*++pos != '\0' &&
				(*pos != ',' || *++pos == ',' || *pos == '\0'))
				goto errout;
		} else {
			spos = pos;
			while (*pos != ',' && *pos != '\0')
				pos++;
			if (*pos == ',')
				*pos++ = '\0';
			if (parse_netgrp(spos))
				return (1);
		}
	}
	return (0);
errout:
	fprintf(stderr, "Bad netgroup %s at ..%s\n", lp->l_groupname,
		spos);
	return (1);
}

/*
 * Read the netgroup file and save lines until the line for the netgroup
 * is found. Return 1 if eof is encountered.
 */
static struct linelist *
read_for_group(group)
	char *group;
{
	register char *pos, *spos;
	register int len;
	struct linelist *lp;
	char line[LINSIZ + 1];

	while (fgets(line, LINSIZ, netf) != NULL) {
		pos = line;
		while (*pos == ' ' || *pos == '\t')
			pos++;
		spos = pos;
		while (*pos != ' ' && *pos != '\t' && *pos != '\n' &&
			*pos != '\0')
			pos++;
		len = pos - spos;
		while (*pos == ' ' || *pos == '\t')
			pos++;
		if (*pos != '\n' && *pos != '\0') {
			lp = (struct linelist *)malloc(sizeof (*lp));
			lp->l_parsed = 0;
			lp->l_groupname = (char *)malloc(len + 1);
			bcopy(spos, lp->l_groupname, len);
			*(lp->l_groupname + len) = '\0';
			spos = lp->l_line = (char *)malloc(LINSIZ);
			while (*pos != '\n' && *pos != '\0') {
				if (*pos != ' ' && *pos != '\t')
					*spos++ = *pos++;
				else
					pos++;
			}
			*spos = '\0';
			lp->l_next = linehead;
			linehead = lp;

			/*
			 * If this is the one we wanted, we are done.
			 */
			if (!strcmp(lp->l_groupname, group))
				return (lp);
		}
	}
	return ((struct linelist *)0);
}
