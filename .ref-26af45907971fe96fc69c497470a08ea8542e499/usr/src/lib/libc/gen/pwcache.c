/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)pwcache.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <grp.h>
#include <pwd.h>
#include <stdio.h>
#include <utmp.h>

#define	NCACHE	64			/* power of 2 */
#define	MASK	NCACHE - 1		/* bits to store with */

char *
user_from_uid(uid, nouser)
	uid_t uid;
	int nouser;
{
	static struct ncache {
		uid_t	uid;
		char	name[UT_NAMESIZE + 1];
	} c_uid[NCACHE];
	static int pwopen;
	static char nbuf[15];		/* 32 bits == 10 digits */
	register struct passwd *pw;
	register struct ncache *cp;

	cp = c_uid + (uid & MASK);
	if (cp->uid != uid || !*cp->name) {
		if (pwopen == 0) {
			setpassent(1);
			pwopen = 1;
		}
		if ((pw = getpwuid(uid)) == NULL) {
			if (nouser)
				return (NULL);
			(void)snprintf(nbuf, sizeof(nbuf), "%u", uid);
			return (nbuf);
		}
		cp->uid = uid;
		(void)strncpy(cp->name, pw->pw_name, UT_NAMESIZE);
		cp->name[UT_NAMESIZE] = '\0';
	}
	return (cp->name);
}

char *
group_from_gid(gid, nogroup)
	gid_t gid;
	int nogroup;
{
	static struct ncache {
		gid_t	gid;
		char	name[UT_NAMESIZE + 1];
	} c_gid[NCACHE];
	static int gropen;
	static char nbuf[15];		/* 32 bits == 10 digits */
	struct group *gr;
	struct ncache *cp;

	cp = c_gid + (gid & MASK);
	if (cp->gid != gid || !*cp->name) {
		if (gropen == 0) {
			setgroupent(1);
			gropen = 1;
		}
		if ((gr = getgrgid(gid)) == NULL) {
			if (nogroup)
				return (NULL);
			(void)snprintf(nbuf, sizeof(nbuf), "%u", gid);
			return (nbuf);
		}
		cp->gid = gid;
		(void)strncpy(cp->name, gr->gr_name, UT_NAMESIZE);
		cp->name[UT_NAMESIZE] = '\0';
	}
	return (cp->name);
}
