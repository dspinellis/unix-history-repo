/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)grp.h	5.2 (Berkeley) %G%
 */

#ifndef _POSIX_SOURCE
#define	_PATH_GROUP		"/etc/group"
#endif

struct group {
	char	*gr_name;		/* group name */
	char	*gr_passwd;		/* group password */
	int	gr_gid;			/* group id */
	char	**gr_mem;		/* group members */
};

#if __STDC__ || c_plusplus
extern struct group *getgrgid(gid_t);
extern struct group *getgrnam(const char *);
#ifndef _POSIX_SOURCE
extern struct group *getgrent(void);
extern int setgrent(void);
extern void endgrent(void);
extern void setgrfile(const char *);
extern int setgroupent(int);
#endif
#else
extern struct group *getgrgid();
extern struct group *getgrnam();
#ifndef _POSIX_SOURCE
extern struct group *getgrent();
extern int setgrent();
extern void endgrent();
extern void setgrfile();
extern int setgroupent();
#endif
#endif
