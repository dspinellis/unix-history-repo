/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)grp.h	5.2 (Berkeley) 5/29/90
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
