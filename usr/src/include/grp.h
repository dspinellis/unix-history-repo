/*-
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)grp.h	8.1 (Berkeley) %G%
 */

#ifndef _GRP_H_
#define	_GRP_H_

#ifndef _POSIX_SOURCE
#define	_PATH_GROUP		"/etc/group"
#endif

struct group {
	char	*gr_name;		/* group name */
	char	*gr_passwd;		/* group password */
	int	gr_gid;			/* group id */
	char	**gr_mem;		/* group members */
};

#include <sys/cdefs.h>

__BEGIN_DECLS
struct group *getgrgid __P((gid_t));
struct group *getgrnam __P((const char *));
#ifndef _POSIX_SOURCE
struct group *getgrent __P((void));
int setgrent __P((void));
void endgrent __P((void));
void setgrfile __P((const char *));
int setgroupent __P((int));
#endif
__END_DECLS

#endif /* !_GRP_H_ */
