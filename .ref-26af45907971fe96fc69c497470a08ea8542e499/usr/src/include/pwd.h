/*-
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pwd.h	8.1 (Berkeley) %G%
 */

#ifndef _PWD_H_
#define	_PWD_H_

#include <sys/types.h>

#ifndef _POSIX_SOURCE
#define	_PATH_PASSWD		"/etc/passwd"
#define	_PATH_MASTERPASSWD	"/etc/master.passwd"

#define	_PATH_MP_DB		"/etc/pwd.db"
#define	_PATH_SMP_DB		"/etc/spwd.db"

#define	_PATH_PWD_MKDB		"/usr/sbin/pwd_mkdb"

#define	_PW_KEYBYNAME		'1'	/* stored by name */
#define	_PW_KEYBYNUM		'2'	/* stored by entry in the "file" */
#define	_PW_KEYBYUID		'3'	/* stored by uid */

#define	_PASSWORD_EFMT1		'_'	/* extended encryption format */

#define	_PASSWORD_LEN		128	/* max length, not counting NULL */
#endif

struct passwd {
	char	*pw_name;		/* user name */
	char	*pw_passwd;		/* encrypted password */
	int	pw_uid;			/* user uid */
	int	pw_gid;			/* user gid */
	time_t	pw_change;		/* password change time */
	char	*pw_class;		/* user access class */
	char	*pw_gecos;		/* Honeywell login info */
	char	*pw_dir;		/* home directory */
	char	*pw_shell;		/* default shell */
	time_t	pw_expire;		/* account expiration */
};

#include <sys/cdefs.h>

__BEGIN_DECLS
struct passwd	*getpwuid __P((uid_t));
struct passwd	*getpwnam __P((const char *));
#ifndef _POSIX_SOURCE
struct passwd	*getpwent __P((void));
int		 setpassent __P((int));
int		 setpwent __P((void));
void		 endpwent __P((void));
#endif
__END_DECLS

#endif /* !_PWD_H_ */
