/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)pwd.h	5.5 (Berkeley) %G%
 */

#include <sys/types.h>

#ifndef _POSIX_SOURCE
#define	_PATH_PASSWD		"/etc/passwd"
#define	_PATH_MASTERPASSWD	"/etc/master.passwd"
#define	_PATH_MKPASSWD		"/usr/sbin/mkpasswd"
#define	_PATH_PTMP		"/etc/ptmp"

#define	_PW_KEYBYNAME		'0'
#define	_PW_KEYBYUID		'1'

#define	_PASSWORD_LEN		128
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

#if __STDC__ || c_plusplus
struct passwd *getpwuid(uid_t);
struct passwd *getpwnam(const char *);
#ifndef _POSIX_SOURCE
struct passwd *getpwent(void);
int setpwent(void);
void endpwent(void);
void setpwfile(const char *);
int setpassent(int);
#endif
#else
struct passwd *getpwuid();
struct passwd *getpwnam();
#ifndef _POSIX_SOURCE
struct passwd *getpwent();
int setpwent();
void endpwent();
void setpwfile();
int setpassent();
#endif
#endif
