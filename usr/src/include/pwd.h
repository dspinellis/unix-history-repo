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
 *	@(#)pwd.h	5.5 (Berkeley) 5/29/90
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
