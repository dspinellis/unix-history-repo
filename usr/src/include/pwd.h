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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)pwd.h	4.3 (Berkeley) %G%
 */

#define	_PATH_PASSWD		"/etc/passwd"
#define	_PATH_MASTERPASSWD	"/etc/master.passwd"
#define	_PATH_MKPASSWD		"/usr/sbin/mkpasswd"
#define	_PATH_PTMP		"/etc/ptmp"

#define	_PW_KEYBYNAME		'0'
#define	_PW_KEYBYUID		'1'

struct passwd {
	char	*pw_name;			/* user name */
	char	*pw_passwd;			/* encrypted password */
	int	pw_uid;				/* user uid */
	int	pw_gid;				/* user gid */
	time_t	pw_change;			/* password change time */
	char	*pw_class;			/* user access class */
	char	*pw_gecos;			/* Honeywell login info */
	char	*pw_dir;			/* home directory */
	char	*pw_shell;			/* default shell */
	time_t	pw_expire;			/* account expiration */
};

struct passwd *getpwent(), *getpwuid(), *getpwnam();
void endpwent(), setpwfile();
int setpassent(), setpwent();
