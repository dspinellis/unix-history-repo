/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 *
 *	@(#)pathnames.h	5.1 (Berkeley) %G%
 */

#include <paths.h>

#define	_PATH_ACULOG		"/var/log/aculog"
#define	_PATH_LOCKDIRNAME	"/var/spool/uucp/LCK..%s"
#ifdef notdef
#define	_PATH_LOCKDIRNAME	"/var/spool/uucp/LCK/LCK..%s"
#endif
#define	_PATH_PHONES		"/etc/phones"
#define	_PATH_REMOTE		"/etc/remote"
