/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)table.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stddef.h>
#include "chpass.h"

char e1[] = ": ";
char e2[] = ":,";

int p_change(), p_class(), p_expire(), p_gecos(), p_gid(), p_hdir();
int p_login(), p_passwd(), p_shell(), p_uid();

ENTRY list[] = {
	{ "login",		p_login,  1,   5, e1,   },
	{ "password",		p_passwd, 1,   8, e1,   },
	{ "uid",		p_uid,    1,   3, e1,   },
	{ "gid",		p_gid,    1,   3, e1,   },
	{ "class",		p_class,  1,   5, e1,   },
	{ "change",		p_change, 1,   6, NULL, },
	{ "expire",		p_expire, 1,   6, NULL, },
	{ "full name",		p_gecos,  0,   9, e2,   },
	{ "office phone",	p_gecos,  0,  12, e2,   },
	{ "home phone",		p_gecos,  0,  10, e2,   },
	{ "location",		p_gecos,  0,   8, e2,   },
	{ "home directory",	p_hdir,   1,  14, e1,   },
	{ "shell",		p_shell,  0,   5, e1,   },
	{ NULL, 0, },
};
