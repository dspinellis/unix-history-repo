/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)portald.h	8.1 (Berkeley) %G%
 *
 * $Id: portald.h,v 1.1 1992/05/25 21:43:09 jsp Exp jsp $
 */

#include <sys/cdefs.h>
#include <miscfs/portal/portal.h>

/*
 * Meta-chars in an RE.  Paths in the config file containing
 * any of these characters will be matched using regexec, other
 * paths will be prefix-matched.
 */
#define RE_CHARS ".|()[]*+?\\^$"

typedef struct qelem qelem;

struct qelem {
	qelem *q_forw;
	qelem *q_back;
};

typedef struct provider provider;
struct provider {
	char *pr_match;
	int (*pr_func) __P((struct portal_cred *,
				char *key, char **v, int so, int *fdp));
};
extern provider providers[];

/*
 * Portal providers
 */
extern int portal_exec __P((struct portal_cred *,
				char *key, char **v, int so, int *fdp));
extern int portal_file __P((struct portal_cred *,
				char *key, char **v, int so, int *fdp));
extern int portal_tcp __P((struct portal_cred *,
				char *key, char **v, int so, int *fdp));

/*
 * Global functions
 */
extern void activate __P((qelem *q, int so));
extern char **conf_match __P((qelem *q, char *key));
extern void conf_read __P((qelem *q, char *conf));
