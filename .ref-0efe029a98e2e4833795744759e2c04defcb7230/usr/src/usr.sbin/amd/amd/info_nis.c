/*
 * $Id: info_nis.c,v 5.2 90/06/23 22:19:32 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)info_nis.c	5.1 (Berkeley) %G%
 */

/*
 * Get info from NIS map
 */

#include "am.h"

#ifdef HAS_NIS_MAPS
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>

/*
 * Figure out the nis domain name
 */
static int determine_nis_domain(P_void)
{
static	 char default_domain[YPMAXDOMAIN];

	if (getdomainname(default_domain, sizeof(default_domain)) < 0) {
		plog(XLOG_ERROR, "getdomainname: %m");
		return EIO;
	}

	domain = default_domain;
	if (!*domain) {
		plog(XLOG_ERROR, "YP domain name is not set");
		return ENOENT;
	}

	return 0;
}

/*
 * Try to locate a key using NIS.
 * Modify time is ignored in NIS - XXX
 */
int nis_search P((mnt_map *m, char *map, char *key, char **val, time_t *tp));
int nis_search(m, map, key, val, tp)
mnt_map *m;
char *map;
char *key;
char **val;
time_t *tp;
{
	int outlen;
	int res;

	if (!domain) {
		int error = determine_nis_domain();
		if (error)
			return error;
	}

	res = yp_match(domain, map, key, strlen(key), val, &outlen);

	/*
	 * Do something interesting with the return code
	 */
	switch (res) {
	case 0:
		return 0;

	case YPERR_KEY:
		return ENOENT;

	default:
		plog(XLOG_ERROR, "%s: %s", map, yperr_string(res));
		return EIO;
	}
}

int nis_init P((char *map));
int nis_init(map)
char *map;
{
	char *name = 0;

	if (!domain) {
		int error = determine_nis_domain();
		if (error)
			return error;
	}

	/*
	 * To see if the map exists, try to find
	 * a master for it.
	 */
	if (yp_master(domain, map, &name))
		return ENOENT;
	free(name);
	return 0;
}
#endif /* HAS_NIS_MAPS */
