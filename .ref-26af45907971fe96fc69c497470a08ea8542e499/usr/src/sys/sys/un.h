/*
 * Copyright (c) 1982, 1986, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)un.h	8.1 (Berkeley) %G%
 */

/*
 * Definitions for UNIX IPC domain.
 */
struct	sockaddr_un {
	u_char	sun_len;		/* sockaddr len including null */
	u_char	sun_family;		/* AF_UNIX */
	char	sun_path[104];		/* path name (gag) */
};

#ifdef KERNEL
int	unp_discard();
#else

/* actual length of an initialized sockaddr_un */
#define SUN_LEN(su) \
	(sizeof(*(su)) - sizeof((su)->sun_path) + strlen((su)->sun_path))
#endif
