/*	@(#)htable.h	4.2 (Berkeley) %G%	*/

#include <sys/types.h>

/*
 * common definitions for htable
 */

struct addr {
	u_long	addr_val;
	struct	addr *addr_link;
};

struct name {
	char	*name_val;
	struct	name *name_link;
};

#define	NOADDR			((struct addr *)0)
#define	NONAME			((struct name *)0)

#define	KW_NET		1
#define	KW_GATEWAY	2
#define	KW_HOST		3

struct name *newname();
char *malloc();

char *infile;			/* Input file name */
