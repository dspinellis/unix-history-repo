/*	@(#)htable.h	4.1 (Berkeley) %G%	*/

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

#define	alloc_addr(dummy)	((struct addr *) malloc(sizeof(struct addr)))
#define	free_addr(x)		free((char *) x)
#define	NOADDR			((struct addr *) 0)
#define net(x)			((x) & 0xff)
#define host(x)			(((x) >> 8) & 0xff)
#define lhost(x)		(((x) >> 16) & 0xff)
#define imp(x)			(((x) >> 24) & 0xff)

#define	alloc_name(dummy)	((struct name *) malloc(sizeof(struct name)))
#define	free_name(x)		free(x->name_val); \
				free((char *) x)
#define	NONAME			((struct name *) 0)

#define	KW_NET		1
#define	KW_GATEWAY	2
#define	KW_HOST		3

struct name *newname();
char *malloc();

char *infile;			/* Input file name */
