#include "../sys/param.h"
#include "../sys/conf.h"

dev_t	rootdev = makedev(3, 0);
dev_t	argdev  = makedev(3, 1);
dev_t	dumpdev = makedev(3, 1);

struct	swdevt swdevt[] = {
	{ makedev(3, 1),	0,	0 },	/* hk0b */
	{ 0, 0, 0 }
};
