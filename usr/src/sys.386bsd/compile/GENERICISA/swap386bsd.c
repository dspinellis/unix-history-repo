#include "sys/param.h"
#include "sys/conf.h"

dev_t	rootdev = makedev(0, 0);
dev_t	dumpdev = makedev(0, 1);

struct	swdevt swdevt[] = {
	{ makedev(0, 1),	0,	0 },	/* wd0b */
	{ makedev(4, 1),	0,	0 },	/* as0b */
	{ 0, 0, 0 }
};
