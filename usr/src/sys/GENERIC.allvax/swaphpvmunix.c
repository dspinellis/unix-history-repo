#include "../sys/param.h"
#include "../sys/conf.h"

dev_t	rootdev = makedev(0, 0);
dev_t	argdev  = makedev(0, 1);
dev_t	dumpdev = makedev(0, 1);

struct	swdevt swdevt[] = {
	{ makedev(0, 1),	0,	0 },	/* hp0b */
	{ 0, 0, 0 }
};
