#include "sys/param.h"
#include "sys/conf.h"

dev_t	rootdev = NODEV;
dev_t	dumpdev = NODEV;

struct	swdevt swdevt[] = {
	{ NODEV,	0,	5000 },	/* Remote NFS swap  */
	{ 0, 0, 0 }
};
