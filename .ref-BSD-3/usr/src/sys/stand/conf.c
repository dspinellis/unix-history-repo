#include "../h/param.h"
#include "../h/inode.h"
#include "../h/mba.h"
#include "saio.h"

devread(io)
register struct iob *io;
{

	return( (*devsw[io->i_ino.i_dev].dv_strategy)(io,READ) );
}

devwrite(io)
register struct iob *io;
{
	return( (*devsw[io->i_ino.i_dev].dv_strategy)(io, WRITE) );
}

devopen(io)
register struct iob *io;
{
	(*devsw[io->i_ino.i_dev].dv_open)(io);
}

devclose(io)
register struct iob *io;
{
	(*devsw[io->i_ino.i_dev].dv_close)(io);
}

nullsys()
{ ; }

int	nullsys();
int	hpstrategy(), hpopen();
int	htstrategy(), htopen(), htclose();
int	upstrategy(), upopen();

struct devsw devsw[] = {
	"hp",	hpstrategy,	hpopen,		nullsys,
	"ht",	htstrategy,	htopen,		htclose,
	"up",	upstrategy,	upopen,		nullsys,
	0,0,0,0
};

int mbanum[] = {	/* mba number of major device */
	0,		/* disk */
	1,		/* tape */
	-1,		/* unused */
};

int *mbaloc[] = { 	/* physical location of mba */
	(int *)PHYSMBA0,
	(int *)PHYSMBA1,
};
