#include <sys/param.h>
#include <sys/inode.h>
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

int rpstrategy();
int rkstrategy();
int	nullsys();
int	tmstrategy(), tmrew(), tmopen();
int	htstrategy(), htopen(),htclose();
int	hpstrategy();
struct devsw devsw[] {
	"rp",	rpstrategy,	nullsys,	nullsys,
	"hp",	hpstrategy,	nullsys,	nullsys,
	"rk",	rkstrategy,	nullsys,	nullsys,
	"tm",	tmstrategy,	tmopen,		tmrew,
	"ht",	htstrategy,	htopen,		htclose,
	0,0,0,0
};
