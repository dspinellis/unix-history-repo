/*	confxx.c	4.2	82/12/17	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "saio.h"

devread(io)
	register struct iob *io;
{

	return( (*devsw[io->i_ino.i_dev].dv_strategy)(io, READ) );
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
{

	;
}

int	nullsys();
int	xxstrategy(), xxopen();

struct devsw devsw[] = {
	"XX",	xxstrategy,	xxopen,		nullsys,
	0,0,0,0
};
