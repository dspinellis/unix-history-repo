/*	conf.c	4.8	81/11/12	*/

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/mbareg.h"
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
int	hpstrategy(), hpopen();
int	htstrategy(), htopen(), htclose();
int	upstrategy(), upopen();
int	tmstrategy(), tmopen(), tmclose();
int	tsstrategy(), tsopen(), tsclose();
int	mtstrategy(), mtopen(), mtclose();
int	rkstrategy(), rkopen();
int	udstrategy(), udopen();

struct devsw devsw[] = {
	"hp",	hpstrategy,	hpopen,		nullsys,
	"ht",	htstrategy,	htopen,		htclose,
	"up",	upstrategy,	upopen,		nullsys,
	"tm",	tmstrategy,	tmopen,		tmclose,
	"hk",	rkstrategy,	rkopen,		nullsys,
	"ts",	tsstrategy,	tsopen,		tsclose,
	"mt",	mtstrategy,	mtopen,		mtclose,
	"ra",	udstrategy,	udopen,		nullsys,
	0,0,0,0
};
