/*	conf.c	4.11	82/06/25	*/

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
#if defined(VAX780) || defined(VAX750)
int	hpstrategy(), hpopen();
#endif
int	upstrategy(), upopen();
int	rkstrategy(), rkopen();
int	udstrategy(), udopen();
int	idcstrategy(), idcopen();
#ifndef BOOT
int	tmstrategy(), tmopen(), tmclose();
int	tsstrategy(), tsopen(), tsclose();
#if defined(VAX780) || defined(VAX750)
int	htstrategy(), htopen(), htclose();
int	mtstrategy(), mtopen(), mtclose();
#endif
int	utstrategy(), utopen(), utclose();
#endif

struct devsw devsw[] = {
#if defined(VAX780) || defined(VAX750)
	"hp",	hpstrategy,	hpopen,		nullsys,
#endif
	"up",	upstrategy,	upopen,		nullsys,
	"hk",	rkstrategy,	rkopen,		nullsys,
	"ra",	udstrategy,	udopen,		nullsys,
	"rb",	idcstrategy,	idcopen,	nullsys,
#ifndef BOOT
	"ts",	tsstrategy,	tsopen,		tsclose,
#if defined(VAX780) || defined(VAX750)
	"ht",	htstrategy,	htopen,		htclose,
	"mt",	mtstrategy,	mtopen,		mtclose,
#endif
	"tm",	tmstrategy,	tmopen,		tmclose,
	"ut",	utstrategy,	utopen,		utclose,
#endif
	0,0,0,0
};
