/*	conf.c	4.10	82/05/27	*/

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
int	htstrategy(), htopen(), htclose();
#endif
int	upstrategy(), upopen();
int	tmstrategy(), tmopen(), tmclose();
int	tsstrategy(), tsopen(), tsclose();
#if defined(VAX780) || defined(VAX750)
int	mtstrategy(), mtopen(), mtclose();
#endif
int	rkstrategy(), rkopen();
int	udstrategy(), udopen();
int	utstrategy(), utopen(), utclose();
int	idcstrategy(), idcopen();

struct devsw devsw[] = {
#if defined(VAX780) || defined(VAX750)
	"hp",	hpstrategy,	hpopen,		nullsys,
	"ht",	htstrategy,	htopen,		htclose,
#endif
	"up",	upstrategy,	upopen,		nullsys,
	"tm",	tmstrategy,	tmopen,		tmclose,
	"hk",	rkstrategy,	rkopen,		nullsys,
	"ts",	tsstrategy,	tsopen,		tsclose,
#if defined(VAX780) || defined(VAX750)
	"mt",	mtstrategy,	mtopen,		mtclose,
#endif
	"ra",	udstrategy,	udopen,		nullsys,
	"ut",	utstrategy,	utopen		utclose,
	"rb",	idcstrategy,	idcopen,	nullsys,
	0,0,0,0
};
