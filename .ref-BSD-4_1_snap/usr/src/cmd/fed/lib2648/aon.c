#include "2648.h"

aon()
{
	sync();
	escseq(ESCD);
	outchar('e');
}

aoff()
{
	sync();
	escseq(ESCD);
	outchar('f');
}
