#include "2648.h"

cleara()
{
	sync();
	escseq(NONE);
	outstr("\33H\33J");
}
