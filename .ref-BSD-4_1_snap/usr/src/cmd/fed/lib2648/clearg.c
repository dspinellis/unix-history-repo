#include "2648.h"

clearg()
{
	sync();
	escseq(ESCD);
	outchar((_video==INVERSE) ? 'b' : 'a');
}
