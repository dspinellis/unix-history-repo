#include "2648.h"

beep()
{
	escseq(NONE);
	outchar('\7');
}
