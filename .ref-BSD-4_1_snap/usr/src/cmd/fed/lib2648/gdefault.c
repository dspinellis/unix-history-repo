/*
 * reset terminal to default graphics state
 */
#include "2648.h"

gdefault()
{
	escseq(ESCM);
	outstr("r");
}
