#include "2648.h"

zoomn(size)
char size;
{
	sync();
	escseq(ESCD);
	outchar(size+'0');
	outchar('i');
}
