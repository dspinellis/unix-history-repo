#include "2648.h"

vidnorm()
{
	_video = NORMAL;
}

vidinv()
{
	_video = INVERSE;
}

togvid()
{
	_video = (_video==NORMAL) ? INVERSE : NORMAL;
	escseq(ESCM);
	outstr("3a1b0 0 719 359e");
}
