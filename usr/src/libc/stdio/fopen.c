#include	<stdio.h>

FILE *
fopen(file, mode)
	char *file, *mode;
{
	FILE *_findiop(), *_endopen();

	return(_endopen(file, mode, _findiop()));
}
