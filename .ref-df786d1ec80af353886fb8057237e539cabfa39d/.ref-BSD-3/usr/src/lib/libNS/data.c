/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
char	_sibuf[BUFSIZ];
char	_sobuf[BUFSIZ];

struct	_iobuf	_iob[_NFILE] = {
	{ 0, _sibuf, _sibuf, _IOREAD, 0, 0},
	{ 0, NULL, NULL, _IOWRT, 1, 0},
	{ 0, NULL, NULL, _IOWRT+_IONBF, 2, 0},
};
/*
 * Ptr to end of buffers
 */
struct	_iobuf	*_lastbuf = { &_iob[_NFILE] };
