/*
** sys/types.h
**
** Emulation of the Unix sys/types.h header file for PRIMOS
**
** Author: Peter Eriksson <pen@lysator.liu.se>
*/

#ifndef __SYS_TYPES_H__
#define __SYS_TYPES_H__

typedef long	size_t;
typedef long	time_t;

typedef long	off_t;
typedef short	dev_t;
typedef short	ino_t;
typedef short	mode_t;
typedef short	uid_t;
typedef short	gid_t;

typedef char   *caddr_t;

#endif

