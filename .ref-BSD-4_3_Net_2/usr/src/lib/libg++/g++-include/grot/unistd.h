#ifndef unistd_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define unistd_h 1

#include <sys/fcntl.h>

/* A safe-looking set of things from various system versions */

#ifndef SEEK_SET
#define SEEK_SET 0
#endif

#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif

#ifndef SEEK_END
#define SEEK_END 2
#endif

#ifndef GF_PATH
#define GF_PATH "/etc/group"
#endif

#ifndef PF_PATH
#define PF_PATH "/etc/passwd"
#endif

#ifndef IN_PATH
#define IN_PATH "/usr/include"
#endif

#ifndef R_OK
#define R_OK 4
#endif

#ifndef W_OK
#define W_OK 2
#endif

#ifndef X_OK
#define X_OK 1
#endif

#ifndef F_OK
#define F_OK 0
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef F_ULOCK
#define F_ULOCK 0
#endif

#ifndef F_LOCK
#define F_LOCK 1
#endif

#ifndef F_TLOCK
#define F_TLOCK 2
#endif

#ifndef F_TEST
#define F_TEST 3
#endif

#endif


