#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <strings.h>
#ifndef O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif
#ifndef	SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#endif

#ifndef	MAXPATHLEN
#define	MAXPATHLEN	1024
#endif	/* MAXPATHLEN */

#ifndef DONTDECLARE_MALLOC
extern PTR  EXFUN(malloc,(unsigned));
extern PTR  EXFUN(realloc, (PTR, unsigned));
extern void EXFUN( free,(PTR));
#endif

/* EXACT TYPES */
typedef char int8e_type;
typedef unsigned char uint8e_type;
typedef short int16e_type;
typedef unsigned short uint16e_type;
typedef int int32e_type;
typedef unsigned int uint32e_type;

/* CORRECT SIZE OR GREATER */
typedef char int8_type;
typedef unsigned char uint8_type;
typedef short int16_type;
typedef unsigned short uint16_type;
typedef int int32_type;
typedef unsigned int uint32_type;

#include "fopen-same.h"
