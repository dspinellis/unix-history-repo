#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#ifndef O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif
 
#ifndef DONTDECLARE_MALLOC
extern PTR  EXFUN(malloc,(unsigned));
extern PTR  EXFUN(realloc, (PTR, unsigned));
#endif
extern int  EXFUN(abort,(void));
extern int  EXFUN(free,(PTR));
extern void EXFUN(bcopy,(char*,char*,int));
extern void EXFUN(exit,(int));
extern void EXFUN(bzero,(char *, int));
extern int strtol();
#define NO_STDARG 1
 
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
