#include <ansidecl.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <string.h>
#include <sys/file.h>
#ifndef O_ACCMODE
#define O_ACCMODE (O_RDONLY | O_WRONLY | O_RDWR)
#endif
#define SEEK_SET 0
#define SEEK_CUR 1

#ifdef __STDC__
extern void EXFUN(free,(PTR));
#else /* not __STDC__ */
extern int EXFUN(free,(PTR));
#endif /* not __STDC__ */
extern void  EXFUN(abort,(void));
extern int  EXFUN(close,(int));
extern int  EXFUN(fcntl,(int des, int cmd, int e));
extern int  EXFUN(fprintf,(FILE *,char *,...));
extern int  EXFUN(printf,(char *,...));
extern int  EXFUN(qsort,(void *data,int els, int siz, int func()));
extern void  EXFUN(exit,(int));
extern int  EXFUN(fseek,(FILE*, int, int));
extern int  EXFUN(fclose,(FILE*));
extern void EXFUN(bcopy,(char*,char*,int));
extern int  EXFUN(bcmp,(char *, char *, int));
extern void EXFUN(bzero,(char *, int));
extern void EXFUN(perror,(CONST char *));
extern char *getenv();
extern char * strchr();
extern char *memchr();
extern char *strrchr();
extern int chmod();
extern int fread();
extern int fstat();
extern int fwrite();
extern int sscanf();
extern int stat();
extern int strtol();
#ifndef DONTDECLARE_MALLOC
extern PTR   EXFUN(malloc,(unsigned));
extern PTR   EXFUN(realloc, (PTR, unsigned));
#endif

extern char *strrchr();
extern char *ctime();
extern int _flsbuf();
extern int fclose();
extern int time();
extern int utimes();
extern int vfprintf();
extern long atol();
extern char *getenv();
extern int fputc();
extern int unlink();


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
