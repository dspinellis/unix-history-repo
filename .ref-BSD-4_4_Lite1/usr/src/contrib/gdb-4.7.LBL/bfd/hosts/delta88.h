#include <errno.h>
#include <sys/types.h>
#include <unistd.h>

#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>
#include <ctype.h>

#include <stdio.h>

#ifndef DONTDECLARE_MALLOC
extern PTR  EXFUN(malloc,(unsigned));
extern PTR  EXFUN(realloc, (PTR, unsigned));
#endif
extern int  EXFUN(abort,(void));
extern int  EXFUN(free,(PTR));
extern void EXFUN(bcopy,(char*,char*,int));
extern void EXFUN(exit,(int));
extern int  EXFUN(strtol());
extern void EXFUN(bzero,(char *, int));
#define NO_STDARG 1
extern int  EXFUN(abort,(void));
extern int  EXFUN(close,(int));
extern int  EXFUN(fcntl,(int des, int cmd, int e));
extern int  EXFUN(qsort,(void *data,int els, int siz, int func()));
extern int  EXFUN(fseek,(FILE*, int, int));
extern int  EXFUN(fclose,(FILE*));
extern void EXFUN(bcopy,(char*,char*,int));
extern int  EXFUN(bcmp,(char *, char *, int));
extern void EXFUN(bzero,(char *, int));
extern void EXFUN(perror,(CONST char *));
extern PTR  EXFUN(memchr,(const void *, int, unsigned ));
extern unsigned short EXFUN(getuid,(void));
extern unsigned short EXFUN(getgid,(void));

extern char * strchr();

extern char *getenv();
extern char *strrchr();
extern int chmod();
extern int fread();
extern int fstat();
extern int fwrite();
extern int sscanf();
extern int stat();
extern int strtol();
extern int fileno();
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
