/*	fatal.h	1.2	88/12/22	*/

#include <setjmp.h>

extern int Fflags;
extern char *Ffile;
extern int Fvalue;
extern int (*Ffunc)();
extern jmp_buf Fjmp;

#define FTLMSG 0100000
#define FTLCLN 0040000
#define FTLFUNC 020000
#define FTLACT     077
#define FTLJMP      02
#define FTLEXIT      1
#define FTLRET       0
