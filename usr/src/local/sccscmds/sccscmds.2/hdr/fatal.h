/*	fatal.h	1.1	88/12/22	*/

extern int Fflags;
extern char *Ffile;
extern int Fvalue;
extern int (*Ffunc)();
extern int Fjmp[10];

#define FTLMSG 0100000
#define FTLCLN 0040000
#define FTLFUNC 020000
#define FTLACT     077
#define FTLJMP      02
#define FTLEXIT      1
#define FTLRET       0
