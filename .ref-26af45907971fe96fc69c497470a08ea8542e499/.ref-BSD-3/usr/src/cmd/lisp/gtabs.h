/*  these are the tables of global lispvals known to the interpreter	*/
/*  and compiler.  They are not used by the garbage collector.		*/
#define GFTABLEN 200
#define GCTABLEN 8
extern lispval gftab[GFTABLEN];
extern lispval gctab[GCTABLEN];
