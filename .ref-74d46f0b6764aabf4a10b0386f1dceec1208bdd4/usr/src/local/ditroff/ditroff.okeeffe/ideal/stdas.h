/*	stdas.h	(CWI)	1.1	85/03/01	*/
/* STanDard memory Allocation and String manipulation function declarations */

extern char *strcat();
extern char *strncat();
extern int strcmp();
extern int strncmp();
extern char *strcpy();
extern char *strncpy();
extern int strlen();
extern char *index();
extern char *rindex();

extern char *malloc();
extern free();
extern char *realloc();
extern char *calloc();

extern char *galloc();
extern char *gcalloc();
extern void gfree();
