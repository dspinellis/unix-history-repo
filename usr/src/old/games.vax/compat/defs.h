/*
 * 	defs.h	1.1	82/05/12
 */
#define	EMTS	0210
#define	TRAPS	0211
#define	SETD	0170011
extern unsigned int sigvals[];
extern unsigned long psl;
extern unsigned short regs[];
extern unsigned short *pc;
extern unsigned short wordspace[];
extern unsigned char bytespace[];
extern unsigned char *memsiz;
extern int incompat;
