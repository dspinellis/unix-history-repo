/*	SYS.h	1.1	86/01/03	*/

/*
 * Macros used to define entry points
 * in assembly language routines.
 */
#ifdef GPROF
#define	ENTRY(name, regs) \
	.globl _/**/name; .align 2; _/**/name: .word regs; callf $0,mcount
#define	ASENTRY(name, regs) \
	.globl name; .align 2; name: .word regs; callf $0,mcount
#else
#define	ENTRY(name, regs) \
	.globl _/**/name; .align 2; _/**/name: .word regs
#define	ASENTRY(name, regs) \
	.globl name; .align 2; name: .word regs;
#endif PROF
#define R0	0x0001
#define R1	0x0002
#define R2	0x0004
#define R3	0x0008
#define R4	0x0010
#define R5	0x0020
#define R6	0x0040
#define	R7	0x0080
#define	R8	0x0100
#define	R9	0x0200
#define	R10	0x0400
#define	R11	0x0800
#define	R12	0x1000
