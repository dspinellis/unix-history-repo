/*	trap.h	4.6	81/03/03	*/

/*
 * Trap type values
 */

/* The first three constant values are known to the real world <signal.h> */
#define	RESADFLT	0		/* reserved addressing fault */
#define	PRIVINFLT	1		/* privileged instruction fault */
#define	RESOPFLT	2		/* reserved operand fault */
/* End of known constants */
#define	BPTFLT		3		/* bpt instruction fault */
#define	XFCFLT		4		/* xfc instruction fault */
#define	SYSCALL		5		/* chmk instruction (syscall trap) */
#define	ARITHTRAP	6		/* arithmetic trap */
#define	ASTFLT		7		/* software level 2 trap (ast deliv) */
#define	SEGFLT		8		/* segmentation fault */
#define	PROTFLT		9		/* protection fault */
#define	TRCTRAP		10		/* trace trap */
#define	COMPATFLT	11		/* compatibility mode fault */
#define	PAGEFLT		12		/* page fault */
#define	TABLEFLT	13		/* page table fault */
