/*	trap.h	1.3	87/07/11	*/

/*	first 3 constants known in system C files - don't change */
#define	T_RESADFLT	0	/* reserved addressing */
#define	T_PRIVINFLT	1	/* privileged instruction */
#define	T_RESOPFLT	2	/* reserved operand */
/*	those constants shouldn't change 	*/
#define	T_BPTFLT	3	/* breakpoint instruction */
#define	T_SYSCALL	5	/* system call (kcall) */
#define	T_ARITHTRAP	6	/* arithmetic trap */
#define	T_ASTFLT	7	/* system forced exception */
#define	T_SEGFLT	8	/* segmentation (limit) fault */
#define	T_PROTFLT	9	/* protection fault */
#define	T_TRCTRAP	10	/* trace trap */
#define	T_PAGEFLT	12	/* page fault */
#define	T_TABLEFLT	13	/* page table fault */
#define	T_ALIGNFLT	14	/* alignment fault */
#define	T_KSPNOTVAL	15	/* kernel stack pointer not valid */
#define	T_BUSERR	16	/* bus error */
#define	T_KDBTRAP	17	/* kernel debugger trap */
