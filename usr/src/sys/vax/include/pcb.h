/*	pcb.h	4.4	81/02/26	*/

/*
 * VAX process control block
 */

struct pcb
{
	int	pcb_ksp; 	/* kernel stack pointer */
	int	pcb_esp; 	/* exec stack pointer */
	int	pcb_ssp; 	/* supervisor stack pointer */
	int	pcb_usp; 	/* user stack pointer */
	int	pcb_r0; 
	int	pcb_r1; 
	int	pcb_r2; 
	int	pcb_r3; 
	int	pcb_r4; 
	int	pcb_r5; 
	int	pcb_r6; 
	int	pcb_r7; 
	int	pcb_r8; 
	int	pcb_r9; 
	int	pcb_r10; 
	int	pcb_r11; 
	int	pcb_r12; 
	int	pcb_r13; 
	int	pcb_pc; 	/* program counter */
	int	pcb_psl; 	/* program status longword */
	struct  pte *pcb_p0br; 	/* seg 0 base register */
	int	pcb_p0lr; 	/* seg 0 length register and astlevel */
	struct  pte *pcb_p1br; 	/* seg 1 base register */
	int	pcb_p1lr; 	/* seg 1 length register and pme */
/*
 * Software pcb (extension)
 */
	int	pcb_szpt; 	/* number of pages of user page table */
	int	pcb_cmap2;
	int	*pcb_sswap;
	int	pcb_sigc[3];
};

#define	AST_NONE	0x04000000	/* ast level */
#define	AST_USER	0x03000000	/* ast for user mode */

#define	ASTLVL_NONE	4
#define	ASTLVL_USER	3

#define	AST_CLR		0x07000000

#define	aston() \
	{ \
		u.u_pcb.pcb_p0lr = (u.u_pcb.pcb_p0lr &~ AST_CLR) | AST_USER; \
		mtpr(ASTLVL, ASTLVL_USER); \
	}

#define	astoff() \
	{ \
		u.u_pcb.pcb_p0lr = (u.u_pcb.pcb_p0lr &~ AST_CLR) | AST_NONE; \
		mtpr(ASTLVL, ASTLVL_NONE); \
	}
