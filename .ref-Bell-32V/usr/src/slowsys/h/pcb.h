/*
   VAX process control block
 */

struct pcb {
	int
	pcb_ksp, 	/* kernal stack pointer */
	pcb_esp, 	/* exec stack pointer */
	pcb_ssp, 	/* supervisor stack pointer */
	pcb_usp, 	/* user stack pointer */
	pcb_r0, 
	pcb_r1, 
	pcb_r2, 
	pcb_r3, 
	pcb_r4, 
	pcb_r5, 
	pcb_r6, 
	pcb_r7, 
	pcb_r8, 
	pcb_r9, 
	pcb_r10, 
	pcb_r11, 
	pcb_r12, 
	pcb_r13, 
	pcb_pc, 	/* program counter */
	pcb_psl, 	/* program status longword */
	pcb_p0br, 	/* seg 0 base register */
	pcb_p0lr, 	/* seg 0 length register and astlevel */
	pcb_p1br, 	/* seg 1 base register */
	pcb_p1lr, 	/* seg 1 length register and pme */

/*
   Software pcb (extension)
 */

	pcb_szpt; 	/* number of pages of user page table */

};
