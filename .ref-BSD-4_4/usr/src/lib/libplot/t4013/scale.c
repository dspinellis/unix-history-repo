/O Registers */
#endif	IO_ISABEGIN

/*
 * Input / Output Memory Physical Addresses
 */

#ifndef	IOM_BEGIN
#define	IOM_BEGIN	0xa0000		/* Start of I/O Memory "hole" */
#define	IOM_END		0xFFFFF		/* End of I/O Memory "hole" */
#endif	IOM_BEGIN

/*
 * RAM Physical Address Space (ignoring the above mentioned "hole")
 */

#ifndef	RAM_BEGIN
#define	RAM_BEGIN	0x000000	/* Start of RAM Memory */
#define	RAM_END		0xFFFFFF	/* End of RAM Memory */
#endif	IOM_BEGIN

/*
 * Oddball Physical Memory Addresses
 */
#ifndef	COMPAQ_RAMRELOC
#define	COMPAQ_RAMRELOC	0x80c00000	/* Compaq RAM relocation/diag */
#define	COMPAQ_RAMSETUP	0x80c00002	/* Compaq RAM setup */
#define	WEITEK_FPU	0xC0000000	/* WT