/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vbaparam.h	7.3 (Berkeley) %G%
 */

/*
 * Parameters related to the VERSAbus i/o configuration.
 */

/*
 * VERSAbus i/o devices use either memory mapped interfaces
 * or mapped i/o register banks, or some of both.  Page tables
 * are allocated at boot time by each device driver, as needed.
 * VMEMmap is used to map a fixed size portion of the VERSAbus
 * i/o space, while VMEMmap1 maps dynamically defined portions
 * for devices which utilize shared i/o memory.  VBmap is used
 * for mapping kernel intermediate buffers for DMA devices which
 * are incapable of utilizing user virtual addresses or which
 * require page aligned i/o buffers.  The size of the VMEMmap1
 * VBmap tables must be large enough for the needs of all devices
 * in the system.
 */
extern	struct pte VMEMmap[], VMEMmap1[];
extern	caddr_t	vmem1, vmemend;
extern	struct pte VBmap[];
extern	caddr_t vbbase, vbend; 

/*
 * The following macros relate to the segmentation of the VERSAbus
 * i/o space.
 *
 * The VERSAbus adapter segments the i/o space (as seen by the cpu)
 * into three regions.  Cpu accesses to the upper 64Kb of the i/o space
 * generate VERSAbus cycles with a 16-bit address and a non-privileged
 * short i/o space address modifier.  Accesses to the next 1Mb - 64Kb
 * generate 24-bit addresses and a non-privileged standard address
 * modifier.  Accesses to the remainder of the 1Gb i/o space generate
 * 32-bit addresses with a non-privileged extended address modifier.
 * Beware that 32-bit addresses generated from this region always have
 * zero in the upper 2 bits; e.g. a reference to physical address fe000000
 * results in a VERSAbus address of 3e000000.
 */
#define	VBIO16BIT(a)	((unsigned)0xfffe0000 <= ((unsigned)(a)))
#define	VBIO24BIT(a)	((unsigned)0xff000000 <= ((unsigned)(a)) && \
			 ((unsigned)(a)) < (unsigned)0xfffe0000)
#define	VBIO32BIT(a)	(((unsigned)(a)) < (unsigned)0xff000000)

/* 
 * The following constants define the fixed size map of the
 * VERSAbus i/o space.  The values should reflect the range
 * of i/o addresses used by all the controllers unprepared
 * to allocate and initialize their own page maps.
 */
#define VBIOBASE	0xfff00000	/* base of VERSAbus address space */
#define VBIOEND		0xffffee45	/* last address in mapped space */
/* number of entries in the system page table for i/o space */
#define VBIOSIZE	btoc(VBIOEND-VBIOBASE)
/* is device in mapped region */
#define	VBIOMAPPED(a)	((unsigned)VBIOBASE <= ((unsigned)(a)) && \
			 ((unsigned)(a)) <= (unsigned)VBIOEND) 
#define	vboff(addr)	((int)(((caddr_t)(addr)) - VBIOBASE))
