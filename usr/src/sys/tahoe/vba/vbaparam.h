/*	vbaparam.h	1.1	86/01/21	*/

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
 * The following constants define the fixed size map of the
 * VERSAbus i/o space.  The values should reflect the range
 * of i/o addresses used by all the controllers handled in
 * the system as specified in the ubminit structure in ioconf.c.
 */
#define VBIOBASE	0xfff00000	/* base of VERSAbus address space */
#define VBIOEND		0xffffee45	/* last address in mapped space */
/* number of entries in the system page pable for i/o space */
#define VBIOSIZE	btoc(VBIOEND-VBIOBASE)

/*
 * Page table map sizes.
 *
 * Current VBmap allotments are:
 *	4 vd controllers	32+1 pte's
 *	2 cy controllers	32+1 pte's
 * Current VBMEMmap allotments are:
 *	2 ace controllers	32+1 pte's
 */
#define	VBPTSIZE	(((4*(32+1))+2*(32+1)+3) &~ 3)
#define	VBMEMSIZE	((2*(32+1)+3) &~ 3)
