/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)qd.c	1.12 (Berkeley) %G%
 */

/************************************************************************
*									*
*			Copyright (c) 1985-1988 by			*
*		Digital Equipment Corporation, Maynard, MA		*
*			All rights reserved.				*
*									*
*   This software is furnished under a license and may be used and	*
*   copied  only  in accordance with the terms of such license and	*
*   with the  inclusion  of  the  above  copyright  notice.   This	*
*   software  or  any  other copies thereof may not be provided or	*
*   otherwise made available to any other person.  No title to and	*
*   ownership of the software is hereby transferred.			*
*									*
*   The information in this software is subject to change  without	*
*   notice  and should not be construed as a commitment by Digital	*
*   Equipment Corporation.						*
*									*
*   Digital assumes no responsibility for the use  or  reliability	*
*   of its software on equipment which is not supplied by Digital.	*
*									*
*************************************************************************/

/*
 * qd.c - QDSS display driver for VAXSTATION-II GPX workstation
 */

#include "qd.h"

#if NQD > 0
#include "types.h"
#include "machine/pte.h"
#include "machine/mtpr.h"
#include "machine/cpu.h"
#include "param.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "qdioctl.h"
#include "tty.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "bk.h"
#include "clist.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "exec.h"
#include "proc.h"
#include "ubareg.h"
#include "ubavar.h"
#include "syslog.h"
#include "qduser.h"	/* definitions shared with user level client */
#include "qdreg.h"	/* QDSS device register structures */

/*
 * QDSS driver status flags for tracking operational state 
 */
struct qdflags {
	u_int inuse;		/* which minor dev's are in use now */
	u_int config;		/* I/O page register content */
	u_int mapped;		/* user mapping status word */
	u_int kernel_loop;	/* if kernel console is redirected */
	u_int user_dma;		/* DMA from user space in progress */
	u_short pntr_id;	/* type code of pointing device */
	u_short duart_imask;	/* shadowing for duart intrpt mask reg */
	u_short adder_ie;	/* shadowing for adder intrpt enbl reg */
	u_short curs_acc;	/* cursor acceleration factor */
	u_short curs_thr;	/* cursor acceleration threshold level */
	u_short tab_res;	/* tablet resolution factor */
	u_short selmask;	/* mask for active qd select entries */
};

/*
 * bit definitions for 'inuse' entry  
 */
#define CONS_DEV	0x01
#define GRAPHIC_DEV	0x04

/*
 * bit definitions for 'mapped' member of flag structure 
 */
#define MAPDEV		0x01		/* hardware is mapped */
#define MAPDMA		0x02		/* DMA buffer mapped */
#define MAPEQ		0x04		/* event queue buffer mapped */
#define MAPSCR		0x08		/* scroll param area mapped */
#define MAPCOLOR	0x10		/* color map writing buffer mapped */

/*
 * bit definitions for 'selmask' member of qdflag structure 
 */
#define SEL_READ	0x01		/* read select is active */
#define SEL_WRITE	0x02		/* write select is active */

/*
 * constants used in shared memory operations 
 */
#define EVENT_BUFSIZE  1024	/* # of bytes per device's event buffer */
#define MAXEVENTS  ( (EVENT_BUFSIZE - sizeof(struct qdinput))	 \
	/ sizeof(struct _vs_event) )
#define DMA_BUFSIZ	(1024 * 10)
#define COLOR_BUFSIZ  ((sizeof(struct color_buf) + 512) & ~0x01FF)

/*
 * reference to an array of "uba_device" structures built by the auto
 * configuration program.  The uba_device structure decribes the device
 * sufficiently for the driver to talk to it.  The auto configuration code
 * fills in the uba_device structures (located in ioconf.c) from user
 * maintained info.  
 */
struct uba_device *qdinfo[NQD];  /* array of pntrs to each QDSS's */
struct tty qd_tty[NQD*4];	/* teletype structures for each.. */
extern char qvmem[][128*NBPG];
extern struct pte QVmap[][128];
#define CHUNK	  (64 * 1024)
#define QMEMSIZE  (1024 * 1024 * 4)	/* 4 meg */

/*
 * static storage used by multiple functions in this code  
 */
int Qbus_unmap[NQD];		/* Qbus mapper release code */
struct qdflags qdflags[NQD];	/* QDSS device status flags */
struct qdmap qdmap[NQD];	/* QDSS register map structure */
caddr_t qdbase[NQD];		/* base address of each QDSS unit */
struct buf qdbuf[NQD];		/* buf structs used by strategy */
short qdopened[NQD];		/* graphics device is open exclusive use */

/*
 * the array "event_shared[]" is made up of a number of event queue buffers
 * equal to the number of QDSS's configured into the running kernel (NQD).
 * Each event queue buffer begins with an event queue header (struct qdinput)
 * followed by a group of event queue entries (struct _vs_event).  The array
 * "*eq_header[]" is an array of pointers to the start of each event queue
 * buffer in "event_shared[]".  
 */
#define EQSIZE ((EVENT_BUFSIZE * NQD) + 512)

char event_shared[EQSIZE];	    /* reserve space for event bufs */
struct qdinput *eq_header[NQD];     /* event queue header pntrs */

/*
 * This allocation method reserves enough memory pages for NQD shared DMA I/O
 * buffers.  Each buffer must consume an integral number of memory pages to
 * guarantee that a following buffer will begin on a page boundary.  Also,
 * enough space is allocated so that the FIRST I/O buffer can start at the
 * 1st page boundary after "&DMA_shared".  Page boundaries are used so that
 * memory protections can be turned on/off for individual buffers. 
 */
#define IOBUFSIZE  ((DMA_BUFSIZ * NQD) + 512)

char DMA_shared[IOBUFSIZE];	    /* reserve I/O buffer space */
struct DMAreq_header *DMAheader[NQD];  /* DMA buffer header pntrs */

/*
 * The driver assists a client in scroll operations by loading dragon
 * registers from an interrupt service routine.	The loading is done using
 * parameters found in memory shrade between the driver and it's client.
 * The scroll parameter structures are ALL loacted in the same memory page
 * for reasons of memory economy.  
 */
char scroll_shared[2 * 512];	/* reserve space for scroll structs */
struct scroll *scroll[NQD];	/* pointers to scroll structures */

/*
 * the driver is programmable to provide the user with color map write
 * services at VSYNC interrupt time.  At interrupt time the driver loads
 * the color map with any user-requested load data found in shared memory 
 */
#define COLOR_SHARED  ((COLOR_BUFSIZ * NQD) + 512)

char color_shared[COLOR_SHARED];      /* reserve space: color bufs */
struct color_buf *color_buf[NQD];     /* pointers to color bufs */

/*
 * mouse input event structures 
 */
struct mouse_report last_rep[NQD];
struct mouse_report current_rep[NQD];

struct proc *qdrsel[NQD]; 	/* process waiting for select */
struct _vs_cursor cursor[NQD];	/* console cursor */
int qdcount = 0;		/* count of successfully probed qd's */
int nNQD = NQD;
int DMAbuf_size = DMA_BUFSIZ;
int QDlast_DMAtype;             /* type of the last DMA operation */

#define QDSSMAJOR	41	/* QDSS major device number */
/*
 * macro to get system time.  Used to time stamp event queue entries 
 */
#define TOY ((time.tv_sec * 100) + (time.tv_usec / 10000))

int qdprobe();
int qdattach();
int qddint();			/* DMA gate array intrpt service */
int qdaint();			/* Dragon ADDER intrpt service */
int qdiint();

u_short qdstd[] = { 0 };

struct uba_driver qddriver = {
	qdprobe,		/* device probe entry */
	0,			/* no slave device */
	qdattach,		/* device attach entry */
	0,			/* no "fill csr/ba to start" */
	qdstd,			/* device addresses */
	"qd",			/* device name string */
	qdinfo			/* ptr to QDSS's uba_device struct */
};

#define QDPRIOR (PZERO-1)		/* must be negative */
#define FALSE	0
#define TRUE	~FALSE
#define BAD	-1
#define GOOD	0

/*
 * macro to create a system virtual page number from system virtual adrs 
 */
#define VTOP(x)  (((int)x & ~0xC0000000) >> PGSHIFT)

/*
 * QDSS register address offsets from start of QDSS address space 
 */
#define QDSIZE	 (52 * 1024)	/* size of entire QDSS foot print */
#define TMPSIZE  (16 * 1024)	/* template RAM is 8k SHORT WORDS */
#define TMPSTART 0x8000 	/* offset of template RAM from base adrs */
#define REGSIZE  (5 * 512)	/* regs touch 2.5k (5 pages) of addr space */
#define REGSTART 0xC000 	/* offset of reg pages from base adrs */
#define ADDER	(REGSTART+0x000)
#define DGA	(REGSTART+0x200)
#define DUART	(REGSTART+0x400)
#define MEMCSR	(REGSTART+0x800)
#define CLRSIZE  (3 * 512)		/* color map size */
#define CLRSTART (REGSTART+0xA00)	/* color map start offset from base */
/*  0x0C00 really */
#define RED	(CLRSTART+0x000)
#define BLUE	(CLRSTART+0x200)
#define GREEN	(CLRSTART+0x400)


/*
 * QDSS minor device numbers.  The *real* minor device numbers are in
 * the bottom two bits of the major/minor device spec.  Bits 2 and up are
 * used to specify the QDSS device number (ie: which one?) 
 */

#define CONS		0
#define GRAPHIC 	2

/*
 * console cursor bitmap (white block cursor)  
 */
short cons_cursor[32] = {
	/* A */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
	0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
	/* B */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
	0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF
};

/*
 * constants used in font operations 
 */
#define CHARS		190 			/* # of chars in the font */
#define CHAR_HEIGHT	15			/* char height in pixels */
#define CHAR_WIDTH	8			/* char width in pixels*/
#define FONT_WIDTH	(CHAR_WIDTH * CHARS)	/* font width in pixels */
#define ROWS		CHAR_HEIGHT
#define FONT_X		0			/* font's off screen adrs */
#define FONT_Y		(2048 - CHAR_HEIGHT)

/* Offset to second row characters (XXX - should remove) */
#define FONT_OFFSET	((MAX_SCREEN_X/CHAR_WIDTH)*CHAR_HEIGHT)

extern char q_font[];		/* reference font object code */
extern	u_short q_key[];	/* reference key xlation tables */
extern	u_short q_shift_key[];
extern	char *q_special[];

/*
 * definitions for cursor acceleration reporting  
 */
#define ACC_OFF 	0x01		/* acceleration is inactive */

/*
 * virtual console support.
 */
extern (*v_putc)();
extern struct cdevsw *consops;
int qdputc();
int qdstart();

/*
 * LK-201 state storage for input console keyboard conversion to ASCII 
 */
struct q_keyboard {
	int shift;			/* state variables	*/
	int cntrl;
	int lock;
	int lastcode;			/* last keycode typed	*/
	unsigned kup[8];		/* bits for each keycode*/
	unsigned dkeys[8];		/* down/up mode keys	*/
	char last;			/* last character	*/
} q_keyboard;

/*
 * tty settings on first open
 */
#define IFLAGS	(EVENP|ECHO|XTABS|CRMOD)
#ifdef POSIXTTY
#define IFLAG (BRKINT|ISTRIP|IXON|IXANY|ICRNL|IEXTEN|IMAXBEL)
#define OFLAG (OPOST|OXTABS|ONLCR)
#define LFLAG (ISIG|ICANON|ECHO)
#define CFLAG (PARENB|CREAD|CS7|CLOCAL)
#endif

/*
 * Init QDSS as console (before probe routine)
 */

qdcons_init()
{
	register unit;
	caddr_t phys_adr;		/* physical QDSS base adrs */
	u_int mapix;			/* index into QVmap[] array */
	struct percpu *pcpu;		/* pointer to cpusw structure  */
	register struct qbus *qb;
	u_short *qdaddr;		/* address of QDSS IO page CSR */
	u_short *devptr;		/* vitual device space */
	extern cnputc();

#define QDSSCSR 0x1F00

	if (v_putc != cnputc)
	    return 0;

	unit = 0;

	/*
	 * find the cpusw entry that matches this machine. 
	 */
	for (pcpu = percpu; pcpu && pcpu->pc_cputype != cpu; pcpu++)
		;
	if (pcpu == NULL)
	    return 0;
	if (pcpu->pc_io->io_type != IO_QBUS)
	    return 0;

	/*
	 * Map device registers - the last 8K of qvmem.
	 */
	qb = (struct qbus *)pcpu->pc_io->io_details;
	ioaccess(qb->qb_iopage, UMEMmap[0] + qb->qb_memsize,
		 UBAIOPAGES * NBPG);
	devptr = (u_short *)((char *)umem[0]+(qb->qb_memsize * NBPG));
	qdaddr = (u_short *)((u_int)devptr + ubdevreg(QDSSCSR));
	if (badaddr((caddr_t)qdaddr, sizeof(short)))
		return 0;

	/*
	 * Map q-bus memory used by qdss. (separate map)
	 */
	mapix = QMEMSIZE - (CHUNK * (unit + 1));
	phys_adr = qb->qb_maddr + mapix;
	ioaccess(phys_adr, QVmap[0], (CHUNK*NQD));

	/*
	 * tell QDSS which Q memory address base to decode 
	 * (shifted right 16 bits - its in 64K units)
	 */
	*qdaddr = (u_short)((int)mapix >> 16);
	qdflags[unit].config = *(u_short *)qdaddr;

	/*
	 * load qdmap struct with the virtual addresses of the QDSS elements 
	 */
	qdbase[unit] = (caddr_t) (qvmem[0]);
	qdmap[unit].template = qdbase[unit] + TMPSTART;
	qdmap[unit].adder = qdbase[unit] + ADDER;
	qdmap[unit].dga = qdbase[unit] + DGA;
	qdmap[unit].duart = qdbase[unit] + DUART;
	qdmap[unit].memcsr = qdbase[unit] + MEMCSR;
	qdmap[unit].red = qdbase[unit] + RED;
	qdmap[unit].blue = qdbase[unit] + BLUE;
	qdmap[unit].green = qdbase[unit] + GREEN;

	qdflags[unit].duart_imask = 0;	/* init shadow variables */

	/*
	 * init the QDSS  
	 */
	/* 
	printf("qdbase[0] = %x, qdmap[0].memcsr = %x\n",
		(char *)qdbase[0], qdmap[0].memcsr);
	*/

	*(short *)qdmap[unit].memcsr |= SYNC_ON; /* once only: turn on sync */

	cursor[unit].x = 0;
	cursor[unit].y = 0;
	init_shared(unit);		/* init shared memory */
	setup_dragon(unit);		/* init the ADDER/VIPER stuff */
	clear_qd_screen(unit);		/* clear the screen */
	ldfont(unit);			/* load the console font */
	ldcursor(unit, cons_cursor);	/* load default cursor map */
	setup_input(unit);		/* init the DUART */
	v_putc = qdputc;		/* kernel console output to qdss */
	consops = &cdevsw[QDSSMAJOR];	/* virtual console is qdss */
	return 1;

} /* qdcons_init */

/*
 *  Configure QDSS into Q memory and make it intrpt.
 *
 *  side effects: QDSS gets mapped into Qbus memory space at the first
 *		 vacant 64kb boundary counting back from the top of
 *		 Qbus memory space (qvmem+4mb)
 *
 *  return: QDSS bus request level and vector address returned in
 *	   registers by UNIX convention.
 *
 */
qdprobe(reg)
	caddr_t reg;	/* character pointer to the QDSS I/O page register */
{
	register int br, cvec;  	/* value-result */
	register int unit;
	struct dga *dga;		/* pointer to gate array structure */
	int vector;
#ifdef notdef
	int *ptep;			/* page table entry pointer */
	caddr_t phys_adr;		/* physical QDSS base adrs */
	u_int mapix;
#endif

#ifdef lint
	br = 0; cvec = br; br = cvec; nNQD = br; br = nNQD;
	qddint(0); qdaint(0); qdiint(0); (void)qdgetc();
#endif

	/*
	 * calculate board unit number from I/O page register address  
	 */
	unit = (int) (((int)reg >> 1) & 0x0007);

	/*
	 * QDSS regs must be mapped to Qbus memory space at a 64kb
 	 * physical boundary.  The Qbus memory space is mapped into
	 * the system memory space at config time.  After config
	 * runs, "qvmem[0]" (ubavar.h) holds the system virtual adrs
	 * of the start of Qbus memory.   The Qbus memory page table
	 * is found via an array of pte ptrs called "QVmap[]" (ubavar.h)
	 * which is also loaded at config time.   These are the
	 * variables used below to find a vacant 64kb boundary in
	 * Qbus memory, and load it's corresponding physical adrs
	 * into the QDSS's I/O page CSR.  
	 */

	/*
	 * Only if QD is the graphics device.
	 */

	/* if this QDSS is NOT the console, then do init here.. */

	if (unit != 0) {
		printf("qd: can't support two qdss's (yet)\n");
#ifdef notdef	/* can't test */
		if (v_consputc != qdputc  ||  unit != 0) {

			/*
			* read QDSS config info 
			*/
			qdflags[unit].config = *(u_short *)reg;

			/*
			* find an empty 64kb adrs boundary 
			*/

			qdbase[unit] = (caddr_t) (qvmem[0] + QMEMSIZE - CHUNK);

			/*
			* find the cpusw entry that matches this machine. 
			*/
			cpup = &cpusw[cpu];
			while (!(BADADDR(qdbase[unit], sizeof(short))))
				qdbase[unit] -= CHUNK;

			/*
			* tell QDSS which Q memory address base to decode 
			*/
			mapix = (int) (VTOP(qdbase[unit]) - VTOP(qvmem[0]));
			ptep = (int *) QVmap[0] + mapix;
			phys_adr = (caddr_t)(((int)*ptep&0x001FFFFF)<<PGSHIFT);
			*(u_short *)reg = (u_short) ((int)phys_adr >> 16);

			/*
			* load QDSS adrs map with system addresses 
			* of device regs 
			*/
			qdmap[unit].template = qdbase[unit] + TMPSTART;
			qdmap[unit].adder = qdbase[unit] + ADDER;
			qdmap[unit].dga = qdbase[unit] + DGA;
			qdmap[unit].duart = qdbase[unit] + DUART;
			qdmap[unit].memcsr = qdbase[unit] + MEMCSR;
			qdmap[unit].red = qdbase[unit] + RED;
			qdmap[unit].blue = qdbase[unit] + BLUE;
			qdmap[unit].green = qdbase[unit] + GREEN;

			/* device init */

			cursor[unit].x = 0;
			cursor[unit].y = 0;
			init_shared(unit);		/* init shared memory */
			setup_dragon(unit); 	/* init the ADDER/VIPER stuff */
			ldcursor(unit, cons_cursor);	/* load default cursor map */
			setup_input(unit);		/* init the DUART */
			clear_qd_screen(unit);
			ldfont(unit);			/* load the console font */

			/* once only: turn on sync */

			*(short *)qdmap[unit].memcsr |= SYNC_ON;
		}
#endif /*notdef*/
	}

	/*
	* The QDSS interrupts at HEX vectors xx0 (DMA) xx4
	* (ADDER) and xx8 (DUART).  Therefore, we take three
	* vectors from the vector pool, and then continue
	* to take them until we get a xx0 HEX vector.  The
	* pool provides vectors in contiguous decending
	* order.  
	*/

	vector = (uba_hd[0].uh_lastiv -= 4*3);	/* take three vectors */

	while (vector & 0x0F) {		   /* if lo nibble != 0.. */
		/* ..take another vector */
		vector = (uba_hd[0].uh_lastiv -= 4);  
	}

	/*
	* setup DGA to do a DMA interrupt (transfer count = 0)	
	*/
	dga = (struct dga *) qdmap[unit].dga;
	dga->csr = (short) HALT;	/* disable everything */
	dga->ivr = (short) vector;	/* load intrpt base vector */
	dga->bytcnt_lo = (short) 0;	/* DMA xfer count = 0 */
	dga->bytcnt_hi = (short) 0;

	/* 
	* turn on DMA interrupts 
	*/
	dga->csr &= ~SET_DONE_FIFO;
	dga->csr |= DMA_IE | DL_ENB;

	DELAY(20000);			/* wait for the intrpt */
	dga->csr = HALT;		/* stop the wheels */

	if (cvec != vector)		/* if vector != base vector.. */
		return(0);		/* ..return = 'no device' */

	/*
	* score this as an existing qdss
	*/
	qdcount++;

	return(sizeof(short));	    /* return size of QDSS I/O page reg */

} /* qdprobe */

qdattach(ui)
	struct uba_device *ui;
{
	register unit;			/* QDSS module # for this call */

	unit = ui->ui_unit;		/* get QDSS number */

	/*
	* init "qdflags[]" for this QDSS 
	*/
	qdflags[unit].inuse = 0;	/* init inuse variable EARLY! */
	qdflags[unit].mapped = 0;
	qdflags[unit].kernel_loop = -1;
	qdflags[unit].user_dma = 0;
	qdflags[unit].curs_acc = ACC_OFF;
	qdflags[unit].curs_thr = 128;
	qdflags[unit].tab_res = 2;	/* default tablet resolution factor */
	qdflags[unit].duart_imask = 0;	/* init shadow variables */
	qdflags[unit].adder_ie = 0;

	/*
	* init structures used in kbd/mouse interrupt service.	This code must
	* come after the "init_shared()" routine has run since that routine 
	* inits the eq_header[unit] structure used here.   
	*/

	/*
	* init the "latest mouse report" structure 
	*/
	last_rep[unit].state = 0;
	last_rep[unit].dx = 0;
	last_rep[unit].dy = 0;
	last_rep[unit].bytcnt = 0;

	/*
	* init the event queue (except mouse position) 
	*/
	eq_header[unit]->header.events = 
	    (struct _vs_event *)((int)eq_header[unit] + sizeof(struct qdinput));

	eq_header[unit]->header.size = MAXEVENTS;
	eq_header[unit]->header.head = 0;
	eq_header[unit]->header.tail = 0;

	/*
	 * open exclusive for graphics device.
	 */
	qdopened[unit] = 0;

} /* qdattach */

/*ARGSUSED*/
qdopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct uba_device *ui; /* ptr to uba structures */
	register struct dga *dga;	/* ptr to gate array struct */
	register struct tty *tp;
	struct duart *duart;
	int unit;
	int minor_dev;

	minor_dev = minor(dev); /* get QDSS minor device number */
	unit = minor_dev >> 2;

	/*
	* check for illegal conditions	
	*/
	ui = qdinfo[unit];		/* get ptr to QDSS device struct */
	if (ui == 0  || ui->ui_alive == 0)
		return(ENXIO);		/* no such device or address */

	duart = (struct duart *) qdmap[unit].duart;
	dga = (struct dga *) qdmap[unit].dga;

	if ((minor_dev & 0x03) == 2) {
		/*
		* this is the graphic device... 
		*/
		if (qdopened[unit] != 0)
			return(EBUSY);
		else
			qdopened[unit] = 1;
		qdflags[unit].inuse |= GRAPHIC_DEV;  /* graphics dev is open */
		/*
		 * enble kbd & mouse intrpts in DUART mask reg 
		 */
		qdflags[unit].duart_imask |= 0x22;
		duart->imask = qdflags[unit].duart_imask;
	} else {
		/*
		* this is the console 
		*/
		qdflags[unit].inuse |= CONS_DEV;  /* mark console as open */
		dga->csr |= CURS_ENB;
		qdflags[unit].duart_imask |= 0x02;
		duart->imask = qdflags[unit].duart_imask;
		/*
		* some setup for tty handling 
		*/
		tp = &qd_tty[minor_dev];
		tp->t_addr = ui->ui_addr;
		tp->t_oproc = qdstart;
		if ((tp->t_state & TS_ISOPEN) == 0) {
			ttychars(tp);
			tp->t_flags = IFLAGS;
			tp->t_ispeed = B9600;
			tp->t_ospeed = B9600;
			tp->t_state = TS_ISOPEN | TS_CARR_ON;
#ifdef POSIXTTY
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
#endif
		}
		/*
		* enable intrpts, open line discipline 
		*/
		dga->csr |= GLOBAL_IE;	/* turn on the interrupts */
		return ((*linesw[tp->t_line].l_open)(dev, tp));
	}
	dga->csr |= GLOBAL_IE;	/* turn on the interrupts */
	return(0);

} /* qdopen */

/*ARGSUSED*/
qdclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	register struct qdmap *qd;
	register int *ptep;
	struct dga *dga;		/* gate array register map pointer */
	struct duart *duart;
	struct adder *adder;
	int unit;
	int minor_dev;
	u_int mapix;
	int i;				/* SIGNED index */

	minor_dev = minor(dev); 	/* get minor device number */
	unit = minor_dev >> 2;		/* get QDSS number */
	qd = &qdmap[unit];

	if ((minor_dev & 0x03) == 2) {
		/*
		* this is the graphic device... 
		*/
		if (qdopened[unit] != 1)
		    	return(EBUSY);
		else
			qdopened[unit] = 0;	/* allow it to be re-opened */
		/*
		* re-protect device memory 
		*/
		if (qdflags[unit].mapped & MAPDEV) {
			/*
			* TEMPLATE RAM 
			*/
			mapix = VTOP((int)qd->template) - VTOP(qvmem[0]);
			ptep = (int *)(QVmap[0] + mapix);
			for (i = 0; i < btop(TMPSIZE); i++, ptep++)
				*ptep = (*ptep & ~PG_PROT) | PG_V | PG_KW;
			/*
			* ADDER 
			*/
			mapix = VTOP((int)qd->adder) - VTOP(qvmem[0]);
			ptep = (int *)(QVmap[0] + mapix);
			for (i = 0; i < btop(REGSIZE); i++, ptep++)
				*ptep = (*ptep & ~PG_PROT) | PG_V | PG_KW;
			/*
			* COLOR MAPS 
			*/
			mapix = VTOP((int)qd->red) - VTOP(qvmem[0]);
			ptep = (int *)(QVmap[0] + mapix);
			for (i = 0; i < btop(CLRSIZE); i++, ptep++)
				*ptep = (*ptep & ~PG_PROT) | PG_V | PG_KW;
		}

		/*
		* re-protect DMA buffer and free the map registers 
		*/
		if (qdflags[unit].mapped & MAPDMA) {
			dga = (struct dga *) qdmap[unit].dga;
			adder = (struct adder *) qdmap[unit].adder;
			dga->csr &= ~DMA_IE;
			dga->csr &= ~0x0600;	     /* kill DMA */
			adder->command = CANCEL;
			/* 
			 * if DMA was running, flush spurious intrpt 
			 */
			if (dga->bytcnt_lo != 0) {
				dga->bytcnt_lo = 0;
				dga->bytcnt_hi = 0;
				DMA_SETIGNORE(DMAheader[unit]);
				dga->csr |= DMA_IE;
				dga->csr &= ~DMA_IE;
			}
			ptep = (int *)
			   ((VTOP(DMAheader[unit]*4)) + (mfpr(SBR)|0x80000000));
			for (i = 0; i < btop(DMAbuf_size); i++, ptep++)
				*ptep = (*ptep & ~PG_PROT) | PG_V | PG_KW;
			ubarelse(0, &Qbus_unmap[unit]);
		}

		/*
		* re-protect 1K (2 pages) event queue 
		*/
		if (qdflags[unit].mapped & MAPEQ) {
			ptep = (int *)
			   ((VTOP(eq_header[unit])*4) + (mfpr(SBR)|0x80000000));
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V; ptep++;
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V;
		}
		/*
		* re-protect scroll param area and disable scroll intrpts  
		*/
		if (qdflags[unit].mapped & MAPSCR) {
			ptep = (int *) ((VTOP(scroll[unit]) * 4)
				+ (mfpr(SBR) | 0x80000000));
			/*
			 * re-protect 512 scroll param area 
			 */
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V;
			adder = (struct adder *) qdmap[unit].adder;
			qdflags[unit].adder_ie &= ~FRAME_SYNC;
			adder->interrupt_enable = qdflags[unit].adder_ie;
		}
		/*
		* re-protect color map write buffer area and kill intrpts 
		*/
		if (qdflags[unit].mapped & MAPCOLOR) {
			ptep = (int *) ((VTOP(color_buf[unit]) * 4)
				+ (mfpr(SBR) | 0x80000000));
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V; ptep++;
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V;
			color_buf[unit]->status = 0;
			adder = (struct adder *) qdmap[unit].adder;
			qdflags[unit].adder_ie &= ~VSYNC;
			adder->interrupt_enable = qdflags[unit].adder_ie;
		}
		mtpr(TBIA, 0);		
		/* flag everything now unmapped */
		qdflags[unit].mapped = 0;   
		qdflags[unit].inuse &= ~GRAPHIC_DEV;
		qdflags[unit].curs_acc = ACC_OFF;
		qdflags[unit].curs_thr = 128;
		/*
		* restore the console 
		*/
		dga = (struct dga *) qdmap[unit].dga;
		adder = (struct adder *) qdmap[unit].adder;
		dga->csr &= ~DMA_IE;
		dga->csr &= ~0x0600;	/* halt the DMA! (just in case...) */
		dga->csr |= DMA_ERR;	/* clear error condition */
		adder->command = CANCEL;
		/*
		 * if DMA was running, flush spurious intrpt 
		 */
		if (dga->bytcnt_lo != 0) {
			dga->bytcnt_lo = 0;
			dga->bytcnt_hi = 0;
			DMA_SETIGNORE(DMAheader[unit]);
			dga->csr |= DMA_IE;
			dga->csr &= ~DMA_IE;
		}
		init_shared(unit);		/* init shared memory */
		setup_dragon(unit);		/* init ADDER/VIPER */
		ldcursor(unit, cons_cursor);	/* load default cursor map */
		setup_input(unit);		/* init the DUART */
		ldfont(unit);
		cursor[unit].x = 0;
		cursor[unit].y = 0;
		/*
		 * shut off the mouse rcv intrpt and turn on kbd intrpts 
		 */
		duart = (struct duart *) qdmap[unit].duart;
		qdflags[unit].duart_imask &= ~(0x20);
		qdflags[unit].duart_imask |= 0x02;
		duart->imask = qdflags[unit].duart_imask;
		/*
		* shut off interrupts if all is closed  
		*/
		if (!(qdflags[unit].inuse & CONS_DEV)) {
			dga = (struct dga *) qdmap[unit].dga;
			dga->csr &= ~(GLOBAL_IE | DMA_IE);
		}
	} else {
		/*
		* this is the console 
		*/
		tp = &qd_tty[minor_dev];
		(*linesw[tp->t_line].l_close)(tp);
		ttyclose(tp);
		tp->t_state = 0;
		qdflags[unit].inuse &= ~CONS_DEV;
		/*
		* if graphics device is closed, kill interrupts 
		*/
		if (!(qdflags[unit].inuse & GRAPHIC_DEV)) {
			dga = (struct dga *) qdmap[unit].dga;
			dga->csr &= ~(GLOBAL_IE | DMA_IE);
		}
	}

	return(0);

} /* qdclose */

qdioctl(dev, cmd, datap, flags)
	dev_t dev;
	int cmd;
	register caddr_t datap;
	int flags;
{
	register int *ptep;		/* page table entry pointer */
	register int mapix;		/* QVmap[] page table index */
	register struct _vs_event *event;
	register struct tty *tp;
	register i;
	struct qdmap *qd;		/* pointer to device map struct */
	struct dga *dga;		/* Gate Array reg structure pntr */
	struct duart *duart;		/* DUART reg structure pointer */
	struct adder *adder;		/* ADDER reg structure pointer */
	struct prgkbd *cmdbuf;
	struct prg_cursor *curs;
	struct _vs_cursor *pos;
	int unit = minor(dev) >> 2;	/* number of caller's QDSS */
	u_int minor_dev = minor(dev);
	int error;
	int s;
	short *temp;			/* a pointer to template RAM */

	/*
	* service graphic device ioctl commands 
	*/
	switch (cmd) {

	case QD_GETEVENT:
		/*
		* extract the oldest event from the event queue 
		*/
		if (ISEMPTY(eq_header[unit])) {
			event = (struct _vs_event *) datap;
			event->vse_device = VSE_NULL;
			break;
		}
		event = (struct _vs_event *) GETBEGIN(eq_header[unit]);
		s = spl5();
		GETEND(eq_header[unit]);
		splx(s);
		bcopy((caddr_t)event, datap, sizeof(struct _vs_event));
		break;

	case QD_RESET:
		/*
		* init the dragon stuff, DUART, and driver variables  
		*/
		init_shared(unit);		/* init shared memory */
		setup_dragon(unit);	      /* init the ADDER/VIPER stuff */
		clear_qd_screen(unit);
		ldcursor(unit, cons_cursor);	/* load default cursor map */
		ldfont(unit);			/* load the console font */
		setup_input(unit);		/* init the DUART */
		break;

	case QD_SET:
		/*
		* init the DUART and driver variables  
		*/
		init_shared(unit);
		setup_input(unit);
		break;

	case QD_CLRSCRN:
		/*
		* clear the QDSS screen.  (NOTE that this reinits the dragon) 
		*/
#ifdef notdef	/* has caused problems and isn't necessary */
		setup_dragon(unit);
		clear_qd_screen(unit);
#endif
		break;

	case QD_WTCURSOR:
		/*
		* load a cursor into template RAM  
		*/
		ldcursor(unit, (short *)datap);
		break;

	case QD_RDCURSOR:

		temp = (short *) qdmap[unit].template;
		/*
		 * cursor is 32 WORDS from the end of the 8k WORD...
		 *  ...template space 
		 */
		temp += (8 * 1024) - 32;
		for (i = 0; i < 32; ++i, datap += sizeof(short))
			*(short *)datap = *temp++;
		break;

	case QD_POSCURSOR:
		/*
		* position the mouse cursor  
		*/
		dga = (struct dga *) qdmap[unit].dga;
		pos = (struct _vs_cursor *) datap;
		s = spl5();
		dga->x_cursor = TRANX(pos->x);
		dga->y_cursor = TRANY(pos->y);
		eq_header[unit]->curs_pos.x = pos->x;
		eq_header[unit]->curs_pos.y = pos->y;
		splx(s);
		break;

	case QD_PRGCURSOR:
		/*
		* set the cursor acceleration factor 
		*/
		curs = (struct prg_cursor *) datap;
		s = spl5();
		qdflags[unit].curs_acc = curs->acc_factor;
		qdflags[unit].curs_thr = curs->threshold;
		splx(s);
		break;

	case QD_MAPDEVICE:
		/*
		* enable 'user write' to device pages 
		*/
		qdflags[unit].mapped |= MAPDEV;
		qd = (struct qdmap *) &qdmap[unit];
		/*
		* enable user write to template RAM 
		*/
		mapix = VTOP((int)qd->template) - VTOP(qvmem[0]);
		ptep = (int *)(QVmap[0] + mapix);
		for (i = 0; i < btop(TMPSIZE); i++, ptep++)
			*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V;
		/*
		* enable user write to registers 
		*/
		mapix = VTOP((int)qd->adder) - VTOP(qvmem[0]);
		ptep = (int *)(QVmap[0] + mapix);
		for (i = 0; i < btop(REGSIZE); i++, ptep++)
			*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V;
		/*
		* enable user write to color maps 
		*/
		mapix = VTOP((int)qd->red) - VTOP(qvmem[0]);
		ptep = (int *)(QVmap[0] + mapix);
		for (i = 0; i < btop(CLRSIZE); i++, ptep++)
			*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V;
		/*
		* enable user write to DUART 
		*/
		mapix = VTOP((int)qd->duart) - VTOP(qvmem[0]);
		ptep = (int *)(QVmap[0] + mapix);
		*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V; /* duart page */

		mtpr(TBIA, 0);		/* invalidate translation buffer */

		/*
		 * stuff qdmap structure in return buffer 
		 */
		bcopy((caddr_t)qd, datap, sizeof(struct qdmap));
		break;

	case QD_MAPIOBUF:
		/*
		 * do setup for DMA by user process	
		 *
		 * set 'user write enable' bits for DMA buffer  
		 */
		qdflags[unit].mapped |= MAPDMA;
		ptep = (int *) ((VTOP(DMAheader[unit]) * 4)
			+ (mfpr(SBR) | 0x80000000));
		for (i = 0; i < btop(DMAbuf_size); i++, ptep++)
			*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V;
		mtpr(TBIA, 0);		/* invalidate translation buffer */
		/*
		* set up QBUS map registers for DMA 
		*/
		DMAheader[unit]->QBAreg =
		    uballoc(0, (caddr_t)DMAheader[unit], DMAbuf_size, 0);
		if (DMAheader[unit]->QBAreg == 0)
		    printf("qd%d: qdioctl: QBA setup error\n", unit);
		Qbus_unmap[unit] = DMAheader[unit]->QBAreg;
		DMAheader[unit]->QBAreg &= 0x3FFFF;
		/*
		* return I/O buf adr 
		*/
		*(int *)datap = (int) DMAheader[unit];
		break;

	case QD_MAPSCROLL:
		/*
		* map the shared scroll param area and enable scroll interpts  
		*/
		qdflags[unit].mapped |= MAPSCR;
		ptep = (int *) ((VTOP(scroll[unit]) * 4)
			+ (mfpr(SBR) | 0x80000000));
		/*
		 * allow user write to scroll area 
		 */
		*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V;
		mtpr(TBIA, 0);			/* invalidate translation buf */
		scroll[unit]->status = 0;
		adder = (struct adder *) qdmap[unit].adder;
		qdflags[unit].adder_ie |= FRAME_SYNC;
		adder->interrupt_enable = qdflags[unit].adder_ie;
		*(int *)datap = (int) scroll[unit]; /* return scroll area */
		break;

	case QD_UNMAPSCROLL:
		/*
		* unmap shared scroll param area and disable scroll intrpts 
		*/
		if (qdflags[unit].mapped & MAPSCR) {
			qdflags[unit].mapped &= ~MAPSCR;
			ptep = (int *) ((VTOP(scroll[unit]) * 4)
				+ (mfpr(SBR) | 0x80000000));
			/*
			 * re-protect 512 scroll param area 
			 */
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V;
			mtpr(TBIA, 0);	/* smash CPU's translation buf */
			adder = (struct adder *) qdmap[unit].adder;
			qdflags[unit].adder_ie &= ~FRAME_SYNC;
			adder->interrupt_enable = qdflags[unit].adder_ie;
		}
		break;

	case QD_MAPCOLOR:
		/*
		* map shared color map write buf and turn on vsync intrpt 
		*/
		qdflags[unit].mapped |= MAPCOLOR;
		ptep = (int *) ((VTOP(color_buf[unit]) * 4)
			+ (mfpr(SBR) | 0x80000000));
		/*
		 * allow user write to color map write buffer 
		 */
		*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V; ptep++;
		*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V;
		mtpr(TBIA, 0);			/* clr CPU translation buf */
		adder = (struct adder *) qdmap[unit].adder;
		qdflags[unit].adder_ie |= VSYNC;
		adder->interrupt_enable = qdflags[unit].adder_ie;
		/*
		 * return color area address 
		 */
		*(int *)datap = (int) color_buf[unit];
		break;

	case QD_UNMAPCOLOR:
		/*
		 * unmap shared color map write buffer and kill VSYNC intrpts 
		 */
		if (qdflags[unit].mapped & MAPCOLOR) {
			qdflags[unit].mapped &= ~MAPCOLOR;
			ptep = (int *) ((VTOP(color_buf[unit]) * 4)
				+ (mfpr(SBR) | 0x80000000));
			/*
			 * re-protect color map write buffer 
			 */
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V; ptep++;
			*ptep = (*ptep & ~PG_PROT) | PG_KW | PG_V;
			mtpr(TBIA, 0);
			adder = (struct adder *) qdmap[unit].adder;
			qdflags[unit].adder_ie &= ~VSYNC;
			adder->interrupt_enable = qdflags[unit].adder_ie;
		}
		break;

	case QD_MAPEVENT:
		/*
		* give user write access to the event queue 
		*/
		qdflags[unit].mapped |= MAPEQ;
		ptep = (int *) ((VTOP(eq_header[unit]) * 4)
			+ (mfpr(SBR) | 0x80000000));
		/*
		 * allow user write to 1K event queue 
		 */
		*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V; ptep++;
		*ptep = (*ptep & ~PG_PROT) | PG_UW | PG_V;
		mtpr(TBIA, 0);			/* clr CPU translation buf */
		/*
		 * return event queue address 
		 */
		*(int *)datap = (int)eq_header[unit];
		break;

	case QD_PRGKBD:
		/*
		* pass caller's programming commands to LK201 
		*/
		duart = (struct duart *)qdmap[unit].duart;
		cmdbuf = (struct prgkbd *)datap;    /* pnt to kbd cmd buf */
		/*
		* send command 
		*/
		for (i = 1000; i > 0; --i) {
			if (duart->statusA&XMT_RDY) {
				duart->dataA = cmdbuf->cmd;
				break;
			}
		}
		if (i == 0) {
			printf("qd%d: qdioctl: timeout on XMT_RDY [1]\n", unit);
			break;
		}
		/*
		* send param1? 
		*/
		if (cmdbuf->cmd & LAST_PARAM)
			break;
		for (i = 1000; i > 0; --i) {
			if (duart->statusA&XMT_RDY) {
				duart->dataA = cmdbuf->param1;
				break;
			}
		}
		if (i == 0) {
			printf("qd%d: qdioctl: timeout on XMT_RDY [2]\n", unit);
			break;
		}
		/*
		* send param2? 
		*/
		if (cmdbuf->param1 & LAST_PARAM)
		    break;
		for (i = 1000; i > 0; --i) {
			if (duart->statusA&XMT_RDY) {
				duart->dataA = cmdbuf->param2;
				break;
			}
		}
		if (i == 0) {
			printf("qd%d: qdioctl: timeout on XMT_RDY [3]\n", unit);
			break;
		}
		break;

	case QD_PRGMOUSE:
		/*
		* pass caller's programming commands to the mouse  
		*/
		duart = (struct duart *) qdmap[unit].duart;
		for (i = 1000; i > 0; --i) {
			if (duart->statusB&XMT_RDY) {
				duart->dataB = *datap;
				break;
			}
		}
		if (i == 0) {
			printf("qd%d: qdioctl: timeout on XMT_RDY [4]\n", unit);
		}
		break;

	case QD_RDCONFIG:
		/*
		* get QDSS configuration word and return it  
		*/
		*(short *)datap = qdflags[unit].config;
		break;

	case QD_KERN_LOOP:
	case QD_KERN_UNLOOP:
		/*
		 * vestige from ultrix.  BSD uses TIOCCONS to redirect
		 * kernel console output.
		 */
		break;

	case QD_PRGTABLET:
		/*
		* program the tablet 
		*/
		duart = (struct duart *) qdmap[unit].duart;
		for (i = 1000; i > 0; --i) {
			if (duart->statusB&XMT_RDY) {
				duart->dataB = *datap;
				break;
			}
		}
		if (i == 0) {
			printf("qd%d: qdioctl: timeout on XMT_RDY [5]\n", unit);
		}
		break;

	case QD_PRGTABRES:
		/*
		* program the tablet report resolution factor 
		*/
		qdflags[unit].tab_res = *(short *)datap;
		break;

	default:
		/*
		* service tty ioctl's  
		*/
		if (!(minor_dev & 0x02)) {
			tp = &qd_tty[minor_dev];
			error = 
			   (*linesw[tp->t_line].l_ioctl)(tp, cmd, datap, flags);
			if (error >= 0) {
				return(error);
			}
			error = ttioctl(tp, cmd, datap, flags);
			if (error >= 0) {
				return(error);
			}
		}
		break;
	}

	return(0);

} /* qdioctl */

qdselect(dev, rw)
	dev_t dev;
	int rw;
{
	register s;
	register unit;
	register struct tty *tp;
	u_int minor_dev = minor(dev);

	s = spl5();
	unit = minor_dev >> 2;

	switch (rw) {
	case FREAD:
		if ((minor_dev & 0x03) == 2) {
			/*
			* this is a graphics device, so check for events
			*/
			if(!(ISEMPTY(eq_header[unit]))) {
				splx(s);
				return(1);
			}
			qdrsel[unit] = u.u_procp;
			qdflags[unit].selmask |= SEL_READ;
			splx(s);
			return(0);
		} else {
			/*
			* this is a tty device
			*/
			tp = &qd_tty[minor_dev];
			if (ttnread(tp))
			    return(1);
			tp->t_rsel = u.u_procp;
			splx(s);
			return(0);
		}

	case FWRITE:
		if ((minor(dev) & 0x03) == 2) {
			/*
			* this is a graphics device, so check for dma buffers
			*/
			if (DMA_ISEMPTY(DMAheader[unit]))
			    {
				splx(s);
				return(1);
			}
			qdrsel[unit] = u.u_procp;
			qdflags[unit].selmask |= SEL_WRITE;
			splx(s);
			return(0);
		} else {
			/*
			* this is a tty device
			*/
			tp = &qd_tty[minor_dev];
			if (tp->t_outq.c_cc <= TTLOWAT(tp))
			    return(1);
			tp->t_wsel = u.u_procp;
			splx(s);
			return(0);
		}
	}
	splx(s);
	return(0);

} /* qdselect() */

extern qd_strategy();

qdwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;
	register minor_dev;
	register unit;

	minor_dev = minor(dev);
	unit = (minor_dev >> 2) & 0x07;

	if (((minor_dev&0x03) != 0x02) && (qdflags[unit].inuse&CONS_DEV)) {
		/*
		* this is the console...  
		*/
		tp = &qd_tty[minor_dev];
		return ((*linesw[tp->t_line].l_write)(tp, uio));
	} else if (qdflags[unit].inuse & GRAPHIC_DEV) {
		/*
		* this is a DMA xfer from user space 
		*/
		return (physio(qd_strategy, &qdbuf[unit],
		dev, B_WRITE, minphys, uio));
	}
	return (ENXIO);
}

qdread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;
	register minor_dev;
	register unit;

	minor_dev = minor(dev);
	unit = (minor_dev >> 2) & 0x07;

	if ((minor_dev & 0x03) != 0x02 && qdflags[unit].inuse & CONS_DEV) {
		/*
		* this is the console
		*/
		tp = &qd_tty[minor_dev];
		return ((*linesw[tp->t_line].l_read)(tp, uio));
	} else if (qdflags[unit].inuse & GRAPHIC_DEV) {
		/*
		* this is a bitmap-to-processor xfer 
		*/
		return (physio(qd_strategy, &qdbuf[unit],
		dev, B_READ, minphys, uio));
	}
	return (ENXIO);
}

/***************************************************************
*
*	qd_strategy()... strategy routine to do DMA
*
***************************************************************/

qd_strategy(bp)
	register struct buf *bp;
{
	register struct dga *dga;
	register struct adder *adder;
	register unit;
	int QBAreg;
	int s;
	int cookie;

	unit = (minor(bp->b_dev) >> 2) & 0x07;

	/*
	* init pointers 
	*/
	if ((QBAreg = ubasetup(0, bp, 0)) == 0) {
		printf("qd%d: qd_strategy: QBA setup error\n", unit);
		goto STRAT_ERR;
	}
	dga = (struct dga *) qdmap[unit].dga;
	s = spl5();
	qdflags[unit].user_dma = -1;
	dga->csr |= DMA_IE;
	cookie = QBAreg & 0x3FFFF;
	dga->adrs_lo = (short) cookie;
	dga->adrs_hi = (short) (cookie >> 16);
	dga->bytcnt_lo = (short) bp->b_bcount;
	dga->bytcnt_hi = (short) (bp->b_bcount >> 16);

	while (qdflags[unit].user_dma) {
		sleep((caddr_t)&qdflags[unit].user_dma, QDPRIOR);
	}
	splx(s);
	ubarelse(0, &QBAreg);
	if (!(dga->csr & DMA_ERR)) {
		iodone(bp);
		return;
	}

STRAT_ERR:
	adder = (struct adder *) qdmap[unit].adder;
	adder->command = CANCEL;	/* cancel adder activity */
	dga->csr &= ~DMA_IE;
	dga->csr &= ~0x0600;		/* halt DMA (reset fifo) */
	dga->csr |= DMA_ERR;		/* clear error condition */
	bp->b_flags |= B_ERROR; 	/* flag an error to physio() */

	/*
	 * if DMA was running, flush spurious intrpt 
	 */
	if (dga->bytcnt_lo != 0) {
		dga->bytcnt_lo = 0;
		dga->bytcnt_hi = 0;
		DMA_SETIGNORE(DMAheader[unit]);
		dga->csr |= DMA_IE;
	}
	iodone(bp);

} /* qd_strategy */

/*
 *  Start output to the console screen
 */
qdstart(tp)
	register struct tty *tp;
{
	register which_unit, unit, c;
	int s;

	unit = minor(tp->t_dev);
	which_unit = (unit >> 2) & 0x3;
	unit &= 0x03;

	s = spl5();

	/*
	* If it's currently active, or delaying, no need to do anything. 
	*/
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;

	/*
	* Display chars until the queue is empty.
	* Drop input from anything but the console
	* device on the floor.	
	*
	* XXX - this loop is done at spltty.
	*
	*/
	while (tp->t_outq.c_cc) {
		c = getc(&tp->t_outq);
		if (unit == 0)
			blitc(which_unit, (u_char)c);
	}
	/*
	* If there are sleepers, and output has drained below low
	* water mark, wake up the sleepers. 
	*/
	if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
		if (tp->t_state & TS_ASLEEP){
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t) &tp->t_outq);
		}
	}

	tp->t_state &= ~TS_BUSY;

out:
	splx(s);

} /* qdstart */

/*ARGSUSED*/
qdstop(tp, flag)
	register struct tty *tp;
	int flag;
{
	register int s;

	s = spl5();	/* block intrpts during state modification */
	if (tp->t_state & TS_BUSY)
		if ((tp->t_state & TS_TTSTOP) == 0)
			tp->t_state |= TS_FLUSH;
		else
			tp->t_state &= ~TS_BUSY;
	splx(s);
}

/*
 *  Output a character to the QDSS screen
 */

blitc(unit, chr)
	register unit;
	register u_char chr;
{
	register struct adder *adder;
	register struct dga *dga;
	register int i;
	int nograph = !(qdflags[unit].inuse&GRAPHIC_DEV);
	static short inescape[NQD];

	adder = (struct adder *)qdmap[unit].adder;
	dga = (struct dga *) qdmap[unit].dga;
	/* 
	 * BSD comment: this (&=0177) defeats the extended character 
	 * set code for the glass tty, but if i had the time i would 
	 * spend it ripping out the code completely.  This driver
	 * is too big for its own good.
	 */
	chr &= 0177;
	/*
	 * Cursor addressing (so vi will work).
	 * Decode for "\E=%.%." cursor motion description.
	 * Corresponds to type "qdcons" in /etc/termcap:
	 *
	 *    qd|qdss|qdcons|qdss glass tty (4.4 BSD):\
	 *      :am:do=^J:le=^H:bs:cm=\E=%.%.:cl=1^Z:co#128:li#57::nd=^L:up=^K:
	 *
	 */
	if (inescape[unit] && nograph) {	
		switch (inescape[unit]++) {
		case 1:
			if (chr != '=') {
				/* abort escape sequence */
				inescape[unit] = 0;
				blitc(unit, chr);
			}
			return;
		case 2:
			/* position row */
			cursor[unit].y = CHAR_HEIGHT * chr;
			if (cursor[unit].y > 863 - CHAR_HEIGHT)
				cursor[unit].y = 863 - CHAR_HEIGHT;
			dga->y_cursor = TRANY(cursor[unit].y);
			return;
		case 3:
			/* position column */
			cursor[unit].x = CHAR_WIDTH * chr;
			if (cursor[unit].x > 1024 - CHAR_WIDTH)
				cursor[unit].x = 1023 - CHAR_WIDTH;
			dga->x_cursor = TRANX(cursor[unit].x);
			inescape[unit] = 0;
			return;
		default:
			inescape[unit] = 0;
			blitc(unit, chr);
		}
	}

	switch (chr) {
	case '\r':			/* return char */
		cursor[unit].x = 0;
		if (nograph)
			dga->x_cursor = TRANX(cursor[unit].x);
		return;

	case '\t':			/* tab char */
		for (i = 8 - ((cursor[unit].x >> 3) & 0x07); i > 0; --i) {
			blitc(unit, ' ');
		}
		return;

	case '\n':			/* line feed char */
		if ((cursor[unit].y += CHAR_HEIGHT) > (863 - CHAR_HEIGHT)) {
			if (nograph) {
				cursor[unit].y -= CHAR_HEIGHT;
				scroll_up(adder);
			} else
				cursor[unit].y = 0;
		}
		if (nograph)
			dga->y_cursor = TRANY(cursor[unit].y);
		return;

	case '\b':			/* backspace char */
		if (cursor[unit].x > 0) {
			cursor[unit].x -= CHAR_WIDTH;
			if (nograph)
				dga->x_cursor = TRANX(cursor[unit].x);
		}
		return;
	case CTRL('k'):		/* cursor up */
		if (nograph && cursor[unit].y > 0) {
			cursor[unit].y -= CHAR_HEIGHT;
			dga->y_cursor = TRANY(cursor[unit].y);
		}
		return;

	case CTRL('^'):		/* home cursor */
		if (nograph) {
			cursor[unit].x = 0;
			dga->x_cursor = TRANX(cursor[unit].x);
			cursor[unit].y = 0;
			dga->y_cursor = TRANY(cursor[unit].y);
		}
		return;

	case CTRL('l'):		/* cursor right */
		if (nograph && cursor[unit].x < 1023 - CHAR_WIDTH) {
			cursor[unit].x += CHAR_WIDTH;
			dga->x_cursor = TRANX(cursor[unit].x);
		}
		return;

	case CTRL('z'):		/* clear screen */
		if (nograph) {
			setup_dragon(unit);  	
			clear_qd_screen(unit);
			/* home cursor - termcap seems to assume this */
			cursor[unit].x = 0;
			dga->x_cursor = TRANX(cursor[unit].x);
			cursor[unit].y = 0;
			dga->y_cursor = TRANY(cursor[unit].y);
		}
		return;

	case '\033':		/* start escape sequence */
		if (nograph)
			inescape[unit] = 1;
		return;

	default:
		if ((chr < ' ') || (chr > '~'))
			return;
	}
	/*
	 * setup VIPER operand control registers  
	 */
	write_ID(adder, CS_UPDATE_MASK, 0x0001);  /* select plane #0 */
	write_ID(adder, SRC1_OCR_B,
	EXT_NONE | INT_SOURCE | ID | BAR_SHIFT_DELAY);
	write_ID(adder, CS_UPDATE_MASK, 0x00FE);  /* select other planes */
	write_ID(adder, SRC1_OCR_B,
	EXT_SOURCE | INT_NONE | NO_ID | BAR_SHIFT_DELAY);
	write_ID(adder, CS_UPDATE_MASK, 0x00FF);  /* select all planes */
	write_ID(adder, DST_OCR_B,
	EXT_NONE | INT_NONE | NO_ID | NO_BAR_SHIFT_DELAY);
	write_ID(adder, MASK_1, 0xFFFF);
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 1);
	write_ID(adder, VIPER_Z_LOAD | BACKGROUND_COLOR_Z, 0);
	adder->x_clip_min = 0;
	adder->x_clip_max = 1024;
	adder->y_clip_min = 0;
	adder->y_clip_max = 864;
	/*
	 * load DESTINATION origin and vectors  
	 */
	adder->fast_dest_dy = 0;
	adder->slow_dest_dx = 0;
	adder->error_1 = 0;
	adder->error_2 = 0;
	adder->rasterop_mode = DST_WRITE_ENABLE | NORMAL;
	(void)wait_status(adder, RASTEROP_COMPLETE);
	adder->destination_x = cursor[unit].x;
	adder->fast_dest_dx = CHAR_WIDTH;
	adder->destination_y = cursor[unit].y;
	adder->slow_dest_dy = CHAR_HEIGHT;
	/*
	 * load SOURCE origin and vectors  
	 */
	if ((chr - ' ') > (CHARS - 1))  {
		printf("Invalid character (x)%x in blitc\n",chr);
		chr = ' ';
	}
	/*
	 * X position is modulo the number of characters per line 
	 */
	adder->source_1_x = FONT_X + 
	    (((chr - ' ') % (MAX_SCREEN_X/CHAR_WIDTH)) * CHAR_WIDTH);
	/*
	 * Point to either first or second row 
	 */
	adder->source_1_y = 2048 - 15 * 
	    (((chr - ' ')/(MAX_SCREEN_X/CHAR_WIDTH)) + 1);
	adder->source_1_dx = CHAR_WIDTH;
	adder->source_1_dy = CHAR_HEIGHT;
	write_ID(adder, LU_FUNCTION_R1, FULL_SRC_RESOLUTION | LF_SOURCE);
	adder->cmd = RASTEROP | OCRB | 0 | S1E | DTE;
	/*
	 * update console cursor coordinates 
	 */
	cursor[unit].x += CHAR_WIDTH;
	if (nograph)
		dga->x_cursor = TRANX(cursor[unit].x);
	if (cursor[unit].x > (1024 - CHAR_WIDTH)) {
		blitc(unit, '\r');
		blitc(unit, '\n');
	}

} /* blitc */

qdreset() { }

/*
 *  INTERRUPT SERVICE ROUTINES
 */

/*
 *  Service "DMA DONE" interrupt condition
 */
qddint(qd)
	register qd;
{
	register struct DMAreq_header *header;
	register struct DMAreq *request;
	register struct dga *dga;
	struct adder *adder;
	int cookie;			/* DMA adrs for QDSS */

	(void)spl4(); 			/* allow interval timer in */

	/*
	* init pointers 
	*/
	header = DMAheader[qd]; 	    /* register for optimization */
	dga = (struct dga *) qdmap[qd].dga;
	adder = (struct adder *) qdmap[qd].adder;

	/*
	* if this interrupt flagged as bogus for interrupt flushing purposes.. 
	*/
	if (DMA_ISIGNORE(header)) {
		DMA_CLRIGNORE(header);
		return;
	}

	/*
	* dump a DMA hardware error message if appropriate
	*/
	if (dga->csr & DMA_ERR) {

		if (dga->csr & PARITY_ERR)
		    printf("qd%d: qddint: DMA hardware parity fault.\n", qd);

		if (dga->csr & BUS_ERR)
		    printf("qd%d: qddint: DMA hardware bus error.\n", qd);
	}

	/*
	* if this was a DMA from user space... 
	*/
	if (qdflags[qd].user_dma) {
		qdflags[qd].user_dma = 0;
		wakeup((caddr_t)&qdflags[qd].user_dma);
		return;
	}

	/*
	* if we're doing DMA request queue services, field the error condition 
	*/
	if (dga->csr & DMA_ERR) {

		dga->csr &= ~0x0600;		/* halt DMA (reset fifo) */
		dga->csr |= DMA_ERR;		/* clear error condition */
		adder->command = CANCEL;	/* cancel adder activity */

		DMA_SETERROR(header);	/* flag error in header status word */
		DMA_CLRACTIVE(header);
		header->DMAreq[header->oldest].DMAdone |= HARD_ERROR;
		header->newest = header->oldest;
		header->used = 0;

		if (qdrsel[qd] && qdflags[qd].selmask & SEL_WRITE) {
			selwakeup(qdrsel[qd], 0);
			qdrsel[qd] = 0;
			qdflags[qd].selmask &= ~SEL_WRITE;
		}

		if (dga->bytcnt_lo != 0) {
			dga->bytcnt_lo = 0;
			dga->bytcnt_hi = 0;
			DMA_SETIGNORE(header);
		}
		return;
	}

	/*
	* if the DMA request queue is now becoming non-full, 
	* wakeup "select" client.
	*/
	if (DMA_ISFULL(header)) {
		if (qdrsel[qd] && qdflags[qd].selmask & SEL_WRITE) {
			selwakeup(qdrsel[qd], 0);
			qdrsel[qd] = 0;
			qdflags[qd].selmask &= ~SEL_WRITE;
		}
	}

	header->DMAreq[header->oldest].DMAdone |= REQUEST_DONE;
	QDlast_DMAtype = header->DMAreq[header->oldest].DMAtype;

	/* check for unexpected interrupt */
	if (DMA_ISEMPTY(header))
	    return;

	DMA_GETEND(header);	/* update request queue indices */

	/*
	* if no more DMA pending, wake up "select" client and exit 
	*/
	if (DMA_ISEMPTY(header)) {

		if (qdrsel[qd] && qdflags[qd].selmask & SEL_WRITE) {
			selwakeup(qdrsel[qd], 0);
			qdrsel[qd] = 0;
			qdflags[qd].selmask &= ~SEL_WRITE;
		}

		DMA_CLRACTIVE(header);  /* flag DMA done */
		return;
	}

	/*
	* initiate next DMA xfer  
	*/
	request = DMA_GETBEGIN(header);
	if (request->DMAtype != QDlast_DMAtype) {
		dga->csr &= ~0x0600;	  /* halt DMA (reset fifo) */
		adder->command = CANCEL;  /* cancel adder activity */
	}


	switch (request->DMAtype) {

	case DISPLIST:
		if (request->DMAtype != QDlast_DMAtype) {
			dga->csr |= DL_ENB;
			dga->csr &= ~(BTOP_ENB | BYTE_DMA);
		}
		break;

	case PTOB:
		if (request->DMAtype != QDlast_DMAtype) {
			if (request->DMAdone & BYTE_PACK)
			    dga->csr |= (PTOB_ENB | BYTE_DMA);
			else {
				dga->csr |= PTOB_ENB;
				dga->csr &= ~BYTE_DMA;
			}
		}
		break;

	case BTOP:
		if (request->DMAtype != QDlast_DMAtype) {
			if (request->DMAdone & BYTE_PACK) {
				dga->csr &= ~DL_ENB;
				dga->csr |= (BTOP_ENB | BYTE_DMA);
			}
			else {
				dga->csr |= BTOP_ENB;
				dga->csr &= ~(BYTE_DMA | DL_ENB);
			}
		}
		break;
	default:
		printf("qd%d: qddint: illegal DMAtype parameter.\n", qd);
		DMA_CLRACTIVE(header);	/* flag DMA done */
		return;
	}

	if (request->DMAdone & COUNT_ZERO) {
		dga->csr &= ~SET_DONE_FIFO;
	} 
	else if (request->DMAdone & FIFO_EMPTY) {
		dga->csr |= SET_DONE_FIFO;
	}

	if (request->DMAdone & WORD_PACK)
	    dga->csr &= ~BYTE_DMA;
	else if (request->DMAdone & BYTE_PACK)
	    dga->csr |= BYTE_DMA;

	dga->csr |= DMA_IE;
	QDlast_DMAtype = request->DMAtype;

	cookie = ((int)request->bufp - (int)header) + (int)header->QBAreg;

	dga->adrs_lo = (short) cookie;
	dga->adrs_hi = (short) (cookie >> 16);

	dga->bytcnt_lo = (short) request->length;
	dga->bytcnt_hi = (short) (request->length >> 16);

	return;
}

/*
 * ADDER interrupt service routine
 */
qdaint(qd)
	register qd;
{
	register struct adder *adder;
	struct color_buf *cbuf;
	int i;
	register struct rgb *rgbp;
	register short *red;
	register short *green;
	register short *blue;

	(void)spl4(); 			/* allow interval timer in */

	adder = (struct adder *) qdmap[qd].adder;

	/*
	* service the vertical blank interrupt (VSYNC bit) by loading 
	* any pending color map load request  
	*/
	if (adder->status & VSYNC) {
		adder->status &= ~VSYNC;	/* clear the interrupt */
		cbuf = color_buf[qd];
		if (cbuf->status & LOAD_COLOR_MAP) {

			red = (short *) qdmap[qd].red;
			green = (short *) qdmap[qd].green;
			blue = (short *) qdmap[qd].blue;

			for (i = cbuf->count, rgbp = cbuf->rgb;
			     --i >= 0; rgbp++) {
				red[rgbp->offset] = (short) rgbp->red;
				green[rgbp->offset] = (short) rgbp->green;
				blue[rgbp->offset] = (short) rgbp->blue;
			}

			cbuf->status &= ~LOAD_COLOR_MAP;
		}
	}

	/*
	* service the scroll interrupt (FRAME_SYNC bit) 
	*/
	if (adder->status & FRAME_SYNC) {
		adder->status &= ~FRAME_SYNC;	/* clear the interrupt */

		if (scroll[qd]->status & LOAD_REGS) {

			for (i = 1000, adder->status = 0; i > 0 && 
			     !(adder->status&ID_SCROLL_READY); --i)
			      ;

			if (i == 0) {
			    printf("qd%d: qdaint: timeout on ID_SCROLL_READY\n",
				qd);
				return;
			}

			adder->ID_scroll_data = scroll[qd]->viper_constant;
			adder->ID_scroll_command = ID_LOAD | SCROLL_CONSTANT;

			adder->y_scroll_constant =
				scroll[qd]->y_scroll_constant;
			adder->y_offset_pending = scroll[qd]->y_offset;

			if (scroll[qd]->status & LOAD_INDEX) {

				adder->x_index_pending = 
					scroll[qd]->x_index_pending;
				adder->y_index_pending = 
					scroll[qd]->y_index_pending;
			}

			scroll[qd]->status = 0x00;
		}
	}
}

/*
 *  DUART input interrupt service routine
 *
 *  XXX - this routine should be broken out - it is essentially
 *	      straight line code.
 */

qdiint(qd)
	register qd;
{
	register struct _vs_event *event;
	register struct qdinput *eqh;
	struct dga *dga;
	struct duart *duart;
	struct mouse_report *new_rep;
	struct uba_device *ui;
	struct tty *tp;
	u_short chr;
	u_short status;
	u_short data;
	u_short key;
	char do_wakeup = 0;		/* flag to do a select wakeup call */
	char a, b, c;			/* mouse button test variables */

	(void)spl4(); 			/* allow interval timer in */

	eqh = eq_header[qd];		/* optimized as a register */
	new_rep = &current_rep[qd];
	duart = (struct duart *) qdmap[qd].duart;

	/*
	* if the graphic device is turned on..	
	*/
	if (qdflags[qd].inuse & GRAPHIC_DEV) {
		/*
		* empty DUART 
		*/
		while (duart->statusA&RCV_RDY || duart->statusB&RCV_RDY) {
			/*
			 * pick up LK-201 input (if any) 
			 */
			if (duart->statusA&RCV_RDY) {

				/* if error condition, then reset it */

				if (duart->statusA&0x70) {
					duart->cmdA = 0x40;
					continue;
				}

				/* event queue full now? (overflow condition) */

				if (ISFULL(eqh) == TRUE) {
					printf(
					 "qd%d: qdiint: event queue overflow\n",
					   qd);
					break;
				}

				/*
				* Check for various keyboard errors  */

				key = duart->dataA & 0xFF;

				if (key==LK_POWER_ERROR ||
				    key==LK_KDOWN_ERROR ||
				    key == LK_INPUT_ERROR || 
				    key == LK_OUTPUT_ERROR) {
					printf(
				    "qd%d: qdiint: keyboard error, code = %x\n",
					qd,key);
					return;
				}

				if (key < LK_LOWEST)
				    return;

				++do_wakeup;  /* request a select wakeup call */

				event = PUTBEGIN(eqh);
				PUTEND(eqh);

				event->vse_key = key;
				event->vse_key &= 0x00FF;
				event->vse_x = eqh->curs_pos.x;
				event->vse_y = eqh->curs_pos.y;
				event->vse_time = TOY;
				event->vse_type = VSE_BUTTON;
				event->vse_direction = VSE_KBTRAW;
				event->vse_device = VSE_DKB;
			}

			/*
			* pick up the mouse input (if any)  */

			if ((status = duart->statusB) & RCV_RDY  &&
			    qdflags[qd].pntr_id == MOUSE_ID) {

				if (status & 0x70) {
					duart->cmdB = 0x40;
					continue;
				}

				/* event queue full now? (overflow condition) */

				if (ISFULL(eqh) == TRUE) {
					printf(
					"qd%d: qdiint: event queue overflow\n",
					     qd);
					break;
				}

				data = duart->dataB;      /* get report byte */
				++new_rep->bytcnt; /* bump report byte count */

				/*
				* if 1st byte of report.. */

				if ( data & START_FRAME) {
					new_rep->state = data;
					if (new_rep->bytcnt > 1) {
						/* start of new frame */
						new_rep->bytcnt = 1;    
						/* ..continue looking */
						continue;		    
					}
				}

				/*
				* if 2nd byte of report.. */

				else if (new_rep->bytcnt == 2) {
					new_rep->dx = data & 0x00FF;
				}

				/*
				* if 3rd byte of report, load input event queue */

				else if (new_rep->bytcnt == 3) {

					new_rep->dy = data & 0x00FF;
					new_rep->bytcnt = 0;

					/*
					* if mouse position has changed.. */

					if (new_rep->dx != 0  ||  new_rep->dy != 0) {

						/*
						* calculate acceleration factor, if needed	*/

						if (qdflags[qd].curs_acc > ACC_OFF) {

							if (qdflags[qd].curs_thr <= new_rep->dx)
							    new_rep->dx +=
							    (new_rep->dx - qdflags[qd].curs_thr)
							    * qdflags[qd].curs_acc;

							if (qdflags[qd].curs_thr <= new_rep->dy)
							    new_rep->dy +=
							    (new_rep->dy - qdflags[qd].curs_thr)
							    * qdflags[qd].curs_acc;
						}

						/*
						* update cursor position coordinates */

						if (new_rep->state & X_SIGN) {
							eqh->curs_pos.x += new_rep->dx;
							if (eqh->curs_pos.x > 1023)
							    eqh->curs_pos.x = 1023;
						}
						else {
							eqh->curs_pos.x -= new_rep->dx;
							if (eqh->curs_pos.x < -15)
							    eqh->curs_pos.x = -15;
						}

						if (new_rep->state & Y_SIGN) {
							eqh->curs_pos.y -= new_rep->dy;
							if (eqh->curs_pos.y < -15)
							    eqh->curs_pos.y = -15;
						}
						else {
							eqh->curs_pos.y += new_rep->dy;
							if (eqh->curs_pos.y > 863)
							    eqh->curs_pos.y = 863;
						}

						/*
						* update cursor screen position */

						dga = (struct dga *) qdmap[qd].dga;
						dga->x_cursor = TRANX(eqh->curs_pos.x);
						dga->y_cursor = TRANY(eqh->curs_pos.y);

						/*
						* if cursor is in the box, no event report */

						if (eqh->curs_pos.x <= eqh->curs_box.right	&&
						    eqh->curs_pos.x >= eqh->curs_box.left  &&
						    eqh->curs_pos.y >= eqh->curs_box.top  &&
						    eqh->curs_pos.y <= eqh->curs_box.bottom ) {
							goto GET_MBUTTON;
						}

						/*
						* report the mouse motion event */

						event = PUTBEGIN(eqh);
						PUTEND(eqh);

						++do_wakeup;   /* request a select wakeup call */

						event->vse_x = eqh->curs_pos.x;
						event->vse_y = eqh->curs_pos.y;

						event->vse_device = VSE_MOUSE;  /* mouse */
						event->vse_type = VSE_MMOTION;  /* pos changed */
						event->vse_key = 0;
						event->vse_direction = 0;
						event->vse_time = TOY;	/* time stamp */
					}

GET_MBUTTON:
					/*
					* if button state has changed */

					a = new_rep->state & 0x07;    /*mask nonbutton bits */
					b = last_rep[qd].state & 0x07;

					if (a ^ b) {

						for ( c = 1;  c < 8; c <<= 1) {

							if (!( c & (a ^ b))) /* this button change? */
							    continue;

							/* event queue full? (overflow condition) */

							if (ISFULL(eqh) == TRUE) {
								printf("qd%d: qdiint: event queue overflow\n", qd);
								break;
							}

							event = PUTBEGIN(eqh);	/* get new event */
							PUTEND(eqh);

							++do_wakeup;   /* request select wakeup */

							event->vse_x = eqh->curs_pos.x;
							event->vse_y = eqh->curs_pos.y;

							event->vse_device = VSE_MOUSE;	/* mouse */
							event->vse_type = VSE_BUTTON; /* new button */
							event->vse_time = TOY;	      /* time stamp */

							/* flag changed button and if up or down */

							if (c == RIGHT_BUTTON)
							    event->vse_key = VSE_RIGHT_BUTTON;
							else if (c == MIDDLE_BUTTON)
							    event->vse_key = VSE_MIDDLE_BUTTON;
							else if (c == LEFT_BUTTON)
							    event->vse_key = VSE_LEFT_BUTTON;

							/* set bit = button depressed */

							if (c & a)
							    event->vse_direction = VSE_KBTDOWN;
							else
								event->vse_direction = VSE_KBTUP;
						}
					}

					/* refresh last report */

					last_rep[qd] = current_rep[qd];

				}  /* get last byte of report */
			} else if ((status = duart->statusB)&RCV_RDY &&
			           qdflags[qd].pntr_id == TABLET_ID) {
				/*
				* pickup tablet input, if any  
				*/
				if (status&0x70) {
					duart->cmdB = 0x40;
					continue;
				}
				/* 
				 * event queue full now? (overflow condition) 
				 */
				if (ISFULL(eqh) == TRUE) {
					printf("qd%d: qdiint: event queue overflow\n", qd);
					break;
				}

				data = duart->dataB;      /* get report byte */
				++new_rep->bytcnt;	      /* bump report byte count */

				/*
				* if 1st byte of report.. */

				if (data & START_FRAME) {
					new_rep->state = data;
					if (new_rep->bytcnt > 1) {
						new_rep->bytcnt = 1;    /* start of new frame */
						continue;		    /* ..continue looking */
					}
				}

				/*
				* if 2nd byte of report.. */

				else if (new_rep->bytcnt == 2) {
					new_rep->dx = data & 0x3F;
				}

				/*
				* if 3rd byte of report.. */

				else if (new_rep->bytcnt == 3) {
					new_rep->dx |= (data & 0x3F) << 6;
				}

				/*
				* if 4th byte of report.. */

				else if (new_rep->bytcnt == 4) {
					new_rep->dy = data & 0x3F;
				}

				/*
				* if 5th byte of report, load input event queue */

				else if (new_rep->bytcnt == 5) {

					new_rep->dy |= (data & 0x3F) << 6;
					new_rep->bytcnt = 0;

					/*
					* update cursor position coordinates */

					new_rep->dx /= qdflags[qd].tab_res;
					new_rep->dy = (2200 - new_rep->dy)
					    / qdflags[qd].tab_res;

					if (new_rep->dx > 1023) {
						new_rep->dx = 1023;
					}
					if (new_rep->dy > 863) {
						new_rep->dy = 863;
					}

					/*
					* report an event if the puck/stylus has moved
					*/

					if (eqh->curs_pos.x != new_rep->dx ||
					    eqh->curs_pos.y != new_rep->dy) {

						eqh->curs_pos.x = new_rep->dx;
						eqh->curs_pos.y = new_rep->dy;

						/*
						* update cursor screen position */

						dga = (struct dga *) qdmap[qd].dga;
						dga->x_cursor = TRANX(eqh->curs_pos.x);
						dga->y_cursor = TRANY(eqh->curs_pos.y);

						/*
						* if cursor is in the box, no event report
						*/

						if (eqh->curs_pos.x <= eqh->curs_box.right	&&
						    eqh->curs_pos.x >= eqh->curs_box.left  &&
						    eqh->curs_pos.y >= eqh->curs_box.top  &&
						    eqh->curs_pos.y <= eqh->curs_box.bottom ) {
							goto GET_TBUTTON;
						}

						/*
						* report the tablet motion event */

						event = PUTBEGIN(eqh);
						PUTEND(eqh);

						++do_wakeup;   /* request a select wakeup call */

						event->vse_x = eqh->curs_pos.x;
						event->vse_y = eqh->curs_pos.y;

						event->vse_device = VSE_TABLET;  /* tablet */
						/*
						* right now, X handles tablet motion the same
						* as mouse motion
						*/
						event->vse_type = VSE_MMOTION;   /* pos changed */
						event->vse_key = 0;
						event->vse_direction = 0;
						event->vse_time = TOY;	/* time stamp */
					}
GET_TBUTTON:
					/*
					* if button state has changed */

					a = new_rep->state & 0x1E;   /* mask nonbutton bits */
					b = last_rep[qd].state & 0x1E;

					if (a ^ b) {

						/* event queue full now? (overflow condition) */

						if (ISFULL(eqh) == TRUE) {
							printf("qd%d: qdiint: event queue overflow\n",qd);
							break;
						}

						event = PUTBEGIN(eqh);  /* get new event */
						PUTEND(eqh);

						++do_wakeup;   /* request a select wakeup call */

						event->vse_x = eqh->curs_pos.x;
						event->vse_y = eqh->curs_pos.y;

						event->vse_device = VSE_TABLET;  /* tablet */
						event->vse_type = VSE_BUTTON; /* button changed */
						event->vse_time = TOY;	   /* time stamp */

						/* define the changed button and if up or down */

						for ( c = 1;  c <= 0x10; c <<= 1) {
							if (c & (a ^ b)) {
								if (c == T_LEFT_BUTTON)
								    event->vse_key = VSE_T_LEFT_BUTTON;
								else if (c == T_FRONT_BUTTON)
								    event->vse_key = VSE_T_FRONT_BUTTON;
								else if (c == T_RIGHT_BUTTON)
								    event->vse_key = VSE_T_RIGHT_BUTTON;
								else if (c == T_BACK_BUTTON)
								    event->vse_key = VSE_T_BACK_BUTTON;
								break;
							}
						}

						/* set bit = button depressed */

						if (c & a)
						    event->vse_direction = VSE_KBTDOWN;
						else
							event->vse_direction = VSE_KBTUP;
					}

					/* refresh last report */

					last_rep[qd] = current_rep[qd];

				} /* get last byte of report */
			} /* pick up tablet input */

		} /* while input available.. */

		/*
		* do select wakeup	
		*/
		if (qdrsel[qd] && do_wakeup && qdflags[qd].selmask & SEL_READ) {
			selwakeup(qdrsel[qd], 0);
			qdrsel[qd] = 0;
			qdflags[qd].selmask &= ~SEL_READ;
			do_wakeup = 0;
		}
	} else {
		/*
		 * if the graphic device is not turned on, this is console input
		 */
		ui = qdinfo[qd];
		if (ui == 0 || ui->ui_alive == 0)
			return;

		tp = &qd_tty[qd << 2];

		/*
		 * Get a character from the keyboard. 
		 */
		while (duart->statusA&RCV_RDY) {
			key = duart->dataA;
			key &= 0xFF;
			/*
			* Check for various keyboard errors  
			*/
			if (key == LK_POWER_ERROR || key == LK_KDOWN_ERROR ||
			    key == LK_INPUT_ERROR || key == LK_OUTPUT_ERROR) {
				printf("qd%d: qdiint: Keyboard error, code = %x\n",qd,key);
				return;
			}

			if (key < LK_LOWEST)
			    return;

			/*
			* See if its a state change key */

			switch (key) {

			case LOCK:
				q_keyboard.lock ^= 0xffff;	/* toggle */
				if (q_keyboard.lock)
					(void)led_control(qd, LK_LED_ENABLE,
							  LK_LED_LOCK);
				else
					(void)led_control(qd, LK_LED_DISABLE,
							  LK_LED_LOCK);
				return;

			case SHIFT:
				q_keyboard.shift ^= 0xFFFF;
				return;

			case CNTRL:
				q_keyboard.cntrl ^= 0xFFFF;
				return;

			case ALLUP:
				q_keyboard.cntrl = 0;
				q_keyboard.shift = 0;
				return;

			case REPEAT:
				chr = q_keyboard.last;
				break;

				/*
				* Test for cntrl characters. If set, see if the character
				* is elligible to become a control character. */

			default:

				if (q_keyboard.cntrl) {
					chr = q_key[key];
					if (chr >= ' ' && chr <= '~')
					    chr &= 0x1F;
					else if (chr >= 0xA1 && chr <= 0xFE)
					    chr &= 0x9F;
				}
				else if( q_keyboard.lock || q_keyboard.shift )
				    chr = q_shift_key[key];
				else
					chr = q_key[key];
				break;
			}

			q_keyboard.last = chr;

			/*
			* Check for special function keys */

			if (chr & 0x100) {
				char *string;
				string = q_special[chr & 0x7F];
				while(*string)
				    (*linesw[tp->t_line].l_rint)(*string++, tp);
			}
			else {
				(*linesw[tp->t_line].l_rint)(chr&0177, tp);
			}
		}
	}
} /* qdiint */

/*
 *
 * Clear the QDSS screen
 *
 *			     >>> NOTE <<<
 *
 *   This code requires that certain adder initialization be valid.  To
 *   assure that this requirement is satisfied, this routine should be
 *   called only after calling the "setup_dragon()" function.
 *
 *   Clear the bitmap a piece at a time. Since the fast scroll clear
 *   only clears the current displayed portion of the bitmap put a
 *   temporary value in the y limit register so we can access whole
 *   bitmap
 *
 */
clear_qd_screen(unit)
	int unit;
{
	register struct adder *adder;
	adder = (struct adder *) qdmap[unit].adder;

	adder->x_limit = 1024;
	adder->y_limit = 2048 - CHAR_HEIGHT;
	adder->y_offset_pending = 0;
#define WSV  (void)wait_status(adder, VSYNC); (void)wait_status(adder, VSYNC)
	WSV;
	adder->y_scroll_constant = SCROLL_ERASE;
	WSV;
	adder->y_offset_pending = 864;
	WSV;
	adder->y_scroll_constant = SCROLL_ERASE;
	WSV;
	adder->y_offset_pending = 1728;
	WSV;
	adder->y_scroll_constant = SCROLL_ERASE;
	WSV;
	adder->y_offset_pending = 0;	 /* back to normal */
	WSV;
	adder->x_limit = MAX_SCREEN_X;
	adder->y_limit = MAX_SCREEN_Y + FONT_HEIGHT;
#undef WSV

} /* clear_qd_screen */

/*
 *  kernel console output to the glass tty
 */
qdputc(chr)
	register char chr;
{

	/*
	 * if system is now physical, forget it (ie: crash DUMP) 
	 */
	if ((mfpr(MAPEN) & 1) == 0)
		return;

	blitc(0, (u_char)(chr & 0xff));
	if ((chr & 0177) == '\n')
		blitc(0, '\r');

} /* qdputc */

/*
 *  load the mouse cursor's template RAM bitmap
 */
ldcursor(unit, bitmap)
	int unit;
	register short *bitmap;
{
	register struct dga *dga;
	register short *temp;
	register int i;
	int curs;

	dga = (struct dga *) qdmap[unit].dga;
	temp = (short *) qdmap[unit].template;

	if (dga->csr & CURS_ENB) {	/* if the cursor is enabled.. */
		curs = -1;		/* ..note that.. */
		dga->csr &= ~CURS_ENB;	/* ..and shut it off */
	} else 
		curs = 0;

	dga->csr &= ~CURS_ENB;		/* shut off the cursor */

	temp += (8 * 1024) - 32;	/* cursor is 32 WORDS from the end */
	/* ..of the 8k WORD template space */
	for (i = 0; i < 32; ++i)
		*temp++ = *bitmap++;

	if (curs) {			/* if cursor was enabled.. */
		dga->csr |= CURS_ENB;	/* ..turn it back on */
	}

} /* ldcursor */

/*
 *  Put the console font in the QDSS off-screen memory
 */
ldfont(unit)
	int unit;
{
	register struct adder *adder;

	register i, j, k, max_chars_line;
	register short packed;

	adder = (struct adder *) qdmap[unit].adder;

	/*
	* setup VIPER operand control registers  
	*/
	write_ID(adder, MASK_1, 0xFFFF);
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 255);
	write_ID(adder, VIPER_Z_LOAD | BACKGROUND_COLOR_Z, 0);

	write_ID(adder, SRC1_OCR_B,
	EXT_NONE | INT_NONE | ID | BAR_SHIFT_DELAY);
	write_ID(adder, SRC2_OCR_B,
	EXT_NONE | INT_NONE | ID | BAR_SHIFT_DELAY);
	write_ID(adder, DST_OCR_B,
	EXT_SOURCE | INT_NONE | NO_ID | NO_BAR_SHIFT_DELAY);

	adder->rasterop_mode = DST_WRITE_ENABLE | DST_INDEX_ENABLE | NORMAL;

	/*
	* load destination data  
	*/
	(void)wait_status(adder, RASTEROP_COMPLETE);

	adder->destination_x = FONT_X;
	adder->destination_y = FONT_Y;
#if FONT_WIDTH > MAX_SCREEN_X
	adder->fast_dest_dx = MAX_SCREEN_X;
#else
	adder->fast_dest_dx = FONT_WIDTH;
#endif
	adder->slow_dest_dy = CHAR_HEIGHT;

	/*
	* setup for processor to bitmap xfer  */

	write_ID(adder, CS_UPDATE_MASK, 0x0001);
	adder->cmd = PBT | OCRB | 2 | DTE | 2;

	/*
	* Figure out how many characters can be stored on one "line" of 
	* offscreen memory.
	*/
	max_chars_line = MAX_SCREEN_X/(CHAR_WIDTH*2);
	if ((CHARS/2 + CHARS%2) < max_chars_line)
	    max_chars_line = CHARS/2 + CHARS%2;

	/*
	* iteratively do the processor to bitmap xfer */

	for (i = 0; i < ROWS; ++i) {

		/* PTOB a scan line */

		for (j = 0, k = i; j < max_chars_line; ++j) {
			/* PTOB one scan of a char cell */

			packed = q_font[k];
			k += ROWS;
			packed |= ((short)q_font[k] << 8);
			k += ROWS;

			(void)wait_status(adder, TX_READY);
			adder->id_data = packed;
		}
	}

	/*
	 * (XXX XXX XXX - should remove)
	 *
	 * Copy the second row of characters.  Subtract the first
	 * row from the total number.  Divide this quantity by 2
	 * because 2 chars are stored in a short in the PTOB loop
	 * below.  Figure out how many characters can be stored on
	 * one "line" of offscreen memory 
	 */

	max_chars_line = MAX_SCREEN_X/(CHAR_WIDTH*2);
	if ((CHARS/2 + CHARS%2) < max_chars_line)
	    return;
	max_chars_line = (CHARS/2 + CHARS%2) - max_chars_line; /* 95 - 64 */
	/* Paranoia check to see if 3rd row may be needed */
	if (max_chars_line > (MAX_SCREEN_X/(CHAR_WIDTH*2)))
	    max_chars_line = MAX_SCREEN_X/(CHAR_WIDTH*2);

	adder->destination_x = FONT_X;
	adder->destination_y = FONT_Y - CHAR_HEIGHT;
	adder->fast_dest_dx = max_chars_line * CHAR_WIDTH * 2;
	adder->slow_dest_dy = CHAR_HEIGHT;

	/*
	* setup for processor to bitmap xfer  
	*/
	write_ID(adder, CS_UPDATE_MASK, 0x0001);
	adder->cmd = PBT | OCRB | 2 | DTE | 2;

	/*
	* iteratively do the processor to bitmap xfer 
	*/
	for (i = 0; i < ROWS; ++i) {
		/* 
		 * PTOB a scan line 
		 */
		for (j = 0, k = i; j < max_chars_line; ++j) {
			/*
			 * PTOB one scan of a char cell 
			 */
			packed = q_font[k + FONT_OFFSET];
			k += ROWS;
			packed |= ((short)q_font[k + FONT_OFFSET] << 8);
			k += ROWS;
			(void)wait_status(adder, TX_READY);
			adder->id_data = packed;
		}
	}

}  /* ldfont */

/*
 *  Get a character from the LK201 (polled)
 */
qdgetc()
{
	register short key;
	register char chr;
	register struct duart *duart;

	duart = (struct duart *) qdmap[0].duart;

	/*
	* Get a character from the keyboard. 
	*/
LOOP:
	while (!(duart->statusA&RCV_RDY))
		;

	key = duart->dataA;
	key &= 0xFF;

	/*
	* Check for various keyboard errors  */

	if (key == LK_POWER_ERROR || key == LK_KDOWN_ERROR ||
	    key == LK_INPUT_ERROR || key == LK_OUTPUT_ERROR) {
		printf("Keyboard error, code = %x\n", key);
		return(0);
	}

	if (key < LK_LOWEST)
		return(0);

	/*
	 * See if its a state change key 
	 */
	switch (key) {

	case LOCK:
		q_keyboard.lock ^= 0xffff;	/* toggle */
		if (q_keyboard.lock)
			(void)led_control(0, LK_LED_ENABLE, LK_LED_LOCK);
		else
			(void)led_control(0, LK_LED_DISABLE, LK_LED_LOCK);
		goto LOOP;

	case SHIFT:
		q_keyboard.shift ^= 0xFFFF;
		goto LOOP;

	case CNTRL:
		q_keyboard.cntrl ^= 0xFFFF;
		goto LOOP;

	case ALLUP:
		q_keyboard.cntrl = 0;
		q_keyboard.shift = 0;
		goto LOOP;

	case REPEAT:
		chr = q_keyboard.last;
		break;

		/*
		* Test for cntrl characters. If set, see if the character
		* is elligible to become a control character. 
		*/
	default:

		if (q_keyboard.cntrl) {
			chr = q_key[key];
			if (chr >= ' ' && chr <= '~')
			    chr &= 0x1F;
		}
		else if ( q_keyboard.lock || q_keyboard.shift )
		    chr = q_shift_key[key];
		else
			chr = q_key[key];
		break;
	}

	if (chr < ' ' && chr > '~')	/* if input is non-displayable */
		return(0);		/* ..then pitch it! */

	q_keyboard.last = chr;

	/*
	* Check for special function keys */

	if (chr & 0x80) 		/* pitch the function keys */
		return(0);
	else
		return(chr);

} /* qdgetc */

/*
 *  led_control()... twiddle LK-201 LED's
 */
led_control(unit, cmd, led_mask)
	int unit, cmd, led_mask;
{
	register i;
	register struct duart *duart;

	duart = (struct duart *)qdmap[unit].duart;

	for (i = 1000; i > 0; --i) {
		if (duart->statusA&XMT_RDY) {
			duart->dataA = cmd;
			break;
		}
	}
	for (i = 1000; i > 0; --i) {
		if (duart->statusA&XMT_RDY) {
			duart->dataA = led_mask;
			break;
		}
	}
	if (i == 0)
		return(BAD);
	return(GOOD);

} /* led_control */

/*
 *  scroll_up()... move the screen up one character height
 */
scroll_up(adder)
	register struct adder *adder;
{
	/*
	* setup VIPER operand control registers  
	*/
	(void)wait_status(adder, ADDRESS_COMPLETE);
	write_ID(adder, CS_UPDATE_MASK, 0x00FF);  /* select all planes */
	write_ID(adder, MASK_1, 0xFFFF);
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 255);
	write_ID(adder, VIPER_Z_LOAD | BACKGROUND_COLOR_Z, 0);
	write_ID(adder, SRC1_OCR_B,
	EXT_NONE | INT_SOURCE | ID | BAR_SHIFT_DELAY);
	write_ID(adder, DST_OCR_B,
	EXT_NONE | INT_NONE | NO_ID | NO_BAR_SHIFT_DELAY);
	/*
	 * load DESTINATION origin and vectors  
	 */
	adder->fast_dest_dy = 0;
	adder->slow_dest_dx = 0;
	adder->error_1 = 0;
	adder->error_2 = 0;
	adder->rasterop_mode = DST_WRITE_ENABLE | NORMAL;
	adder->destination_x = 0;
	adder->fast_dest_dx = 1024;
	adder->destination_y = 0;
	adder->slow_dest_dy = 864 - CHAR_HEIGHT;
	/*
	 * load SOURCE origin and vectors  
	 */
	adder->source_1_x = 0;
	adder->source_1_dx = 1024;
	adder->source_1_y = 0 + CHAR_HEIGHT;
	adder->source_1_dy = 864 - CHAR_HEIGHT;
	write_ID(adder, LU_FUNCTION_R1, FULL_SRC_RESOLUTION | LF_SOURCE);
	adder->cmd = RASTEROP | OCRB | 0 | S1E | DTE;
	/*
	 * do a rectangle clear of last screen line 
	 */
	write_ID(adder, MASK_1, 0xffff);
	write_ID(adder, SOURCE, 0xffff);
	write_ID(adder,DST_OCR_B,
	(EXT_NONE | INT_NONE | NO_ID | NO_BAR_SHIFT_DELAY));
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 0);
	adder->error_1 = 0;
	adder->error_2 = 0;
	adder->slow_dest_dx = 0;		/* set up the width of	*/
	adder->slow_dest_dy = CHAR_HEIGHT;	/* rectangle */
	adder->rasterop_mode = (NORMAL | DST_WRITE_ENABLE) ;
	(void)wait_status(adder, RASTEROP_COMPLETE);
	adder->destination_x = 0;
	adder->destination_y = 864 - CHAR_HEIGHT;
	adder->fast_dest_dx = 1024;	/* set up the height	*/
	adder->fast_dest_dy = 0;	/* of rectangle 	*/
	write_ID(adder, LU_FUNCTION_R2, (FULL_SRC_RESOLUTION | LF_SOURCE));
	adder->cmd = (RASTEROP | OCRB | LF_R2 | DTE ) ;

} /* scroll_up */

/*
 *  init shared memory pointers and structures
 */
init_shared(unit)
	register unit;
{
	register struct dga *dga;

	dga = (struct dga *) qdmap[unit].dga;

	/*
	* initialize the event queue pointers and header */

	eq_header[unit] = (struct qdinput *)
	    ((((int)event_shared & ~(0x01FF)) + 512)
		+ (EVENT_BUFSIZE * unit));
	eq_header[unit]->curs_pos.x = 0;
	eq_header[unit]->curs_pos.y = 0;
	dga->x_cursor = TRANX(eq_header[unit]->curs_pos.x);
	dga->y_cursor = TRANY(eq_header[unit]->curs_pos.y);
	eq_header[unit]->curs_box.left = 0;
	eq_header[unit]->curs_box.right = 0;
	eq_header[unit]->curs_box.top = 0;
	eq_header[unit]->curs_box.bottom = 0;
	/*
	 * assign a pointer to the DMA I/O buffer for this QDSS. 
	 */
	DMAheader[unit] = (struct DMAreq_header *)
	    (((int)(&DMA_shared[0] + 512) & ~0x1FF)
		+ (DMAbuf_size * unit));
	DMAheader[unit]->DMAreq = (struct DMAreq *) ((int)DMAheader[unit]
	    + sizeof(struct DMAreq_header));
	DMAheader[unit]->QBAreg = 0;
	DMAheader[unit]->status = 0;
	DMAheader[unit]->shared_size = DMAbuf_size;
	DMAheader[unit]->used = 0;
	DMAheader[unit]->size = 10;	/* default = 10 requests */
	DMAheader[unit]->oldest = 0;
	DMAheader[unit]->newest = 0;
	/*
	* assign a pointer to the scroll structure for this QDSS. 
	*/
	scroll[unit] = (struct scroll *)
	    (((int)(&scroll_shared[0] + 512) & ~0x1FF)
		+ (sizeof(struct scroll) * unit));
	scroll[unit]->status = 0;
	scroll[unit]->viper_constant = 0;
	scroll[unit]->y_scroll_constant = 0;
	scroll[unit]->y_offset = 0;
	scroll[unit]->x_index_pending = 0;
	scroll[unit]->y_index_pending = 0;
	/*
	* assign a pointer to the color map write buffer for this QDSS 
	*/
	color_buf[unit] = (struct color_buf *)
	    (((int)(&color_shared[0] + 512) & ~0x1FF)
		+ (COLOR_BUFSIZ * unit));
	color_buf[unit]->status = 0;
	color_buf[unit]->count = 0;

} /* init_shared */

/*
 * init the ADDER, VIPER, bitmaps, & color map
 */
setup_dragon(unit)
	int unit;
{

	register struct adder *adder;
	register struct dga *dga;
	short *memcsr;
	register i;
	short top;		/* clipping/scrolling boundaries */
	short bottom;
	short right;
	short left;
	short *red;		/* color map pointers */
	short *green;
	short *blue;

	/*
	* init for setup 
	*/
	adder = (struct adder *) qdmap[unit].adder;
	dga = (struct dga *) qdmap[unit].dga;
	memcsr = (short *) qdmap[unit].memcsr;
	dga->csr &= ~(DMA_IE | 0x700);	/* halt DMA and kill the intrpts */
	*memcsr = SYNC_ON;		/* blank screen and turn off LED's */
	adder->command = CANCEL;
	/*
	* set monitor timing 
	*/
	adder->x_scan_count_0 = 0x2800;
	adder->x_scan_count_1 = 0x1020;
	adder->x_scan_count_2 = 0x003A;
	adder->x_scan_count_3 = 0x38F0;
	adder->x_scan_count_4 = 0x6128;
	adder->x_scan_count_5 = 0x093A;
	adder->x_scan_count_6 = 0x313C;
	adder->sync_phase_adj = 0x0100;
	adder->x_scan_conf = 0x00C8;
	/*
	 * got a bug in secound pass ADDER! lets take care of it 
	 *
	 * normally, just use the code in the following bug fix code, but to
	 * make repeated demos look pretty, load the registers as if there was
	 * no bug and then test to see if we are getting sync 
	 */
	adder->y_scan_count_0 = 0x135F;
	adder->y_scan_count_1 = 0x3363;
	adder->y_scan_count_2 = 0x2366;
	adder->y_scan_count_3 = 0x0388;
	/*
	 * if no sync, do the bug fix code 
	 */
	if (wait_status(adder, VSYNC) == BAD) {
		/* first load all Y scan registers with very short frame and
		 * wait for scroll service.  This guarantees at least one SYNC
		 * to fix the pass 2 Adder initialization bug (synchronizes
		 * XCINCH with DMSEEDH) 
		 */
		adder->y_scan_count_0 = 0x01;
		adder->y_scan_count_1 = 0x01;
		adder->y_scan_count_2 = 0x01;
		adder->y_scan_count_3 = 0x01;
		/*
		 * delay at least 1 full frame time 
		 */
		(void)wait_status(adder, VSYNC);	
		(void)wait_status(adder, VSYNC);
		/*
		 * now load the REAL sync values (in reverse order just to
		 * be safe.  
		 */
		adder->y_scan_count_3 = 0x0388;
		adder->y_scan_count_2 = 0x2366;
		adder->y_scan_count_1 = 0x3363;
		adder->y_scan_count_0 = 0x135F;
	}
	*memcsr = SYNC_ON | UNBLANK;	/* turn off leds and turn on video */
	/*
	 * zero the index registers 
	 */
	adder->x_index_pending = 0;
	adder->y_index_pending = 0;
	adder->x_index_new = 0;
	adder->y_index_new = 0;
	adder->x_index_old = 0;
	adder->y_index_old = 0;
	adder->pause = 0;
	/*
	 * set rasterop mode to normal pen down 
	 */
	adder->rasterop_mode = DST_WRITE_ENABLE | DST_INDEX_ENABLE | NORMAL;
	/*
	 * set the rasterop registers to a default values 
	 */
	adder->source_1_dx = 1;
	adder->source_1_dy = 1;
	adder->source_1_x = 0;
	adder->source_1_y = 0;
	adder->destination_x = 0;
	adder->destination_y = 0;
	adder->fast_dest_dx = 1;
	adder->fast_dest_dy = 0;
	adder->slow_dest_dx = 0;
	adder->slow_dest_dy = 1;
	adder->error_1 = 0;
	adder->error_2 = 0;
	/*
	 * scale factor = UNITY 
	 */
	adder->fast_scale = UNITY;
	adder->slow_scale = UNITY;
	/*
	 * set the source 2 parameters 
	 */
	adder->source_2_x = 0;
	adder->source_2_y = 0;
	adder->source_2_size = 0x0022;
	/*
	* initialize plane addresses for eight vipers 
	*/
	write_ID(adder, CS_UPDATE_MASK, 0x0001);
	write_ID(adder, PLANE_ADDRESS, 0x0000);
	write_ID(adder, CS_UPDATE_MASK, 0x0002);
	write_ID(adder, PLANE_ADDRESS, 0x0001);
	write_ID(adder, CS_UPDATE_MASK, 0x0004);
	write_ID(adder, PLANE_ADDRESS, 0x0002);
	write_ID(adder, CS_UPDATE_MASK, 0x0008);
	write_ID(adder, PLANE_ADDRESS, 0x0003);
	write_ID(adder, CS_UPDATE_MASK, 0x0010);
	write_ID(adder, PLANE_ADDRESS, 0x0004);
	write_ID(adder, CS_UPDATE_MASK, 0x0020);
	write_ID(adder, PLANE_ADDRESS, 0x0005);
	write_ID(adder, CS_UPDATE_MASK, 0x0040);
	write_ID(adder, PLANE_ADDRESS, 0x0006);
	write_ID(adder, CS_UPDATE_MASK, 0x0080);
	write_ID(adder, PLANE_ADDRESS, 0x0007);
	/*
	 * initialize the external registers. 
	 */
	write_ID(adder, CS_UPDATE_MASK, 0x00FF);
	write_ID(adder, CS_SCROLL_MASK, 0x00FF);
	/*
	 * initialize resolution mode 
	 */
	write_ID(adder, MEMORY_BUS_WIDTH, 0x000C);     /* bus width = 16 */
	write_ID(adder, RESOLUTION_MODE, 0x0000);      /* one bit/pixel */
	/*
	 * initialize viper registers 
	 */
	write_ID(adder, SCROLL_CONSTANT, SCROLL_ENABLE|VIPER_LEFT|VIPER_UP);
	write_ID(adder, SCROLL_FILL, 0x0000);
	/*
	 * set clipping and scrolling limits to full screen 
	 */
	for (i = 1000, adder->status = 0; 
	     i > 0 && !(adder->status&ADDRESS_COMPLETE); --i)
		;
	if (i == 0)
	    printf("qd%d: setup_dragon: timeout on ADDRESS_COMPLETE\n",unit);
	top = 0;
	bottom = 2048;
	left = 0;
	right = 1024;
	adder->x_clip_min = left;
	adder->x_clip_max = right;
	adder->y_clip_min = top;
	adder->y_clip_max = bottom;
	adder->scroll_x_min = left;
	adder->scroll_x_max = right;
	adder->scroll_y_min = top;
	adder->scroll_y_max = bottom;
	(void)wait_status(adder, VSYNC);	/* wait at LEAST 1 full frame */
	(void)wait_status(adder, VSYNC);
	adder->x_index_pending = left;
	adder->y_index_pending = top;
	adder->x_index_new = left;
	adder->y_index_new = top;
	adder->x_index_old = left;
	adder->y_index_old = top;

	for (i = 1000, adder->status = 0; i > 0 && 
	     !(adder->status&ADDRESS_COMPLETE) ; --i)
		;
	if (i == 0)
	       printf("qd%d: setup_dragon: timeout on ADDRESS_COMPLETE\n",unit);

	write_ID(adder, LEFT_SCROLL_MASK, 0x0000);
	write_ID(adder, RIGHT_SCROLL_MASK, 0x0000);
	/*
	* set source and the mask register to all ones (ie: white) o
	*/
	write_ID(adder, SOURCE, 0xFFFF);
	write_ID(adder, MASK_1, 0xFFFF);
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 255);
	write_ID(adder, VIPER_Z_LOAD | BACKGROUND_COLOR_Z, 0);
	/*
	* initialize Operand Control Register banks for fill command 
	*/
	write_ID(adder, SRC1_OCR_A, EXT_NONE | INT_M1_M2  | NO_ID | WAIT);
	write_ID(adder, SRC2_OCR_A, EXT_NONE | INT_SOURCE | NO_ID | NO_WAIT);
	write_ID(adder, DST_OCR_A, EXT_NONE | INT_NONE	 | NO_ID | NO_WAIT);
	write_ID(adder, SRC1_OCR_B, EXT_NONE | INT_SOURCE | NO_ID | WAIT);
	write_ID(adder, SRC2_OCR_B, EXT_NONE | INT_M1_M2  | NO_ID | NO_WAIT);
	write_ID(adder, DST_OCR_B, EXT_NONE | INT_NONE | NO_ID | NO_WAIT);
	/*
	* init Logic Unit Function registers, (these are just common values,
	* and may be changed as required).  
	*/
	write_ID(adder, LU_FUNCTION_R1, FULL_SRC_RESOLUTION | LF_SOURCE);
	write_ID(adder, LU_FUNCTION_R2, FULL_SRC_RESOLUTION | LF_SOURCE |
		 INV_M1_M2);
	write_ID(adder, LU_FUNCTION_R3, FULL_SRC_RESOLUTION | LF_D_OR_S);
	write_ID(adder, LU_FUNCTION_R4, FULL_SRC_RESOLUTION | LF_D_XOR_S);
	/*
	* load the color map for black & white 
	*/
	for (i = 0, adder->status = 0; i < 10000 && !(adder->status&VSYNC); ++i)
		;

	if (i == 0)
		printf("qd%d: setup_dragon: timeout on VSYNC\n", unit);

	red = (short *) qdmap[unit].red;
	green = (short *) qdmap[unit].green;
	blue = (short *) qdmap[unit].blue;

	*red++ = 0x00;			/* black */
	*green++ = 0x00;
	*blue++ = 0x00;

	*red-- = 0xFF;			/* white */
	*green-- = 0xFF;
	*blue-- = 0xFF;

	/*
	* set color map for mouse cursor 
	*/

	red += 254;
	green += 254;
	blue += 254;

	*red++ = 0x00;			/* black */
	*green++ = 0x00;
	*blue++ = 0x00;

	*red = 0xFF;			/* white */
	*green = 0xFF;
	*blue = 0xFF;

} /* setup_dragon */

/*
 * Init the DUART and set defaults in input
 */
setup_input(unit)
	int unit;
{
	register struct duart *duart;	/* DUART register structure pointer */
	register i, bits;
	char id_byte;

	duart = (struct duart *) qdmap[unit].duart;
	duart->imask = 0;

	/*
	* setup the DUART for kbd & pointing device 
	*/
	duart->cmdA = RESET_M;	/* reset mode reg ptr for kbd */
	duart->modeA = 0x13;	/* 8 bits, no parity, rcv IE, */
				/* no RTS control,char error mode */
	duart->modeA = 0x07;	/* 1 stop bit,CTS does not IE XMT */
				/* no RTS control,no echo or loop */
	duart->cmdB = RESET_M;	/* reset mode reg pntr for host */
	duart->modeB = 0x07;	/* 8 bits, odd parity, rcv IE.. */
				/* ..no RTS cntrl, char error mode */
	duart->modeB = 0x07;	/* 1 stop bit,CTS does not IE XMT */
				/* no RTS control,no echo or loop */
	duart->auxctl = 0x00;	/* baud rate set 1 */
	duart->clkselA = 0x99;	/* 4800 baud for kbd */
	duart->clkselB = 0x99;	/* 4800 baud for mouse */

	/* reset everything for keyboard */

	for (bits = RESET_M; bits < START_BREAK; bits += 0x10)
		duart->cmdA = bits;

	/* reset everything for host */

	for (bits = RESET_M; bits < START_BREAK; bits += 0x10)
		duart->cmdB = bits;

	duart->cmdA = EN_RCV | EN_XMT; /* enbl xmt & rcv for kbd */
	duart->cmdB = EN_RCV | EN_XMT; /* enbl xmt & rcv for pointer device */

	/*
	* init keyboard defaults (DUART channel A) 
	*/
	for (i = 500; i > 0; --i) {
		if (duart->statusA&XMT_RDY) {
			duart->dataA = LK_DEFAULTS;
			break;
		}
	}

	for (i = 100000; i > 0; --i) {
		if (duart->statusA&RCV_RDY) {
			break;
		}
	}

	if (duart->dataA)	/* flush the ACK */
		;		

	/*
	* identify the pointing device 
	*/
	for (i = 500; i > 0; --i) {
		if (duart->statusB&XMT_RDY) {
			duart->dataB = SELF_TEST;
			break;
		}
	}

	/*
	* wait for 1st byte of self test report */

	for (i = 100000; i > 0; --i) {
		if (duart->statusB&RCV_RDY) {
			break;
		}
	}

	if (i == 0) {
		printf("qd[%d]: setup_input: timeout on 1st byte of self test\n"
		       ,unit);
		goto OUT;
	}

	if (duart->dataB)
		;

	/*
	* wait for ID byte of self test report	
	*/
	for (i = 100000; i > 0; --i) {
		if (duart->statusB&RCV_RDY) {
			break;
		}
	}

	if (i == 0) {
		printf("qd[%d]: setup_input: timeout on 2nd byte of self test\n", unit);
		goto OUT;
	}

	id_byte = duart->dataB;

	/*
	* wait for other bytes to come in  
	*/
	for (i = 100000; i > 0; --i) {
		if (duart->statusB & RCV_RDY) {
			if (duart->dataB)
				;
			break;
		}
	}
	if (i == 0) {
		printf("qd[%d]: setup_input: timeout on 3rd byte of self test\n", unit);
		goto OUT;
	}
	for (i = 100000; i > 0; --i) {
		if (duart->statusB&RCV_RDY) {
			if (duart->dataB)
				;
			break;
		}
	}
	if (i == 0) {
		printf("qd[%d]: setup_input: timeout on 4th byte of self test\n", unit);
		goto OUT;
	}
	/*
	* flag pointing device type and set defaults 
	*/
	for (i=100000; i>0; --i)
		;		/*XXX*/

	if ((id_byte & 0x0F) != TABLET_ID) {
		qdflags[unit].pntr_id = MOUSE_ID;

		for (i = 500; i > 0; --i) {
			if (duart->statusB&XMT_RDY) {
				duart->dataB = INC_STREAM_MODE;
				break;
			}
		}
	} 
	else {
		qdflags[unit].pntr_id = TABLET_ID;

		for (i = 500; i > 0; --i) {
			if (duart->statusB&XMT_RDY) {
				duart->dataB = T_STREAM;
				break;
			}
		}
	}
OUT:
	duart->imask = qdflags[unit].duart_imask;

} /* setup_input */

/*
 * delay for at least one display frame time
 *
 *	return: BAD means that we timed out without ever seeing the
 *		      vertical sync status bit
 *		GOOD otherwise
 */
wait_status(adder, mask)
	register struct adder *adder;
	register int mask;
{
	register i;

	for (i = 10000, adder->status = 0 ; i > 0  &&  
	     !(adder->status&mask) ; --i)
		;

	if (i == 0) {
		printf("wait_status: timeout polling for 0x%x in adder->status\n", mask);
		return(BAD);
	}

	return(GOOD);

} /* wait_status */

/*
 * write out onto the ID bus
 */
write_ID(adder, adrs, data)
	register struct adder *adder;
	register short adrs;
	register short data;
{
	register i;

	for (i = 100000, adder->status = 0 ; 
	      i > 0  &&  !(adder->status&ADDRESS_COMPLETE) ; --i)
		;

	if (i == 0)
		goto ERR;

	for (i = 100000, adder->status = 0 ; 
	      i > 0  &&  !(adder->status&TX_READY) ; --i)
		;

	if (i > 0) {
		adder->id_data = data;
		adder->command = ID_LOAD | adrs;
		return ;
	}

ERR:
	printf("write_ID: timeout trying to write to VIPER\n");
	return ;

} /* write_ID */
#endif
