/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)qduser.h	7.1 (Berkeley) %G%
 */

/* derived from: @(#)qduser.h	6.1	(ULTRIX)	11/24/87       */
/************************************************************************
 *									*
 *			Copyright (c) 1986 by				*
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
 ************************************************************************/

/***************************************************************************
*
*	QDUSER...
*	This file defines values shared between the driver and a client
*
***************************************************************************/

/***************************************************************************
*	revision history:
****************************************************************************
*
* 21 jul 86  ram    fixed define of CURSOR_MIN_Y
* 25 nov 85  longo  added macro and bit defines for DMA error flags
* 11 nov 85  longo  renamed _vs_eventqueue to "qdinput" struct
* 23 oct 85  longo  added more defines to the DMA stuff
* 17 oct 85  longo  changed "struct rgb" chars to be unsigned
* 16 oct 85  longo  added new TABLET support definitions
* 15 oct 85  longo  re-wrote DMA queue access macros
* 08 oct 85  longo  added status flag manipulation macros to DMA stuff
* 02 oct 85  longo  added support for color map write buffer loading
* 26 sep 85  longo  removed adder sertup params from DMA request struct
* 23 sep 85  longo  added DMA queue access macros
* 30 aug 85  longo  fixed crock in "qdiobuf" struct compile-time sizing. Also
*		    removed DMAcontrol struct from DMA buffer for field test
* 26 aug 85  longo  put in conditional include of "qevent.h" for user prg's
* 18 jul 85  longo  changed semantics so that head is tail and tail is head
* 12 jul 85  longo  moved "mouse_report" struct and defs over to qd_data.c
* 11 jul 85  longo  added device coordinate to gate array cursor coordinate
*		    transformation macros
* 03 jul 85  longo  changed kernel typdef's for data types to long-hand
* 10 may 85  longo  created
*
***************************************************************************/

#ifdef KERNEL
#include "../uba/qevent.h"		/* include event struct defs */
#else
#include <vax/uba/qevent.h>
#endif

/*---------------------
* QDSS device map */

	struct qdmap {			/* map of register blocks in QDSS */

	    char *template;
	    char *adder;
	    char *dga;
	    char *duart;
	    char *memcsr;
	    char *red;
	    char *blue;
	    char *green;
	};

/*--------------------------------------------
* DGA CSR bit definitions and register map */

#define DMADONE		0x8000		/* DMA done status */
#define SET_DONE_FIFO	0x4000		/* set DMADONE when FIFO empty.. */
					/* ..AND count = 0 */

#define PTOB_ENB	0x0600		/* host-to-bitmap DMA xfer */
#define BTOP_ENB	0x0400		/* bitmap-to-host DMA xfer */
#define DL_ENB		0x0200		/* display list DMA xfer */
#define HALT		0x0000		/* halt DGA */

#define BYTE_DMA	0x0100		/* byte/word DMA xfer */

#define DMA_ERR		0x0080		/* DMA error bits */
#define PARITY_ERR	0x0040		/* memory parity error in DMA */
#define BUS_ERR		0x0020		/* bus timeout error in DMA */

#define GLOBAL_IE	0x0004		/* global interrupt enable */
#define DMA_IE		0x0002		/* DMA interrupt enable */
#define CURS_ENB	0x0001		/* cursor enable */

/* QDSS memcsr bit definitions */

#define	UNBLANK			0x0020
#define SYNC_ON			0x0008

	struct dga {

	    unsigned short csr;
	    unsigned short adrs_lo;	/* destination address of bitmap to */
	    unsigned short adrs_hi;	/*   host DMA */
	    unsigned short bytcnt_lo;	/* byte length of requested DMA */
	    unsigned short bytcnt_hi;	/* (WO = bytcnt) (RO = fifo count) */
	    unsigned short fifo;	/* FIFO register */
	    unsigned short x_cursor;	/* cursor position registers */
	    unsigned short y_cursor;
	    unsigned short ivr;		/* interrupt vector register */
	    unsigned short memadr;	/* memory base address register */
	};

/*-------------------------------------------------------------------------
* macros to transform device coordinates to hardware cursor coordinates */

#define CURS_MIN_X 	232	/* device coordinate x = 0 */
#define CURS_MIN_Y 	16	/* device coordinate y = 0 */

#define TRANX(x) ( -(((int)(x)+CURS_MIN_X) & ~0x0003) | \
		   (((int)(x)+CURS_MIN_X) & 0x0003) )

#define TRANY(y) ( -((y)+CURS_MIN_Y) )

/*********************************************************************
*
*	EVENT QUEUE DEFINITIONS
*
**********************************************************************
* most of the event queue definitions are found in "qevent.h".  But a
* few things not found there are here.  */ 	

/* The event queue header */
	
typedef struct qdinput {

	    struct _vs_eventqueue header;  /* event queue ring handling */

	    /* for VS100 and QVSS compatability reasons, additions to this
	    *  structure must be made below this point.  */

	    struct _vs_cursor curs_pos;	/* current mouse position */
	    struct _vs_box curs_box;	/* cursor reporting box */

	};
	
/* vse_key field.  definitions for mouse buttons */

#define VSE_LEFT_BUTTON		0
#define VSE_MIDDLE_BUTTON	1
#define VSE_RIGHT_BUTTON	2

/* vse_key field.  definitions for mouse buttons */

#define VSE_T_LEFT_BUTTON	0
#define VSE_T_FRONT_BUTTON	1
#define VSE_T_RIGHT_BUTTON	2
#define VSE_T_BACK_BUTTON	4

#define VSE_T_BARREL_BUTTON	VSE_T_LEFT_BUTTON
#define VSE_T_TIP_BUTTON	VSE_T_FRONT_BUTTON

/*--------------------------------------------------------------------------
*   These are the macros to be used for loading and extracting from the event
* queue.  It is presumed that the macro user will only use the access macros
* if the event queue is non-empty ( ISEMPTY(eq) == FALSE ), and that the
* driver will only load the event queue after checking that it is not full
* ( ISFULL(eq) == FALSE ).  ("eq" is a pointer to the event queue header.)
*
*   Before an event access session for a particular event, the macro users
* must use the xxxBEGIN macro, and the xxxEND macro after an event is through
* with.  As seen below, the xxxBEGIN and xxxEND macros maintain the event
* queue access mechanism.
*
* First, the macros to be used by the event queue reader 
*/

#define ISEMPTY(eq)	  ((eq)->header.head == (eq)->header.tail)
#define GETBEGIN(eq)	  (&(eq)->header.events[(eq)->header.head]) 

#define GET_X(event)	  ((event)->vse_x)  	     /* get x position */
#define GET_Y(event)	  ((event)->vse_y)  	     /* get y position */
#define GET_TIME(event)	  ((event)->vse_time) 	     /* get time */
#define GET_TYPE(event)	  ((event)->vse_type)	     /* get entry type */
#define GET_KEY(event)	  ((event)->vse_key)  	     /* get keycode */
#define GET_DIR(event)	  ((event)->vse_direction)     /* get direction */
#define GET_DEVICE(event) ((event)->vse_device)        /* get device */

#define GETEND(eq)        (++(eq)->header.head >= (eq)->header.size ? \
			   (eq)->header.head = 0 : 0 )

/*------------------------------------------------
* macros to be used by the event queue loader  */

	/* ISFULL yields TRUE if queue is full */

#define ISFULL(eq)	((eq)->header.tail+1 == (eq)->header.head ||   \
			 ((eq)->header.tail+1 == (eq)->header.size &&  \
			  (eq)->header.head == 0))

	/* get address of the billet for NEXT event */

#define PUTBEGIN(eq)	(&(eq)->header.events[(eq)->header.tail])

#define PUT_X(event, value)  	((event)->vse_x = value)    /* put X pos */
#define PUT_Y(event, value)   	((event)->vse_y = value)    /* put Y pos */
#define PUT_TIME(event, value)	((event)->vse_time = value)   /* put time */
#define PUT_TYPE(event, value)	((event)->vse_type = value) /* put type */
#define PUT_KEY(event, value)	((event)->vse_key = value) /* put input key */
#define PUT_DIR(event, value)	((event)->vse_direction = value) /* put dir */
#define PUT_DEVICE(event, value) ((event)->vse_device = value)   /* put dev */

#define PUTEND(eq)     (++(eq)->header.tail >= (eq)->header.size ?  \
			(eq)->header.tail = 0 : 0) 

/******************************************************************
*
*	DMA I/O DEFINITIONS
*
******************************************************************/

/*---------------------------------------------------------------------
* The DMA request queue is implemented as a ring buffer of "DMAreq"
  structures.  The queue is accessed using ring indices located in the
  "DMAreq_header" structure.  Access is implemented using access macros
  similar to the event queue access macros above.  */

	struct DMAreq {

	    short DMAtype;		/* DMA type code (for QDSS) */
	    short DMAdone;		/* DMA done parameter */
	    char  *bufp;		/* virtual adrs of buffer */
	    int   length;	        /* transfer buffer length */
	};

/* DMA type command codes */

#define DISPLIST	1	/* display list DMA */
#define PTOB		2	/* 1 plane Qbus to bitmap DMA */
#define BTOP		3	/* 1 plane bitmap to Qbus DMA */

/* DMA done notification code */

#define FIFO_EMPTY	0x01	/* DONE when FIFO becomes empty */
#define COUNT_ZERO	0x02	/* DONE when count becomes zero */
#define WORD_PACK	0x04    /* program the gate array for word packing */
#define BYTE_PACK	0x08	/* program gate array for byte packing */
#define REQUEST_DONE	0x100	/* clear when driver has processed request */
#define HARD_ERROR	0x200   /* DMA hardware error occurred */

/* DMA request queue is a ring buffer of request structures */

	struct DMAreq_header {

	    int QBAreg;		    /* cookie Qbus map reg for this buffer */
	    short status;	    /* master DMA status word */
	    int shared_size;	    /* size of shared memory in bytes */
	    struct DMAreq *DMAreq;  /* start address of request queue */
	    int used;		    /* # of queue entries currently used */
	    int size;		    /* # of "DMAreq"'s in the request queue */
	    int oldest;		    /* index to oldest queue'd request */
	    int newest;		    /* index to newest queue'd request */
	};

/* bit definitions for DMAstatus word in DMAreq_header */

#define	DMA_ACTIVE	0x0004		/* DMA in progress */
#define DMA_ERROR	0x0080		/* DMA hardware error */
#define DMA_IGNORE	0x0002		/* flag to ignore this interrupt */

/*------------------------------------------
* macros for DMA request queue fiddling  */

	/* DMA status set/check macros */

#define DMA_SETACTIVE(header)   ((header)->status |= DMA_ACTIVE)
#define DMA_CLRACTIVE(header)	((header)->status &= ~DMA_ACTIVE)
#define DMA_ISACTIVE(header)    ((header)->status & DMA_ACTIVE)

#define DMA_SETERROR(header)    ((header)->status |= DMA_ERROR)
#define DMA_CLRERROR(header)    ((header)->status &= ~DMA_ERROR)
#define DMA_ISERROR(header)     ((header)->status & DMA_ERROR)

#define DMA_SETIGNORE(header)	((header)->status |= DMA_IGNORE)
#define DMA_CLRIGNORE(header)   ((header)->status &= ~DMA_IGNORE)
#define DMA_ISIGNORE(header)    ((header)->status & DMA_IGNORE)

	/* yields TRUE if queue is empty (ISEMPTY) or full (ISFULL) */

#define DMA_ISEMPTY(header)	((header)->used == 0)
#define DMA_ISFULL(header)	((header)->used >= (header)->size)

	/* returns address of the billet for next (PUT)
	 * or oldest (GET) request */

#define DMA_PUTBEGIN(header)	(&(header)->DMAreq[(header)->newest])
#define DMA_GETBEGIN(header)  	(&(header)->DMAreq[(header)->oldest])

	/* does queue access pointer maintenance */

#define DMA_GETEND(header)      (++(header)->oldest >= (header)->size    \
				  ? (header)->oldest = 0 : 0);		 \
				--(header)->used;

#define DMA_PUTEND(header)     	(++(header)->newest >= (header)->size    \
				  ? (header)->newest = 0 : 0);		 \
				++(header)->used;

/******************************************************************
*
*	COLOR MAP WRITE BUFFER DEFINITIONS
*
******************************************************************/

	struct rgb {

	    unsigned char offset;	/* color map address for load */
	    unsigned char red;		/* data for red map */
	    unsigned char green;	/* data for green map */
	    unsigned char blue;		/* data for blue map */
	};

	struct color_buf {

	    char status;		/* load request/service status */
	    short count;		/* number of entries to br loaded */
	    struct rgb rgb[256];
	};

#define LOAD_COLOR_MAP	0x0001

/******************************************************************
*
*	SCROLL ASSIST DEFINITIONS
*
******************************************************************/

	struct scroll {

	    short status;
	    short viper_constant;
	    short y_scroll_constant;
	    short y_offset;
	    short x_index_pending;
	    short y_index_pending;
	};

#define LOAD_REGS	0x0001
#define LOAD_INDEX	0x0002

/******************************************************************
*
*	MOUSE/TABLET/KBD PROGRAMMING DEFINITIONS
*
******************************************************************/

/*-----------------------------------
* LK201 programmming definitions  */

#define LK_UPDOWN 	0x86		/* bits for setting lk201 modes */
#define LK_AUTODOWN 	0x82
#define LK_DOWN 	0x80
#define LK_DEFAULTS 	0xD3		/* reset (some) default settings */
#define LK_AR_ENABLE 	0xE3		/* global auto repeat enable */
#define LK_CL_ENABLE 	0x1B		/* keyclick enable */
#define LK_KBD_ENABLE 	0x8B		/* keyboard enable */
#define LK_BELL_ENABLE 	0x23		/* the bell */
#define LK_RING_BELL 	0xA7		/* ring keyboard bell */

#define LK_LED_ENABLE 	0x13		/* light led */
#define LK_LED_DISABLE 	0x11		/* turn off led */
#define LED_1 		0x81		/* led bits */
#define LED_2 		0x82
#define LED_3 		0x84
#define LED_4 		0x88
#define LED_ALL 	0x8F
#define LK_LED_HOLD	LED_4
#define LK_LED_LOCK	LED_3
#define LK_LED_COMPOSE	LED_2
#define LK_LED_WAIT 	LED_1

#define LK_KDOWN_ERROR	0x3D		/* key down on powerup error */
#define LK_POWER_ERROR 	0x3E		/* keyboard failure on powerup test */
#define LK_OUTPUT_ERROR	0xB5		/* keystrokes lost during inhibit */
#define LK_INPUT_ERROR 	0xB6		/* garbage command to keyboard */
#define LK_LOWEST	0x56		/* lowest significant keycode */
#define LK_DIV6_START	0xAD		/* start of div 6 */
#define LK_DIV5_END	0xB2		/* end of div 5 */

#define LAST_PARAM	0x80		/* "no more params" bit */

	struct prgkbd {

	    short cmd;			/* LK201 command opcode */
	    short param1;		/* 1st cmd parameter (can be null) */
	    short param2;		/* 2nd cmd parameter (can be null) */
	};

/*-------------------------
* "special" LK-201 keys */

#define SHIFT		174
#define LOCK		176
#define REPEAT		180
#define CNTRL		175
#define ALLUP		179

/*--------------------------------
* cursor programming structure */

	struct prg_cursor {

	    unsigned short acc_factor;	/* cursor aceleration factor */
	    unsigned short threshold;	/* threshold to trigger acc at */
	};

/*---------------------
* mouse definitions */

#define INC_STREAM_MODE	'R'		/* stream mode reports (55 hz) */
#define PROMPT_MODE	'D'		/* report when prompted */
#define REQUEST_POS	'P'		/* request position report */
#define SELF_TEST	'T'		/* request self test */

#define MOUSE_ID	0x2		/* mouse ID in lo 4 bits */

#define START_FRAME	0x80		/* start of report frame bit */
#define X_SIGN		0x10		/* position sign bits */
#define Y_SIGN		0x08

#define RIGHT_BUTTON	0x01		/* mouse buttons */
#define MIDDLE_BUTTON	0x02
#define LEFT_BUTTON	0x04

	/* mouse report structure definition */

	struct mouse_report {

	    char state;		/* buttons and sign bits */
	    short dx;		/* delta X since last change */
	    short dy;		/* delta Y since last change */
	    char bytcnt;	/* mouse report byte count */
	};

/*-----------------------------------------
* tablet command/interface definitions  */

#define T_STREAM	'R'		/* continuous stream report mode */
#define T_POINT	 	'D'		/* enter report-on-request mode */
#define T_REQUEST	'P'		/* request position report */

#define T_BAUD		'B'		/* increase baud to 9600 from 4800 */
#define T_RATE_55	'K'		/* report rate: 55/sec */
#define T_RATE_72	'L'		/* report rate: 72/sec */
#define T_RATE_120	'M'		/* report rate: 120/sec (9600 only) */

#define T_TEST		SELF_TEST	/* do self test */

#define TABLET_ID	0x4		/* tablet ID in lo 4 bits */

#define T_START_FRAME	0x80		/* start of report frame bit */
#define T_PROXIMITY	0x01		/* state pointer in proximity */

#define T_LEFT_BUTTON	0x02		/* puck buttons */
#define T_FRONT_BUTTON	0x04
#define T_RIGHT_BUTTON	0x08
#define T_BACK_BUTTON	0x10

#define T_BARREL_BUTTON T_LEFT_BUTTON		/* stylus buttons */
#define T_TIP_BUTTON	T_FRONT_BUTTON

