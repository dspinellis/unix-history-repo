/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * 		@(#)qdcons.c	1.3  Berkeley  %G%
 *
 *	derived from: @(#)qdcons.c  4.1 (ULTRIX    11/23/87
 */

/************************************************************************
*
*	ULTRIX QDSS STANDALONE BOOT DEVICE DRIVER...
*	device driver to boot system with QDSS as console
*
*************************************************************************/
/************************************************************************
*									*
*			Copyright (c) 1985 by				*
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
*************************************************************************
* revision history: (should be moved into sccs comments)
*************************************************************************
*
* 09 oct 85  longo  added uVAXII console ROM cursor reset to bottom of
*		    the screen.  Also spruced up qdputc() & scroll_up()
* 02 oct 85  longo  changed references to ADDRESS to be ADDRESS_COMPLETE
* 23 aug 85  longo  changed I/O page CSR address to be 0x1F00
* 20 aug 85  longo  created
*
************************************************************************/

#include "../h/types.h"
#include "../vax/cpu.h"
#define KERNEL
#include "../vaxuba/qdioctl.h"
#include "../vaxuba/qevent.h"
#include "../vaxuba/qduser.h"
#include "../vaxuba/qdreg.h"
#undef KERNEL

/*-----------------------------------------------------------------------
* constants used to set VAX ROM's cursor to bottom the of the screen  */

#define NVR_ADRS	0x200B8024

#define CURRENT_ROW	0x4C	/* these are offsets to the ROM's scratch.. */
#define ROW_MIN		0x4D    /* ..RAM start adrs as picked up out of NVR */
#define ROW_MAX		0x4E
#define CURRENT_COL	0x50
#define COL_MIN		0x51
#define COL_MAX		0x52

/*----------------------------------------
* LK201 keyboard state tracking struct */

	struct q_keyboard {

	    int shift;			/* state variables	*/
	    int cntrl;
	    int lock;
	    char last;			/* last character	*/

	 } q_keyboard;

	int qdputc(), qdgetc();

	extern (*v_putc)(),(*v_getc)();

/*----------------------------
* general purpose defines  */

#define BAD	-1
#define GOOD	0

/*----------------------------------------------
* console cursor bitmap (block cursor type)  */

	short cons_cursor[32] = {      /* white block cursor */

 /* A */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
 	 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
 /* B */ 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF,
         0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF, 0x00FF

	};

/*-------------------------------------
* constants used in font operations */

#define CHARS		95			/* # of chars in the font */
#define CHAR_HEIGHT	15			/* char height in pixels */
#define CHAR_WIDTH	8			/* char width in pixels*/
#define FONT_WIDTH	(CHAR_WIDTH * CHARS)    /* font width in pixels */
#define ROWS  		CHAR_HEIGHT

#define FONT_X		0			/* font's off screen adrs */
#define FONT_Y		(2047 - CHAR_HEIGHT) 
/*
#define FONT_Y		200
*/

	extern char q_font[];		/* reference font object code */

	extern  char q_key[];		/* reference key xlation tables */
	extern  char q_shift_key[];
	extern  char *q_special[];

/*----------------------------
* console cursor structure */

	struct cons_cur {
	    int x;
	    int y;
	} cursor;

/*------------------------------------------
* MicroVAX-II q-bus addressing constants */

#define QMEMBASE 0x30000000
#define QDSSCSR  0x20001F00

#define CHUNK     (64 * 1024)
#define QMEMSIZE  (1024 * 1024 * 4)
#define	QDBASE    (QMEMBASE + QMEMSIZE - CHUNK)

/*------------------------------------------------------------------
* QDSS register address offsets from start of QDSS address space */

#define QDSIZE 	 (52 * 1024)	/* size of entire QDSS foot print */

#define TMPSIZE  (16 * 1024)	/* template RAM is 8k SHORT WORDS */
#define TMPSTART 0x8000		/* offset of template RAM from base adrs */

#define REGSIZE	 (5 * 512)	/* regs touch 2.5k (5 pages) of addr space */
#define REGSTART 0xC000		/* offset of reg pages from base adrs */

#define ADDER	(REGSTART+0x000)
#define DGA	(REGSTART+0x200)
#define DUART	(REGSTART+0x400)
#define MEMCSR  (REGSTART+0x800)

#define	CLRSIZE  (3 * 512)		/* color map size */
#define CLRSTART (REGSTART+0xA00)	/* color map start offset from base */
					/*  0x0C00 really */
#define RED	(CLRSTART+0x000)
#define BLUE	(CLRSTART+0x200)
#define GREEN	(CLRSTART+0x400)

/*---------------------------------------
* QDSS register address map structure */

	struct qdmap qdmap;

/************************************************************************
*************************************************************************
*************************************************************************
*
*	EXTERNALLY CALLED ROUTINES START HERE:
*
*************************************************************************
*************************************************************************
************************************************************************/

/************************************************************************
*
*	qd_init()... init the QDSS into a physical memory system
*
************************************************************************/

qd_init()
{
	register char *ROM_console;
	register short *NVR;
	register int i;

	caddr_t qdaddr;
	struct dga *dga;

 	qdaddr = (caddr_t) QDSSCSR;
        if (badaddr(qdaddr, sizeof(short)))
            return(0);

	*(short *)qdaddr = (short) (QDBASE >> 16);

/*----------------------------------------------------------------------
* load qdmap struct with the physical addresses of the QDSS elements */

	qdmap.template = (caddr_t) QDBASE + TMPSTART;
	qdmap.adder = (caddr_t) QDBASE + ADDER;
	qdmap.dga = (caddr_t) QDBASE + DGA;
	qdmap.duart = (caddr_t) QDBASE + DUART;
	qdmap.memcsr = (caddr_t) QDBASE + MEMCSR;
	qdmap.red = (caddr_t) QDBASE + RED;
	qdmap.blue = (caddr_t) QDBASE + BLUE;
	qdmap.green = (caddr_t) QDBASE + GREEN;

/*--------------------------
* no interrupts allowed! */

	dga = (struct dga *) qdmap.dga;
	dga->csr = HALT;
	dga->csr |= CURS_ENB;

/*----------------------------
* init the default values  */

	q_keyboard.shift = 0;		/* init keyboard state tracking */
	q_keyboard.lock = 0;
	q_keyboard.cntrl = 0;
	q_keyboard.last = 0;

	cursor.x = 0;			/* init cursor to top left */
	cursor.y = 0;

	set_defaults();		        /* setup the default device */
	ldfont();			/* PtoB the font into off-screen */

/*--------------------------------------------------------------------
* tell the VAX ROM that the cursor is at the bottom of the screen  */

	NVR = (short *) NVR_ADRS;

	i = *NVR++ & 0xFF;
	i |= (*NVR++ & 0xFF) << 8;
	i |= (*NVR++ & 0xFF) << 16;
	i |= (*NVR++ & 0xFF) << 24;

	ROM_console = (char *) i;

	ROM_console[CURRENT_COL] = ROM_console[COL_MIN];
	ROM_console[CURRENT_ROW] = ROM_console[ROW_MAX];

/*----------------------------------------------------------
* smash system virtual console service routine addresses */

	v_getc = qdgetc;
	v_putc = qdputc;

	return(1);

} /* qd_init */

/******************************************************************* 
*
*	qdputc()... output a character to the QDSS screen
*
********************************************************************
*
*	calling convention:
*
*		qdputc(chr);
*		char chr; 		;character to be displayed
*
********/

qdputc(chr)
char chr;
{
	register struct adder *adder;
	register struct dga *dga;
	register int i;

	short x;

	adder = (struct adder *) qdmap.adder;
	dga = (struct dga *) qdmap.dga;

/*---------------------------
* non display character?  */

	chr &= 0x7F;

	switch (chr) {

	    case '\r':			/* return char */
	        cursor.x = 0;
		dga->x_cursor = TRANX(cursor.x);
	    	return(0);

	    case '\t':			/* tab char */

	    	for (i = 8 - ((cursor.x >> 3) & 0x07); i > 0; --i) {
	    	    qdputc(' ');
		}
		return(0);

	    case '\n':			/* line feed char */

	        if ((cursor.y += CHAR_HEIGHT) > (863 - CHAR_HEIGHT)) {
		    cursor.y -= CHAR_HEIGHT;
		    scroll_up(adder);
		}
		dga->y_cursor = TRANY(cursor.y);
		return(0);

	    case '\b':			/* backspace char */
	        if (cursor.x > 0) {
	    	    cursor.x -= CHAR_WIDTH;
		    qdputc(' ');
		    cursor.x -= CHAR_WIDTH;
		    dga->x_cursor = TRANX(cursor.x);
		}
		return(0);

	    default:
		if (chr < ' ' || chr > '~') {
		    return(0);
		}
	}

/*------------------------------------------
* setup VIPER operand control registers  */

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

/*----------------------------------------
* load DESTINATION origin and vectors  */

	adder->fast_dest_dy = 0;
	adder->slow_dest_dx = 0;
	adder->error_1 = 0;
	adder->error_2 = 0;

	adder->rasterop_mode = DST_WRITE_ENABLE | NORMAL;

	wait_status(adder, RASTEROP_COMPLETE);

	adder->destination_x = cursor.x;
	adder->fast_dest_dx = CHAR_WIDTH;

	adder->destination_y = cursor.y;
	adder->slow_dest_dy = CHAR_HEIGHT;

/*-----------------------------------
* load SOURCE origin and vectors  */

	adder->source_1_x = FONT_X + ((chr - ' ') * CHAR_WIDTH);
	adder->source_1_y = FONT_Y;

	adder->source_1_dx = CHAR_WIDTH;
	adder->source_1_dy = CHAR_HEIGHT;

	write_ID(adder, LU_FUNCTION_R1, FULL_SRC_RESOLUTION | LF_SOURCE);
	adder->cmd = RASTEROP | OCRB | 0 | S1E | DTE;

/*-------------------------------------
* update console cursor coordinates */

	cursor.x += CHAR_WIDTH;
	dga->x_cursor = TRANX(cursor.x);

        if (cursor.x > (1024 - CHAR_WIDTH)) {
	    qdputc('\r');
	    qdputc('\n');
	}

} /* qdputc */ 

/******************************************************************* 
*
*	qdgetc()... get a character from the LK201
*
*******************************************************************/

qdgetc()
{
	register short key;
	register char chr;
	register struct duart *duart;

	u_int status;

	duart = (struct duart *) qdmap.duart;

	/*--------------------------------------
	* Get a character from the keyboard. */

LOOP:
	while (!((status = duart->statusA) & RCV_RDY))
			;

	key = duart->dataA;
	key &= 0xFF;

	/*--------------------------------------
	* Check for various keyboard errors  */

	if( key == LK_POWER_ERROR || key == LK_KDOWN_ERROR ||
	    key == LK_INPUT_ERROR || key == LK_OUTPUT_ERROR) {
		printf("Keyboard error, code = %x\n", key);
		return(0);
	}

	if (key < LK_LOWEST) 
	    return(0);

	/*---------------------------------
	* See if its a state change key */

	switch (key) {

	    case LOCK:
		q_keyboard.lock ^= 0xffff;	/* toggle */
		if (q_keyboard.lock)
		    led_control(LK_LED_ENABLE, LK_LED_LOCK);
		else
		    led_control(LK_LED_DISABLE, LK_LED_LOCK);
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

	    /*-------------------------------------------------------
	    * Test for cntrl characters. If set, see if the character
	    * is elligible to become a control character. */

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

	if (chr < ' ' && chr > '~')  	/* if input is non-displayable */
	    return(0);			/* ..then pitch it! */

	q_keyboard.last = chr;

	/*-----------------------------------
	* Check for special function keys */

	if (chr & 0x80) 		/* pitch the function keys */
	    return(0);
	else
	    return(chr);

} /* qdgetc */

/************************************************************************
*************************************************************************
*************************************************************************
*
*	INTERNALLY USED ROUTINES START HERE:
*
*************************************************************************
*************************************************************************
************************************************************************/

/********************************************************************
*
*	ldcursor()... load the mouse cursor's template RAM bitmap
*
********************************************************************/

ldcursor()
{
	register struct dga *dga;
	register short *temp;
	register int i;

	int cursor;

	dga = (struct dga *) qdmap.dga;
	temp = (short *) qdmap.template;

	temp += (8 * 1024) - 32;	/* cursor is 32 WORDS from the end */
					/* ..of the 8k WORD template space */
	for (i = 0; i < 32; ++i)
	    *temp++ = cons_cursor[i];

	return(0);

} /* ldcursor */

/**********************************************************************
*
*	ldfont()... put the console font in the QDSS off-screen memory
*
**********************************************************************/

ldfont()
{
	register struct adder *adder;

	int i;		/* scratch variables */
	int j;
	int k;
	short packed;

	adder = (struct adder *) qdmap.adder;

/*------------------------------------------
* setup VIPER operand control registers  */

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

/*--------------------------
* load destination data  */

	wait_status(adder, RASTEROP_COMPLETE);

	adder->destination_x = FONT_X;
	adder->destination_y = FONT_Y;
	adder->fast_dest_dx = FONT_WIDTH;
	adder->slow_dest_dy = CHAR_HEIGHT;

/*---------------------------------------
* setup for processor to bitmap xfer  */

	write_ID(adder, CS_UPDATE_MASK, 0x0001);
	adder->cmd = PBT | OCRB | 2 | DTE | 2;

/*-----------------------------------------------
* iteratively do the processor to bitmap xfer */

	for (i = 0; i < ROWS; ++i) {

	    /* PTOB a scan line */

	    for (j = 0, k = i; j < 48; ++j) { 

	        /* PTOB one scan of a char cell */

		packed = q_font[k];
		k += ROWS;
		packed |= ((short)q_font[k] << 8);
		k += ROWS;

	        wait_status(adder, TX_READY);
	        adder->id_data = packed;
	    }
	}

}  /* ldfont */

/*********************************************************************
*
*	led_control()... twiddle LK-201 LED's
*
**********************************************************************
*
*	led_control(cmd, led_mask);
*	int cmd;	LED enable/disable command
*	int led_mask;	which LED(s) to twiddle
*
*************/

led_control(cmd, led_mask)
int cmd;
int led_mask;
{
	register int i;
	register int status;
	register struct duart *duart;

	duart = (struct duart *) qdmap.duart;

	for (i = 1000; i > 0; --i) {
    	    if ((status = duart->statusA) & XMT_RDY) {
        	duart->dataA = cmd;
		break;
    	    }
	}

	for (i = 1000; i > 0; --i) {
    	    if ((status = duart->statusA) & XMT_RDY) {
        	duart->dataA = led_mask;
		break;
    	    }
	}

	if (i == 0)
	    return(BAD);

	return(GOOD);

} /* led_control */

/******************************************************************* 
*
*	scroll_up()... move the screen up one character height
*
********************************************************************
*
*	calling convention:
*
*		scroll_up(adder);
*		struct adder *adder;    ;address of adder
*
********/

scroll_up(adder)
register struct adder *adder;
{

/*------------------------------------------
* setup VIPER operand control registers  */

	wait_status(adder, ADDRESS_COMPLETE);

	write_ID(adder, CS_UPDATE_MASK, 0x00FF);  /* select all planes */

	write_ID(adder, MASK_1, 0xFFFF);
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 255);
	write_ID(adder, VIPER_Z_LOAD | BACKGROUND_COLOR_Z, 0);

	write_ID(adder, SRC1_OCR_B, 
			EXT_NONE | INT_SOURCE | ID | BAR_SHIFT_DELAY);
	write_ID(adder, DST_OCR_B, 
			EXT_NONE | INT_NONE | NO_ID | NO_BAR_SHIFT_DELAY);

/*----------------------------------------
* load DESTINATION origin and vectors  */

	adder->fast_dest_dy = 0;
	adder->slow_dest_dx = 0;
	adder->error_1 = 0;
	adder->error_2 = 0;

	adder->rasterop_mode = DST_WRITE_ENABLE | NORMAL;

	adder->destination_x = 0;
	adder->fast_dest_dx = 1024;

	adder->destination_y = 0;
	adder->slow_dest_dy = 864 - CHAR_HEIGHT;

/*-----------------------------------
* load SOURCE origin and vectors  */

	adder->source_1_x = 0;
	adder->source_1_dx = 1024;

	adder->source_1_y = 0 + CHAR_HEIGHT;
	adder->source_1_dy = 864 - CHAR_HEIGHT;

	write_ID(adder, LU_FUNCTION_R1, FULL_SRC_RESOLUTION | LF_SOURCE);
	adder->cmd = RASTEROP | OCRB | 0 | S1E | DTE;

/*--------------------------------------------
* do a rectangle clear of last screen line */

	write_ID(adder, MASK_1, 0xffff);
	write_ID(adder, SOURCE, 0xffff);
	write_ID(adder,DST_OCR_B,  
	  	(EXT_NONE | INT_NONE | NO_ID | NO_BAR_SHIFT_DELAY));
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 0);
	adder->error_1 = 0;
	adder->error_2 = 0;
	adder->slow_dest_dx = 0;	/* set up the width of	*/
	adder->slow_dest_dy = CHAR_HEIGHT;	/* rectangle */

	adder->rasterop_mode = (NORMAL | DST_WRITE_ENABLE) ;
	wait_status(adder, RASTEROP_COMPLETE);
	adder->destination_x = 0;
	adder->destination_y = 864 - CHAR_HEIGHT;

	adder->fast_dest_dx = 1024;	/* set up the height	*/
	adder->fast_dest_dy = 0;	/* of rectangle		*/

	write_ID(adder, LU_FUNCTION_R2, (FULL_SRC_RESOLUTION | LF_SOURCE));
	adder->cmd = (RASTEROP | OCRB | LF_R2 | DTE ) ;

} /* scroll_up */ 

/**********************************************************************
*
*	set_defaults()... init the QDSS device and driver defaults
*
**********************************************************************/

set_defaults()
{
	setup_input();		/* init the DUART */
	setup_dragon();		/* init the ADDER/VIPER stuff */
	ldcursor();		/* load default cursor map */

} /* set_defaults */

/*********************************************************************
*
*	setup_dragon()... init the ADDER, VIPER, bitmaps, & color map
*
*********************************************************************/

setup_dragon()
{

	register struct adder *adder;
	register struct dga *dga;
	short *memcsr;

	int i;			/* general purpose variables */
	int status;

	short top;		/* clipping/scrolling boundaries */
	short bottom;
	short right;
	short left;

	short *red;		/* color map pointers */
	short *green;
	short *blue;

/*------------------
* init for setup */

	adder = (struct adder *) qdmap.adder;
	dga = (struct dga *) qdmap.dga;
	memcsr = (short *) qdmap.memcsr;
	
	*memcsr = SYNC_ON;		/* blank screen and turn off LED's */
	adder->command = CANCEL;

/*----------------------
* set monitor timing */

	adder->x_scan_count_0 = 0x2800;
	adder->x_scan_count_1 = 0x1020;
	adder->x_scan_count_2 = 0x003A;
	adder->x_scan_count_3 = 0x38F0;
	adder->x_scan_count_4 = 0x6128;
	adder->x_scan_count_5 = 0x093A;
	adder->x_scan_count_6 = 0x313C;
	adder->sync_phase_adj = 0x0100;
	adder->x_scan_conf = 0x00C8;

/*---------------------------------------------------------
* got a bug in secound pass ADDER! lets take care of it */

	/* normally, just use the code in the following bug fix code, but to 
	* make repeated demos look pretty, load the registers as if there was
	* no bug and then test to see if we are getting sync */

	adder->y_scan_count_0 = 0x135F;
	adder->y_scan_count_1 = 0x3363;
	adder->y_scan_count_2 = 0x2366;
	adder->y_scan_count_3 = 0x0388;

	/* if no sync, do the bug fix code */

	if (wait_status(adder, VSYNC) == BAD) {

	    /* first load all Y scan registers with very short frame and
	    * wait for scroll service.  This guarantees at least one SYNC 
	    * to fix the pass 2 Adder initialization bug (synchronizes
	    * XCINCH with DMSEEDH) */

	    adder->y_scan_count_0 = 0x01;
	    adder->y_scan_count_1 = 0x01;
	    adder->y_scan_count_2 = 0x01;
	    adder->y_scan_count_3 = 0x01;

	    wait_status(adder, VSYNC);	/* delay at least 1 full frame time */
	    wait_status(adder, VSYNC);

	    /* now load the REAL sync values (in reverse order just to
	    *  be safe.  */

	    adder->y_scan_count_3 = 0x0388;
	    adder->y_scan_count_2 = 0x2366;
	    adder->y_scan_count_1 = 0x3363;
	    adder->y_scan_count_0 = 0x135F;
  	}

/*----------------------------
* zero the index registers */

	adder->x_index_pending = 0;
	adder->y_index_pending = 0;
	adder->x_index_new = 0;
	adder->y_index_new = 0;
	adder->x_index_old = 0;
	adder->y_index_old = 0;

	adder->pause = 0;

/*----------------------------------------
* set rasterop mode to normal pen down */

	adder->rasterop_mode = DST_WRITE_ENABLE | DST_INDEX_ENABLE | NORMAL;

/*--------------------------------------------------
* set the rasterop registers to a default values */

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

/*------------------------
* scale factor = unity */

	adder->fast_scale = UNITY;
	adder->slow_scale = UNITY;

/*-------------------------------
* set the source 2 parameters */

	adder->source_2_x = 0;
	adder->source_2_y = 0;
	adder->source_2_size = 0x0022;

/*-----------------------------------------------
* initialize plane addresses for eight vipers */

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

	/* initialize the external registers. */

	write_ID(adder, CS_UPDATE_MASK, 0x00FF);
	write_ID(adder, CS_SCROLL_MASK, 0x00FF);

	/* initialize resolution mode */

	write_ID(adder, MEMORY_BUS_WIDTH, 0x000C);     /* bus width = 16 */
	write_ID(adder, RESOLUTION_MODE, 0x0000);      /* one bit/pixel */

	/* initialize viper registers */

	write_ID(adder, SCROLL_CONSTANT, SCROLL_ENABLE|VIPER_LEFT|VIPER_UP);
	write_ID(adder, SCROLL_FILL, 0x0000);

/*----------------------------------------------------
* set clipping and scrolling limits to full screen */

	for ( i = 1000, adder->status = 0
	    ; i > 0  &&  !((status = adder->status) & ADDRESS_COMPLETE)
	    ; --i);

	if (i == 0)
	    printf("timeout trying to setup clipping\n");

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

	wait_status(adder, VSYNC);	/* wait at LEAST 1 full frame */
	wait_status(adder, VSYNC);

	adder->x_index_pending = left;
	adder->y_index_pending = top;
	adder->x_index_new = left;
	adder->y_index_new = top;
	adder->x_index_old = left;
	adder->y_index_old = top;

	for ( i = 1000, adder->status = 0
	    ; i > 0  &&  !((status = adder->status) & ADDRESS_COMPLETE)
	    ; --i);

	if (i == 0)
	    printf("timeout waiting for ADDRESS_COMPLETE bit\n");

	write_ID(adder, LEFT_SCROLL_MASK, 0x0000);
	write_ID(adder, RIGHT_SCROLL_MASK, 0x0000);

/*------------------------------------------------------------
* set source and the mask register to all ones (ie: white) */

	write_ID(adder, SOURCE, 0xFFFF);
	write_ID(adder, MASK_1, 0xFFFF);
	write_ID(adder, VIPER_Z_LOAD | FOREGROUND_COLOR_Z, 255);
	write_ID(adder, VIPER_Z_LOAD | BACKGROUND_COLOR_Z, 0);

/*--------------------------------------------------------------
* initialize Operand Control Register banks for fill command */

	write_ID(adder, SRC1_OCR_A, EXT_NONE | INT_M1_M2  | NO_ID | WAIT);
	write_ID(adder, SRC2_OCR_A, EXT_NONE | INT_SOURCE | NO_ID | NO_WAIT);
	write_ID(adder, DST_OCR_A, EXT_NONE | INT_NONE   | NO_ID | NO_WAIT);

	write_ID(adder, SRC1_OCR_B, EXT_NONE | INT_SOURCE | NO_ID | WAIT);
	write_ID(adder, SRC2_OCR_B, EXT_NONE | INT_M1_M2  | NO_ID | NO_WAIT);
	write_ID(adder, DST_OCR_B, EXT_NONE | INT_NONE | NO_ID | NO_WAIT);

/*------------------------------------------------------------------
* init Logic Unit Function registers, (these are just common values,
* and may be changed as required).  */

	write_ID(adder, LU_FUNCTION_R1, FULL_SRC_RESOLUTION | LF_SOURCE);
	write_ID(adder, LU_FUNCTION_R2, FULL_SRC_RESOLUTION | LF_SOURCE | INV_M1_M2);
	write_ID(adder, LU_FUNCTION_R3, FULL_SRC_RESOLUTION | LF_D_OR_S);
	write_ID(adder, LU_FUNCTION_R4, FULL_SRC_RESOLUTION | LF_D_XOR_S);

/*----------------------------------------
* load the color map for black & white */
	
	for ( i = 0, adder->status = 0
	    ; i < 10000  &&  !((status = adder->status) & VSYNC)
	    ; ++i);

	if (i == 0)
	    printf("timeout waiting for VSYNC bit\n");

	red = (short *) qdmap.red;
	green = (short *) qdmap.green;
	blue = (short *) qdmap.blue;

	*red++ = 0x00;			/* black */
	*green++ = 0x00;
	*blue++ = 0x00;

	*red-- = 0xFF;			/* white */
	*green-- = 0xFF;
	*blue-- = 0xFF;

	/*----------------------------------
	* set color map for mouse cursor */

	red += 254;
	green += 254;
	blue += 254;

	*red++ = 0x00;			/* black */
	*green++ = 0x00;
	*blue++ = 0x00;

	*red = 0xFF;			/* white */
	*green = 0xFF;
	*blue = 0xFF;

/*---------------------------------------------------------------------------
* clear the bitmap a piece at a time.  Since the fast scroll clear only clears
* the current displayed portion of the bitmap put a temporary value in the y
* limit register so we can access whole bitmap  */

	adder->x_limit = 1024;
	adder->y_limit = 2048 - CHAR_HEIGHT;
	adder->y_offset_pending = 0;

	wait_status(adder, VSYNC);	/* wait at LEAST 1 full frame */
	wait_status(adder, VSYNC);

	adder->y_scroll_constant = SCROLL_ERASE;

	wait_status(adder, VSYNC);
	wait_status(adder, VSYNC);

	adder->y_offset_pending = 864;

	wait_status(adder, VSYNC);
	wait_status(adder, VSYNC);

	adder->y_scroll_constant = SCROLL_ERASE;

	wait_status(adder, VSYNC);
	wait_status(adder, VSYNC);

	adder->y_offset_pending = 1728;

	wait_status(adder, VSYNC);
	wait_status(adder, VSYNC);

	adder->y_scroll_constant = SCROLL_ERASE;

	wait_status(adder, VSYNC);
	wait_status(adder, VSYNC);

	adder->y_offset_pending = 0;     /* back to normal */

	wait_status(adder, VSYNC);
	wait_status(adder, VSYNC);

	adder->x_limit = MAX_SCREEN_X;
	adder->y_limit = MAX_SCREEN_Y + FONT_HEIGHT;

	*memcsr = SYNC_ON | UNBLANK;	/* turn off leds and turn on video */
	return(0);

} /* setup_dragon */

/******************************************************************
*
*	setup_input()... init the DUART and set defaults in input
*			 devices
*
******************************************************************/

setup_input()
{
	register struct duart *duart;	/* DUART register structure pointer */
	register int bits;
	int i, j;			/* scratch variables */

	short status;

/*---------------
* init stuff */

	duart = (struct duart *) qdmap.duart;

/*---------------------------------------------
* setup the DUART for kbd & pointing device */

	duart->cmdA = RESET_M;    /* reset mode reg ptr for kbd */
	duart->modeA = 0x13;	  /* 8 bits, no parity, rcv IE, */
	 			  /* no RTS control,char error mode */
	duart->modeA = 0x07;	  /* 1 stop bit,CTS does not IE XMT */
				  /* no RTS control,no echo or loop */
	duart->auxctl = 0x00;	  /* baud rate set 1 */

	duart->clkselA = 0x99;	  /* 4800 baud for kbd */

        /* reset everything for keyboard */

	for (bits = RESET_M; bits < START_BREAK; bits += 0x10)
	    duart->cmdA = bits;

	duart->cmdA = EN_RCV | EN_XMT; /* enbl xmt & rcv for kbd */

/*--------------------------
* init keyboard defaults */
/*
	for (i = 500; i > 0; --i) {
    	    if ((status = duart->statusA) & XMT_RDY) {
        	duart->dataA = LK_DEFAULTS;
		break;
    	    }
	}

	for (j = 0; j < 3; ++j) {
	    for (i = 50000; i > 0; --i) {
    	        if ((status = duart->statusA) & RCV_RDY) {
		    status = duart->dataA;
		    break;
 	        }
	    }
	}

	if (i == 0)
	    printf("LK-201 init error\n");
*/

/*--------
* exit */

	return(0);

} /* setup_input */

/**********************************************************************
*
*	wait_status()... delay for at least one display frame time
*
***********************************************************************
*
*	calling convention:
*
*		wait_status(adder, mask);
*		struct *adder adder;
*		int mask;
*
*	return: BAD means that we timed out without ever seeing the
*	              vertical sync status bit
*		GOOD otherwise
*
**************/

wait_status(adder, mask)
register struct adder *adder;
register int mask;
{
	register short status;
	int i;

	for ( i = 10000, adder->status = 0
	    ; i > 0  &&  !((status = adder->status) & mask)
	    ; --i);

	if (i == 0) {
	    printf("timeout polling for 0x%x in adder->status\n", mask);
	    return(BAD);
	}

	return(GOOD);

} /* wait_status */

/**********************************************************************
*
*	write_ID()... write out onto the ID bus
*
***********************************************************************
*
*	calling convention:
*
*		struct *adder adder;	;pntr to ADDER structure
*		short adrs;		;VIPER address
*		short data;		;data to be written
*		write_ID(adder);
*
*	return: BAD means that we timed out waiting for status bits
*		      VIPER-access-specific status bits
*		GOOD otherwise
*
**************/

write_ID(adder, adrs, data)
register struct adder *adder;
register short adrs;
register short data;
{
	int i;
	short status;

	for ( i = 100000, adder->status = 0
	    ; i > 0  &&  !((status = adder->status) & ADDRESS_COMPLETE)
	    ; --i);

	if (i == 0)
	    goto ERR;

	for ( i = 100000, adder->status = 0
	    ; i > 0  &&  !((status = adder->status) & TX_READY)
	    ; --i);

	if (i > 0) {
	    adder->id_data = data;
	    adder->command = ID_LOAD | adrs;
	    return(GOOD);
	}

ERR:
	printf("timeout trying to write to VIPER\n");
	return(BAD);

} /* write_ID */
