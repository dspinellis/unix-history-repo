/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * 		@(#)qvcons.c	7.3  Berkeley  %G%
 *
 *	derived from: @(#)qvcons.c	4.1 11/23/87
 */

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
 *   This software is  derived  from  software  received  from  the	*
 *   University    of   California,   Berkeley,   and   from   Bell	*
 *   Laboratories.  Use, duplication, or disclosure is  subject  to	*
 *   restrictions  under  license  agreements  with  University  of	*
 *   California and with AT&T.						*
 *									*
 *   The information in this software is subject to change  without	*
 *   notice  and should not be construed as a commitment by Digital	*
 *   Equipment Corporation.						*
 *									*
 *   Digital assumes no responsibility for the use  or  reliability	*
 *   of its software on equipment which is not supplied by Digital.	*
 *									*
 ************************************************************************/

/* ---------------------------------------------------------------------
 * Modification History - moved to sccs log
 *
 *  7 Jul 84 --  rjl
 *	Initial version to support the qvss as the system console
 *	during the boot process.
 *
 * ---------------------------------------------------------------------
 */

#include "../h/types.h"
#define KERNEL
#include "../vaxuba/qvioctl.h"
#undef KERNEL
#include "../vax/cpu.h"

/*
 * MicroVAX-II q-bus memory base
 */
#define QMEMBASE 0x30000000
#define QVMAXEVQ	64
#define QVSSCSR 0x20001e80

/*
 * Screen initialization tables. qv_def_scn is used as an index into the
 * table to select the proper initialization parameters.
 */
int qv_def_scn = 1;			/* Screen initialization flag	*/

char	qv_scrn_15[]= {
	31,25,27,0142,31,13,30,31,4,15,040,0,0,0,0,0
};

char	qv_scrn_19s[]= {
	39,30,31,0264,55,5,54,54,4,15,040,0,0,0,0,0
};

char	*qv_init_tbl[]= {
	qv_scrn_15,
	qv_scrn_19s,
};

struct qv_info qv_scn_defaults[] = {
	{0, {0, 0}, 0, {0, 0}, 0, 0, 30, 80, 768, 480, 768-16, 480-16,
	 0, 0, 0, 0, 0, QVMAXEVQ, 0, 0, {0, 0}, {0, 0, 0, 0}, 2, 4},
	{0, {0, 0}, 0, {0, 0}, 0, 0, 55, 120, 960, 864, 960-16, 864-16,
	 0, 0, 0, 0, 0, QVMAXEVQ, 0, 0, {0, 0}, {0, 0, 0, 0}, 2, 4},
	{0, {0, 0}, 0, {0, 0}, 0, 0, 56, 120,1024, 864,1024-16, 864-16,
	 0, 0, 0, 0, 0, QVMAXEVQ, 0, 0, {0, 0}, {0, 0, 0, 0}, 2, 4}
};

struct qv_info  qv_scn;

struct qv_keyboard {
	int shift;			/* state variables	*/
	int cntrl;
	int lock;
	char last;			/* last character	*/
} qv_keyboard;

int qvputc(),qvgetc();

/*
 * Keyboard translation and font tables
 */
extern  char q_key[],q_shift_key[],*q_special[],q_font[];
extern	short q_cursor[];

extern (*v_putc)(),(*v_getc)();

/*
 * Routine called to init a qvss.
 */
qv_init()
{
	struct qvdevice *qvaddr = (struct qvdevice *)QVSSCSR;
	char *qvssmem;
	short *scanline;
	int i;
	short scan;
	char *ptr;
	extern int cpu;

        if( badaddr( qvaddr, sizeof(short) ) )
                return(0);

        if( qvaddr->qv_csr & QV_19INCH )
                qv_def_scn = 1;
        else
                qv_def_scn = 0;
        qv_scn = qv_scn_defaults[ qv_def_scn ];
	qv_scn.qvaddr = qvaddr;

	/*
	 * Initialize the screen.
	 */
	ptr = qv_init_tbl[ qv_def_scn ];
	for( i=0 ; i<16 ; i++ ) {
		qvaddr->qv_crtaddr = i;
		qvaddr->qv_crtdata = *ptr++;
	}

	/*
	 * Turn on the keyboard. 
	 */
	qvaddr->qv_uartcmd = 0x15;	/* set mode pntr/enable rx/tx	*/
	qvaddr->qv_uartmode = 0x17;	/* noparity, 8-bit		*/
	qvaddr->qv_uartmode = 0x07;	/* 1 stop bit			*/
	qvaddr->qv_uartstatus = 0x99;	/* 4800 baud xmit/recv 		*/

	qvssmem = (char *)((qvaddr->qv_csr & QV_MEM_BANK) << 7);
	if( cpu == VAX_630 )
		qvssmem += QMEMBASE;

	qv_scn.bitmap = qvssmem;
	qv_scn.scanmap = (short *)((int)qvssmem + ( 254 * 1024 ));
	qv_scn.cursorbits = (short *)((int)qvssmem + ( 256 * 1024 ) - 32);

	/*
	 * Setup the cursor.
	 */
	for( i=0 ; i<16 ; i++ )
		qv_scn.cursorbits[i] = q_cursor[i];

	/*
	 * Clear the bit map
	 */
	for( i=0 , ptr = qv_scn.bitmap ; i<254 ; i += 2 , ptr += 2048)
		bzero( ptr, 2048 );
	
	/*
	 * Reinitialize the scanmap
	 */
	scan = qv_scn.qvaddr->qv_csr & QV_MEM_BANK;
	scanline = qv_scn.scanmap;
	for(i = 0 ; i < qv_scn.max_y ; i++ )
		*scanline++ = scan++;

	/*
	 * Home the cursor
	 */
	qv_scn.row = qv_scn.col = 0;

	/*
	 * Turn it on.
	 */
	v_getc = qvgetc;
	v_putc = qvputc;
	qvaddr->qv_csr |= QV_CUR_MODE | QV_VIDEO_ENA;
	return 1;
}

/*
 * Routine to display a character on the screen.  The model used is a 
 * glass tty.  It is assummed that the user will only use this emulation
 * during system boot and that the screen will be eventually controlled
 * by a window manager.
 */
qvputc( c )
char c;
{

	char *b_row, *f_row;
	int i, j;
	short *scanline;

	c &= 0x7f;

	switch ( c ) {
	case '\t':				/* tab		*/
		for( j = 8 - (qv_scn.col & 0x7) ; j > 0 ; j-- )
			qvputc( ' ' );
		break;

	case '\r':				/* return	*/
		qv_scn.col = 0;
		break;

	case '\010':				/* backspace	*/
		if( --qv_scn.col < 0 )
			qv_scn.col = 0;
		break;

	case '\n':				/* linefeed	*/
		if( qv_scn.row+1 >= qv_scn.max_row )
			qvscroll();
		else
			qv_scn.row++;
		break;

	case '\007':				/* bell		*/
		if( qv_scn.qvaddr )
			qv_key_out( LK_BELL_ENABLE );
		return;

	default:
		if( c >= ' ' && c <= '~' ) {
			scanline = qv_scn.scanmap;
			b_row = qv_scn.bitmap+(scanline[qv_scn.row*15]&0x3ff)*128+qv_scn.col;
			i = c - ' ';
			if( i < 0 || i > 95 )
				i = 0;
			else
				i *= 15;
			f_row = (char *)((int)q_font + i);
		
			for( i=0 ; i<15 ; i++ , b_row += 128, f_row++ )
				*b_row = *f_row;

			if( ++qv_scn.col >= qv_scn.max_col ) {
				qv_scn.col = 0 ;
				if( qv_scn.row+1 >= qv_scn.max_row )
					qvscroll();
				else
					qv_scn.row++;
			}
		}
		break;
	}
	/*
	 * Position the cursor to the next character location.
	 */
	qv_pos_cur( qv_scn.col*8, qv_scn.row*15 );
}

/*
 * Position the cursor to a particular spot.
 */
qv_pos_cur( x, y)
int x,y;
{
	struct qvdevice *qvaddr;

	if( qvaddr = qv_scn.qvaddr ) {
		if( y < 0 || y > qv_scn.max_cur_y )
			y = qv_scn.max_cur_y;
		if( x < 0 || x > qv_scn.max_cur_x )
			x = qv_scn.max_cur_x;

		qvaddr->qv_crtaddr = 10;	/* select cursor start reg */
		qvaddr->qv_crtdata = y & 0xf;
		qvaddr->qv_crtaddr = 11;	/* select cursor end reg */
		qvaddr->qv_crtdata = y & 0xf;
		qvaddr->qv_crtaddr = 14;	/* select cursor y pos. */
		qvaddr->qv_crtdata = y >> 4;
		qvaddr->qv_xcur = x;		/* pos x axis	*/
	}
}
/*
 * Scroll the bitmap by moving the scanline map words. This could
 * be done by moving the bitmap but it's much too slow for a full screen.
 * The only drawback is that the scanline map must be reset when the user 
 * wants to do graphics.
 */
qvscroll()
{
	int i;
	short tmpscanlines[15];
	char *b_row;
	short *scanline;


	/*
	 * Save the first 15 scanlines so that we can put them at
	 * the bottom when done.
	 */
	bcopy( qv_scn.scanmap, tmpscanlines, sizeof tmpscanlines );

	/*
	 * Clear the wrapping line so that it won't flash on the bottom
	 * of the screen.
	 */
	scanline = qv_scn.scanmap;
	b_row = qv_scn.bitmap+(*scanline&0x3ff)*128;
	bzero( b_row, 1920 );

	/*
	 * Now move the scanlines down 
	 */
	bcopy( qv_scn.scanmap+15, qv_scn.scanmap, (qv_scn.row * 15) * sizeof (short) );

	/*
	 * Now put the other lines back
	 */
	bcopy( tmpscanlines, qv_scn.scanmap+(qv_scn.row * 15), sizeof tmpscanlines );

}

/*
 * QVSS keyboard interrupt.
 */
qvgetc()
{
	int c;
	struct qvdevice *qvaddr;
	char *string;
	int j;

	qvaddr = qv_scn.qvaddr;
	/*
	 * Get a character from the keyboard.
	 */
loop:
	while( (qvaddr->qv_uartstatus & 0x01) == 0 )
		;
	j = qvaddr->qv_uartdata & 0xff;
	/*
	 * See if its a state change key
	 */
	switch ( j ) {
	case LOCK:
		qv_keyboard.lock ^= 0xffff;	/* toggle */
		if( qv_keyboard.lock )
			qv_key_out( LK_LED_ENABLE );
		else
			qv_key_out( LK_LED_DISABLE );
		qv_key_out( LED_3 );
		goto loop;
	case SHIFT:
		qv_keyboard.shift ^= 0xffff;
		goto loop;
	case CNTRL:
		qv_keyboard.cntrl ^= 0xffff;
		goto loop;
	case ALLUP:
		qv_keyboard.cntrl = qv_keyboard.shift = 0;
		goto loop;
	case REPEAT:
		c = qv_keyboard.last;
		break;
	default:
		/*
		 * Test for control characters. If set, see if the character
		 * is elligible to become a control character.
		 */
		if( qv_keyboard.cntrl ) {
			c = q_key[ j ];
			if( c >= ' ' && c <= '~' )
				c &= 0x1f;
		} else if( qv_keyboard.lock || qv_keyboard.shift )
			c = q_shift_key[ j ];
		else
			c = q_key[ j ];
		break;
	}

	qv_keyboard.last = c;

	/*
	 * Check for special function keys
	 */
	if( c & 0x80 ) 
		return 0;
	else
		return c;
}

/*
 * Output to the keyboard. This routine status polls the transmitter on the
 * keyboard to output a code. The timer is to avoid hanging on a bad device.
 */
qv_key_out( c )
char c;
{
	int timer = 30000;

	if( qv_scn.qvaddr ) {
		while( (qv_scn.qvaddr->qv_uartstatus & 0x4) == 0  && timer-- )
			;
		qv_scn.qvaddr->qv_uartdata = c;
	}
}

