/* encode.c - implement encoding routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/encode.c,v 7.4 91/02/22 09:29:14 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/encode.c,v 7.4 91/02/22 09:29:14 mrose Interim $
 *
 *
 * $Log:	encode.c,v $
 * Revision 7.4  91/02/22  09:29:14  mrose
 * Interim 6.8
 * 
 * Revision 1.6  91/01/08  21:56:34  kej
 * Correct bug in flush_output; it wasn't writing the last incomplete
 * byte to the output stream.
 * 
 * Revision 1.5  91/01/07  22:20:34  kej
 * Fully specify the BIT STRING which contains the G3NonBasicParams.
 * 
 * Revision 1.4  91/01/07  23:50:25  kej
 * Support fax images encoded as a SEQUENCE which contains a SET followed by
 * a SEQUENCE of BIT STRING.
 * 
 * Revision 1.3  91/01/05  23:31:07  kej
 * Implement support for specification of all G3-Fax nonbasic parameters.
 * 
 * Revision 1.2  91/01/05  00:31:34  kej
 * ISODE claimed to be creating fax images as ASN.1-encoded BIT STRING's.
 * However, the encoding was incorrect.  This revision corrects the
 * problem, implements 1-d and 2-d encoding of fax images, and it provides
 * a backward compatible mechanism for reading the old, broken images.
 * 
 * Revision 1.1  91/01/02  21:35:28  kej
 * Initial revision
 * 
 * Revision 7.1  90/07/09  14:40:25  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:01:39  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */



#include <stdio.h>
#include "quipu/photo.h"

extern int PIC_LINESIZE,STOP,NUMLINES;

int a0, a1, b1, b2;             /* markers */
int optlen;

char * malloc();

/*
 *   G3-Fax nonbasic parameters.
 */

int twoDimensional = 0;
int fineResolution = 1;
int unlimitedLength = 0;
int b4Length = 0;
int a3Width = 0;
int b4Width = 0;
int uncompressed = 0;
int standardwidth = 0;
int forcesize = 0;
/* encoding format options */

int nopreamble = 0;
int oldformat = 0;


/* ROUTINE:     encode_t4
/*
/* SYNOPSIS:    Implements CCITT recommendation T.4.
/*              This recomendation is concerned with compressing of bit maps.
/*
/* DESCRIPTION:
/*              This routine sets up the data buffers, then calls routines
/* to encode one line of the bit map. A line can be encoded  either one
/* dimensionally  or two dimensionally depending upon the 'k parameter'.
/*
/*    When a line is encoded two dimensionally, the line before is used as a
/* reference. For each line encoded, a record of where the run changes occur
/* are kept.  This is the used as the reference.
/*
*/


char * encode_t4 (k_param, inbuf, eolnskip)
int     k_param;
char *  inbuf;
int eolnskip;

{
    bit_string ref_line;            /* Reference line */
    bit_string t4_line;             /* Output encoded line */
    bit_string code_line;           /* Line we are codeing  */

    short   i,j;                      /* Loop variable */
    int	    run_buf [LINEBUF], run_buf2 [LINEBUF];

    if (a3Width)
	    forcesize = 2432;
    if (b4Width)
	    forcesize = 2048;
    if (standardwidth)
	    forcesize = 1728;

    ref_line.run_top = run_buf;
    code_line.run_top = run_buf2;
   
    code_line.dbuf_top = inbuf;
    t4_line.dbuf_top = malloc ((PIC_LINESIZE * NUMLINES) + 28);

    set_input  (&code_line);
    set_output (&t4_line);

    /* Repeat this loop once for every input line expected */

    for (i = 0; i < NUMLINES; i++) {

	if (code_line.run_top == run_buf) { /*swap buffers*/
       	    ref_line.run_top = run_buf;
	    code_line.run_top = run_buf2;
       	} else {
       	    ref_line.run_top = run_buf2;
       	    code_line.run_top = run_buf;
       	}
       	
	/* reset pointers */

	code_line.run_pos = code_line.run_top;
	ref_line.run_pos = ref_line.run_top;
       
	/* fill buffer for coding line */
       
	get_runs (&code_line);
	code_line.run_pos = code_line.run_top;

	put_eoln (&t4_line);

	if (k_param > 1) {
	    if (i % k_param == 0) {
	        set_bit (&t4_line);		/* tag bit, 1-d line follows */
		code_one (&code_line, &t4_line);
            }
	    else {
	        clr_bit (&t4_line);		/* tag bit, 2-d line follows */
		code_two (&ref_line, &code_line, &t4_line);
            }
        }
	else
	    code_one (&code_line, &t4_line);

	/* skip any extra eoln bit in orig data */

	for (j = 0; j < eolnskip; j++)
	    get_bit (&code_line);

    }

    /* now finish with 6 EOL's, as per T.4 */

    for (i = 0; i < 5; ++i) {
	put_eoln (&t4_line);
	if (k_param > 1) set_bit (&t4_line);
    }
   
    /* flush buffers, write preamble */

    flush_output (&t4_line);
    return (t4_line.dbuf_top);
}



/* ROUTINE:     code_one
/*
/* SYNOPSIS:    codes one line of a bit map into t4
/*
/* DESCRIPTION:
/*              To encode a line one dimensionally, bits are read in until
/* a change is noticed, when this happens, the run_length code for the number
/* of bits read in is found, and written to the output file.
/*
/* A run_length code may consist of two parts if the run is large, a make up
/* and a terminal code.
*/

code_one (lineptr,t4_lineptr)

bit_string * lineptr;           /* input line */
bit_string * t4_lineptr;        /* output line */

{
    char            colour = WHITE; /* the colour of the current bit */
    full_code       code;           /* the code for the characters run_length */
    int             old_pos = 1;    /* the number of bits of the same colur read in */
    int             len = 0;
    int             tlen;

    if (forcesize) {
	len = (forcesize - PIC_LINESIZE)/ 2;
	code = get_code ( len, WHITE);
	if (code.make.length != 0)
	    put_code (t4_lineptr,code.make);         /* the make code */
	put_code (t4_lineptr, code.term);            /* the terminal code */
	code = get_code (0,BLACK);
	put_code (t4_lineptr, code.term);
    }

    do {

	/* get code for next run = pos of current change - pos of last change */
	tlen = *++lineptr->run_pos - old_pos;
	len += tlen;
	code = get_code (tlen,colour);

	if (code.make.length != 0)
	    put_code (t4_lineptr,code.make);         /* the make code */
	put_code (t4_lineptr, code.term);            /* the terminal code */
	colour = 1 - colour;
	old_pos =  *lineptr->run_pos;

    } while (*lineptr->run_pos <= PIC_LINESIZE);

    if (forcesize) { 
	if (colour == BLACK) {
		code = get_code (0,colour);
		put_code (t4_lineptr, code.term);
	}
	colour = 1 - colour;

	code = get_code ( forcesize - len, colour);
	if (code.make.length != 0)
	    put_code (t4_lineptr,code.make);         /* the make code */
	put_code (t4_lineptr, code.term);            /* the terminal code */
    }
}






/* ROUTINE:     code_two
/*
/* SYNOPSIS:    Codes one line of a bit map two dimensionally as
/*              described by CCITT T.4.
/*
/* DESCRIPTION: Two lines are compared by looking at the list of run changes.
/* In order to do this, this list has to be created for the line we are about
/* to encode.  The encoding procedure then follows the flow chart in the CCITT
/* recommendation.  This is summarised as follows:
/*
/*   1. Find the positions a0, a1, b1, b2.
/*   2. Compare to see which mode is required.
/*
/* The positions of a1, b1, b2 are found from the run change list.  a0 is known
/* in advance.
*/

code_two (ref_lineptr,code_lineptr,t4_lineptr)

bit_string * ref_lineptr;       /* reference line */
bit_string * code_lineptr;      /* line to encode */
bit_string * t4_lineptr;        /* output line    */

{
    char    colour = WHITE;
    char    ref_colour = WHITE;

    a0 = 0;
    code_lineptr->run_pos = code_lineptr->run_top;

    do {

	/* find a1 */

	while (*code_lineptr->run_pos > a0)
	    --code_lineptr->run_pos;

	while (*code_lineptr->run_pos <= a0 && *code_lineptr->run_pos < STOP)
	    ++code_lineptr->run_pos;

	a1 = *code_lineptr->run_pos;

	/* find b1 and b2 */

	while (*ref_lineptr->run_pos > a0) {
	    ref_colour = 1 - ref_colour;
	    --ref_lineptr->run_pos;
        }

	while (*ref_lineptr->run_pos <= a0 && *ref_lineptr->run_pos < STOP ) {
	    ref_colour = 1 - ref_colour;
	    ++ref_lineptr->run_pos;
        }

	if (ref_colour == colour && *ref_lineptr->run_pos < STOP) {
	    ref_lineptr->run_pos++;
	    ref_colour = 1 - ref_colour;
        }

	b1 = *ref_lineptr->run_pos;
	if (b1 >= STOP)
	    b2 = STOP;
	else
	    b2 = *(ref_lineptr->run_pos + 1);

	/* select mode and code it */

	if (a1 > b2) {
	    pass_mode (t4_lineptr);
        }
	else if (abs (a1 - b1) <= 3) {
	    vertical_mode (t4_lineptr);
	    colour = 1 - colour;
        }
	else
	    horizontal_mode (code_lineptr,t4_lineptr,colour);

    } while (a0 < STOP);
}


/* ROUTINE:     Pass_mode
/*
/* SYNOPSIS:    Encodes pass_mode
/*
/* DESCRIPTION: When pass mode is detected, the pass mode code is written to
/* the output, and a0 is moved to underneath b2.
*/

pass_mode (t4_lineptr)
bit_string * t4_lineptr;

{
    static code_word code = {4,0x0200};

    put_code (t4_lineptr,code);
    a0 = b2;
}


/* ROUTINE:     Vertical_mode
/*
/* SYNOPSIS:    Encodes vertical mode.
/*
/* DESCRIPTION:  Vertical mode is encoded by writing a particualr code
/* depending on the offset between a1 and b1.
/* a0 is moved to a1
*/

vertical_mode (t4_lineptr)

bit_string * t4_lineptr;

{
    static code_word code [7] = {
	{7,0x080  },    /* -3 */
	{6,0x100  },    /* -2 */
	{3,0x800  },    /* -1 */
	{1,0x1000 },    /*  0 */
	{3,0xc00  },    /*  1 */
	{6,0x180  },    /*  2 */
	{7,0xc0   },    /*  3 */
    };

    put_code (t4_lineptr, code[a1 - b1 + 3]);
    a0 = a1;
}




/* ROUTINE:     Horizontal_mode
/*
/* SYNOPSIS:    Encodes horizontal mode
/*
/* DESCRIPTION: When horizontal mode is detected no further compaction can
/* can take place, so the next two run lengths are written to the output.
/* a0 is moved to after these runs.
*/

horizontal_mode (code_lineptr,t4_lineptr,colour)

bit_string * t4_lineptr;
bit_string * code_lineptr;
char    colour;

{
    int a2;
    static code_word h_code = {3,0x0400};
    full_code code;

    if (a0 == 0)    /* special case at start of line */
        a0 = 1;

    /* find a2 */

    a2 = *(++code_lineptr->run_pos);
    if (a2 >= STOP)
        code_lineptr->run_pos--;

    put_code (t4_lineptr, h_code);       /* code for horiz mode */

    /* get & put first run */

    code = get_code (a1 - a0, colour);
    if (code.make.length != 0)
        put_code (t4_lineptr, code.make);
    put_code (t4_lineptr, code.term);

    /* get & put second run */

    code = get_code (a2 - a1, 1 - colour);
    if (code.make.length != 0)
        put_code (t4_lineptr, code.make);
    put_code (t4_lineptr, code.term);

    a0 = a2;
}


/* ROUTINE:     Put_code ()                                             */
/*                                                                      */
/* SYNOPSIS:    appends the code word to the 'line'.                    */
/*                                                                      */

put_code (lineptr,code)

bit_string *    lineptr;
code_word       code;
{

    int		i;
    short	mask;

    mask = MSB_MASK;     /* set mask to first bit of pattern */

    for (i=0; i< code.length ; i++) {
	if ((code.pattern  & mask) == WHITE)
	    clr_bit (lineptr);
	else
	    set_bit (lineptr);

	mask >>=  1;
    }
}




/* ROUTINE:     put_eoln                                                */
/*                                                                      */
/* SYNOPSIS:    Puts an end of line marker at the end of a t4 line.     */
/*              An end of line (eoln) marker is 11 (or more) zero's     */
/*              followed by a 1.                                        */

put_eoln (lineptr)

bit_string    * lineptr;

{
    int i;

    for (i=0 ; i< 11; i++)
        clr_bit (lineptr);

    set_bit (lineptr);
}



/* ROUTINE:     get_runs
 *
 * SYNOPSIS:    set the runs change buffer fo the next input line
 *
 * DESCRIPTION: To optimise the input process, sequences of all 1's or 0's
 * - the most likely combinations are looked for as special cases, if not
 * found the runs are counted as bits.
 *
 */

get_runs (lineptr)
bit_string * lineptr;

{
    register i,j;
    char     colour = WHITE;

    *lineptr->run_pos++ = 0;

    for (i = 1; i <= PIC_LINESIZE; i++)
        if (get_bit (lineptr) != colour) {
    	    *(lineptr->run_pos++) = i;
	    colour = 1 - colour;
        }

    *lineptr->run_pos++ = STOP;
    *lineptr->run_pos = STOP;
}

/* ROUTINE:     set_output;
 *
 * SYNOPSIS:    Initialises the output buffers, writes the ENODE id, and
 * leaves room for the length (to be filled in later);
*/

set_output (lineptr)
bit_string * lineptr;
{
    lineptr->dbuf_top += 28; /* leave room for ASN.1 preamble */
    lineptr->dbuf = lineptr->dbuf_top;
    lineptr->mask = BIT_MASK;
}



/* ROUTINE:     flush_output;
 *
 * SYNOPSIS:    Flush the output buffer, and set the ASN.1 preamble if
 *              allowed.  The normal preamble consists of a SEQUENCE definition
 *              which wraps a SET and a SEQUENCE of BIT STRING.  The SET
 *              includes G3-Fax nonbasic parameter indications (such as
 *              twoDimensional, fineResolution, etc.).  Optionally, the
 *              old BIT STRING-ish preamble may be selected.
 */

flush_output (lineptr)
bit_string * lineptr;
{
    long length, len;
    int  count, i;

    if ( lineptr->mask != BIT_MASK )     /* writes last char if necessary */
        *lineptr->dbuf++ = lineptr->pos;

    if ( nopreamble ) {
	optlen = lineptr->dbuf - lineptr->dbuf_top;
	return;
    }

    /* set byte which indicates unused bits in last byte of image data */

    if ( !oldformat )
        *(--lineptr->dbuf_top) = 0x00;

    /* set image length */

    len = length = lineptr->dbuf - lineptr->dbuf_top;
   
    if (length <= 127)	{	/* short form length */
	*(--lineptr->dbuf_top) = length; 
    }
    else {

	/* see how many bytes needed for length */

	count = 0;

   	while (len != 0) 
   		{
   		len >>= 8; 
   		count++;
   		}
   	
   	/* go back and write this info */
    
   	for (i = 0; i < count; i++) 
   		*(--lineptr->dbuf_top) = (length >> (8 * i));
    		
   	*(--lineptr->dbuf_top) = 0x80 + count; 	/* length marker*/
    }

    /* set BIT STRING identifier */

    *(--lineptr->dbuf_top) = 0x03;

    if ( oldformat ) {
	optlen = lineptr->dbuf - lineptr->dbuf_top;
	return;
    }

    /* set length of BIT STRING sequence */

    len = length = lineptr->dbuf - lineptr->dbuf_top;

    if (length <= 127)	{	/* short form length */
	*(--lineptr->dbuf_top) = length; 
    }
    else {

	/* see how many bytes needed for length */

	count = 0;

   	while (len != 0) 
   		{
   		len >>= 8; 
   		count++;
   		}
   	
   	/* go back and write this info */
    
   	for (i = 0; i < count; i++) 
   		*(--lineptr->dbuf_top) = (length >> (8 * i));
    		
   	*(--lineptr->dbuf_top) = 0x80 + count; 	/* length marker*/
    }

    /* set SEQUENCE identifier */

    *(--lineptr->dbuf_top) = 0x30;

    /* set SET which includes g3NonBasicParams */

    *(--lineptr->dbuf_top) = uncompressed ? 0x02 : 0x00;

    *(--lineptr->dbuf_top) = 0;
    if (unlimitedLength) *lineptr->dbuf_top |= 0x08;
    if (b4Length)        *lineptr->dbuf_top |= 0x04;
    if (a3Width)         *lineptr->dbuf_top |= 0x02;
    if (b4Width)         *lineptr->dbuf_top |= 0x01;

    *(--lineptr->dbuf_top) = 0;
    if (twoDimensional) *lineptr->dbuf_top |= 0x80;
    if (fineResolution) *lineptr->dbuf_top |= 0x40;

    *(--lineptr->dbuf_top) = 0x00;	/* first byte of BIT STRING */
    *(--lineptr->dbuf_top) = 0x01;	/* count of unused bits */
    *(--lineptr->dbuf_top) = 5;		/* BIT STRING length */
    *(--lineptr->dbuf_top) = 0x81;	/* [1] IMPLICIT G3NonBasicParams */
    *(--lineptr->dbuf_top) = 7;		/* length of SET */
    *(--lineptr->dbuf_top) = 0x31;	/* SET */

    /* set length of entire sequence */

    len = length = lineptr->dbuf - lineptr->dbuf_top;

    if (length <= 127)	{	/* short form length */
	*(--lineptr->dbuf_top) = length; 
    }
    else {

	/* see how many bytes needed for length */

	count = 0;

   	while (len != 0) 
   		{
   		len >>= 8; 
   		count++;
   		}
   	
   	/* go back and write this info */
    
   	for (i = 0; i < count; i++) 
   		*(--lineptr->dbuf_top) = (length >> (8 * i));
    		
   	*(--lineptr->dbuf_top) = 0x80 + count; 	/* length marker*/
    }

    /* set [3] IMPLICIT G3Fax identifier */

    *(--lineptr->dbuf_top) = 0xa3;

    optlen = lineptr->dbuf - lineptr->dbuf_top;
}


/* ROUTINE:     set_input;
/*
/* SYNOPSIS:    Initialises the input buffers
*/

set_input (lineptr)
bit_string * lineptr;
{
    lineptr->mask = BIT_MASK;
    lineptr->dbuf = lineptr->dbuf_top;
    lineptr->pos = *lineptr->dbuf++;
}
