/* decode.c - the generic decoder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/decode.c,v 7.6 91/02/22 09:29:08 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/decode.c,v 7.6 91/02/22 09:29:08 mrose Interim $
 *
 *
 * $Log:	decode.c,v $
 * Revision 7.6  91/02/22  09:29:08  mrose
 * Interim 6.8
 * 
 * Revision 1.3  91/01/07  23:50:17  kej
 * Support fax images encoded as a SEQUENCE which contains a SET followed by
 * a SEQUENCE of BIT STRING.
 * 
 * Revision 1.2  91/01/05  00:31:29  kej
 * ISODE claimed to be creating fax images as ASN.1-encoded BIT STRING's.
 * However, the encoding was incorrect.  This revision corrects the
 * problem, implements 1-d and 2-d encoding of fax images, and it provides
 * a backward compatible mechanism for reading the old, broken images.
 * 
 * Revision 1.1  91/01/02  21:35:13  kej
 * Initial revision
 * 
 * Revision 7.1  90/07/09  14:40:20  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:01:37  mrose
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
#include <sgtty.h>
#include <signal.h>
#include "quipu/photo.h"
#include "psap.h"

#define UNCOMPRESSED_1D	0x0e
#define ERR_RUN		0x0f

/* this file contains the main routines for decoding X400 */

extern int PIC_LINESIZE, STOP, NUMLINES;

/* variables for top of the code word trees */

node * bl_tree_top;
node * wt_tree_top;
node * two_tree_top;

unsigned int position;

static char ref_colour;
static char colour;
char * malloc ();
char *bitmap;

static void  resync ();
static int seqerrs = 0;

int decode_t4 (data, name, size)
char *data;
char *name;
int   size;
{
    PE    member;
    PE    pe;
    PS    ps;
    PE    seq;
    PE    set;
    int   twoDimensional = 0;
    int   res;

    /*
     *  If the first byte does not indicate that the data is an ASN.1-encoded
     *  SET or BIT STRING, attempt to convert the data as a non-ASN.1 image.
     */

    if ((unsigned char)*data != 0xa3 && *data != 0x03) 
	return decode_t4_aux (data, name, (int)size, twoDimensional);

    /* attempt to decode the source */

    if ((ps = ps_alloc (str_open)) == NULLPS) {
	fprintf (stderr, "ps_alloc: unable to allocate presentation stream\n");
	return (-1);
    }
    if (str_setup (ps, data, (int)size, 0) == NOTOK) {
	ps_free (ps);
	return -1;
    }
    if ((pe = ps2pe (ps)) == NULLPE) { /* maybe it's a non-ASN.1 image */
	ps_free (ps);
	return decode_t4_aux (data, name, (int)size, 1);
    }

    if (pe->pe_class == PE_CLASS_UNIV && pe->pe_form == PE_FORM_PRIM &&
	pe->pe_id == PE_PRIM_BITS) { /* old  BIT STRING-like form */
	if (pe_pullup (pe) == NOTOK) {
	    pe_free (pe);
	    ps_free (ps);
	    return -1;
        }
	res = decode_t4_aux (pe -> pe_prim, name, ps_get_abs (pe), 1);
    }
    else if (pe->pe_class == PE_CLASS_CONT && pe->pe_form == PE_FORM_CONS &&
	     pe->pe_id == 3) {

	/* first member must be SET which describes G3-Fax options */

	set = first_member (pe);
	if (!set || set->pe_class != PE_CLASS_UNIV ||
	    set->pe_form != PE_FORM_CONS || set->pe_id != PE_CONS_SET) {
	    fprintf (stderr, "decode_fax: %s is not a fax image\n", name);
	    pe_free (pe);
	    ps_free (ps);
	    return -1;
	}

	for (member = first_member (set); member; member = next_member (set, member)) {
	    if (member->pe_class == PE_CLASS_CONT &&
		member->pe_form == PE_FORM_PRIM && member->pe_id == 1) {
	        twoDimensional = bit_test (prim2bit (member), 8);
		if (twoDimensional == -1) {
		    pe_free (pe);
		    ps_free (ps);
		    return -1;
	        }
	    }
	}

	/* SEQUENCE of BIT STRING should follow SET */

	seq = next_member (pe, set);
	if (!seq || seq->pe_class != PE_CLASS_UNIV ||
	    seq->pe_form != PE_FORM_CONS || seq->pe_id != PE_CONS_SEQ) {
	    fprintf (stderr, "%s: is not a fax image\n", name);
	    pe_free (pe);
	    ps_free (ps);
	    return -1;
	}

	for (member = first_member (seq); member; member = next_member (seq, member)) {
	    if (member->pe_class != PE_CLASS_UNIV ||
		member->pe_form != PE_FORM_PRIM ||
		member->pe_id != PE_PRIM_BITS) {
		fprintf (stderr, "%s: is not a fax image\n", name);
		pe_free (pe);
		ps_free (ps);
		return -1;
	    }
	    res = decode_t4_aux ((member->pe_prim) + 1, name,
			   ps_get_abs (member) - 1, twoDimensional);
	}
    }
    else  /* maybe its a non-ASN.1 image */
	res = decode_t4_aux (data, name, (int)size, 1);

    pe_free (pe);
    ps_free (ps);

    return res;


}

/*
 *  Uncompressed mode black and white pel count tables.  These tables
 *  are indexed by the count of leading zeros in the uncompressed mode
 *  code word.
 */

#define UC_EXIT 6

static int uc_white_pels [] = {
	/* image pattern     code word   */
    0,	/*     1             1           */
    1,	/*     01            01          */
    2,	/*     001           001         */
    3,	/*     0001          0001        */
    4,	/*     00001         00001       */
    5,	/*     00000         000001      */
    0,	/*                   0000001     */
    1,	/*     0             00000001    */
    2,	/*     00            000000001   */
    3,	/*     000           0000000001  */
    4,	/*     0000          00000000001 */
};

static int uc_black_pels [] = {
	/* image pattern     code word    */
    1,	/*     1              1           */
    1,	/*     01             01          */
    1,	/*     001            001         */
    1,	/*     0001           0001        */
    1,	/*     00001          00001       */
    0,	/*     00000          000001      */
    0,	/*                    0000001     */
    0,	/*     0              00000001    */
    0,	/*     00             000000001   */
    0,	/*     000            0000000001  */
    0,	/*     0000           00000000001 */
};

/* ROUTINE:     Decode_t4
/*
/* SYNOPSIS:    Decodes a bit map stored in format T4 as recommended
/*              by CCITT.
/*
/* DESCRIPTION: After setting up the buffers, a line at a time is dealt with.
/* Each line is recognised as being one or two dimensionally coded, depending
/* upon the tag bit.
/* The run change buffers for each line are kept incase the next line is two
/* dimensionally, when it will be used as a reference.
/*
*/


int decode_t4_aux (inbuf, winname, length, twoDimensional)
char *inbuf;
char *winname;
int   length;
int   twoDimensional;
{
   bit_string code_line,      /* output line */
              ref_line,       /* reference line */
              t4_line;        /* input line  */

   int       done;
   run_type  run;
   int	    *buffer1, *buffer2;
   int      *run_buf1, *run_buf2;
   char      oneDtag, tag;

   seqerrs = 0;

   if (photo_start (winname) == -1)
	return (-1);

   if (build_trees () == -1)
	return (-1);

   buffer1      = (int *) malloc (LINEBUF * sizeof(int));
   buffer2      = (int *) malloc (LINEBUF * sizeof(int));
   buffer1[0]   = 0;   /* to halt backtracting if needed at start of line */
   buffer2[0]   = 0;
   run_buf1     = buffer1;
   run_buf2     = buffer2;
   ref_line.run_top  = ++run_buf1;
   code_line.run_top = ++run_buf2;

   code_line.dbuf_top = malloc (BUFSIZ);
   t4_line.dbuf_top = inbuf;

   if (set_dinput (&t4_line, length) == -1)
	return (-1);
   set_doutput (&code_line);

   /* images must always start with a 1-d encoded line */

   position = 1;
   NUMLINES = 1;
   done = decode_one (&code_line, &t4_line);
   if (done < 0) return (-1);
   if ( !done ) { /* no leading EOL */
       flush_doutput (&code_line);
       set_doutput (&code_line);
       photo_line_end (&code_line);
   }
   else { /* leading EOL */
       NUMLINES = 0;
       done = 0;
   }

   if (twoDimensional)
       oneDtag = tag = get_bit(&t4_line);

   while ( done != 1 ) {
      ++NUMLINES;
      position = 1;

      if ( twoDimensional && tag != oneDtag ) {
	  if (code_line.run_top  == run_buf1) {
	      ref_line.run_top  = run_buf1;
	      code_line.run_top = run_buf2;
	  }
	  else {
	      ref_line.run_top  = run_buf2;
	      code_line.run_top = run_buf1;
	  }
	  done = decode_two (&ref_line, &code_line, &t4_line);
      }
      else {
	  done = decode_one (&code_line, &t4_line);
      }

      if (done == -1) return (-1);

      flush_doutput (&code_line);
      set_doutput (&code_line);
      photo_line_end (&code_line);

      if ( twoDimensional && done != 1 )
	  tag = get_bit (&t4_line);

      if ( done == -2 && twoDimensional ) {
	  while ( tag != oneDtag ) {
	      resync (&t4_line);
	      tag = get_bit (&t4_line);
	  }
      }

   }

   bitmap = code_line.dbuf_top;

   (void) free ((char *)buffer1);
   (void) free ((char *)buffer2);
	
   return (photo_end (winname));

}



/* ROUTINE:     find_node
 *
 * SYNOPSIS:    Reads a sequence of bits from a source line and traverses
 *              a code word tree to find a matching code word.
 */

static node *
find_node (lineptr, tree_top)
bit_string * lineptr;
node * tree_top;
{
   node * ptr;

   ptr = tree_top;

   do {

      if (get_bit (lineptr) == 0) {
	  ptr = ptr->zero;
      }
      else {
	  ptr = ptr->one;
      }

      if (ptr == NULL) {
	  (void) fprintf (stderr,
			  "decode_fax: WARNING - sequencing error in line %d\n",
			  NUMLINES);
	  return (NULL);
      }

   } while (ptr->n_type == INTERNAL);

   return (ptr);
}



/* ROUTINE:     next_run
 *
 * SYNOPSIS:    Reads the next run length from the input file.
 *
 * DESCRIPTION: As each bit is read, it is used to move down the decode tree,
 * when a node is found that contains a value, the value is returned.
 * The code is assumed to be one dimensional.
*/

run_type
next_run (lineptr,xcolour)
bit_string * lineptr;
char    xcolour;
{

   node *   ptr;
   run_type result;

   result.run_length = 0;

   if (xcolour == BLACK)
      ptr = find_node (lineptr, bl_tree_top);
   else
      ptr = find_node (lineptr, wt_tree_top);
      
   if (ptr == NULL) {
       result.r_type = ERR_RUN;
       return (result);
   }

   /* if the above value was a make up code, now read the terminal code */

   if (ptr->n_type == MAKE) {
        if (ptr->value == -1) {
	    result.r_type = UNCOMPRESSED_1D;
	    return (result);
	}
	result.run_length = ptr->value;
	if (xcolour == BLACK)
	    ptr = find_node (lineptr, bl_tree_top);
	else
	    ptr = find_node (lineptr, wt_tree_top);

	if (ptr == NULL) {
	    result.r_type = ERR_RUN;
	    return (result);
        }
    }

    result.run_length += ptr->value;
    result.r_type = ptr->n_type;
    return (result);
}



/* ROUTINE:     decode_one
/*
/* SYNOPSIS:    decodes one line of t4.
/*
/* DESCRIPTION: reads a run, then writes that many bits of the appropiate
/* colour to the output.
/*
/* RETURNS:     0 if successful
/*              1 if successful and end of page detected
/*             -1 if line could not be decoded
/*             -2 if error detected and resync succeeded
*/

decode_one (lineptr, t4_lineptr)
bit_string * lineptr;
bit_string * t4_lineptr;

{
   run_type run;
   char xcolour = WHITE;
   int  done;
   int  i;
   int  savelinesize;

   lineptr->run_pos = lineptr->run_top;

   savelinesize = PIC_LINESIZE;
   PIC_LINESIZE = 0;

   for (;;) {
       run = next_run (t4_lineptr, xcolour);
       if (run.r_type == ERR_RUN) {
	   if (++seqerrs < 10) {
	       resync (t4_lineptr);
	       return (-2);
           }
	   else {
	       fputs ("decode_fax: too many sequencing errors\n", stderr);
	       return (-1);
           }
       }
       else if (run.r_type == UNCOMPRESSED_1D) {
	   xcolour = undo_uncompressed_mode (lineptr, t4_lineptr, xcolour, 0);
	   if (xcolour == -1) return (-1);
       }
       else if (run.r_type == EOLN) {
	   break;
       }
       else {
	   PIC_LINESIZE += run.run_length;
	   put_run (lineptr, run.run_length, xcolour);
	   xcolour = 1 - xcolour;
       }
   }

   while (get_bit(t4_lineptr) != 01)
       ; /* skip fill characters */

   if (lineptr->run_pos == lineptr->run_top){
	done = 1;
	PIC_LINESIZE = savelinesize;
   }
   else
	done = 0;

   STOP = PIC_LINESIZE + 1;
   *lineptr->run_pos++ = STOP;
   *lineptr->run_pos   = STOP;

   return (done);
}



/* ROUTINE:     decode_two
/*
/* SYNOPSIS:    decodes a two dim line.
/*
/* DESCRIPTION: The binary codes read in are looked up in the decode tree,
/* and the appropiate routine called to decode that mode.
/*
/* RETURNS:     0 if successful
/*             -1 if too many sequencing errors
/*             -2 if resynch performed
*/

decode_two (ref_lineptr, code_lineptr, t4_lineptr)

bit_string * ref_lineptr;
bit_string * code_lineptr;
bit_string * t4_lineptr;

{
   int length;
   node * ptr;
   int status;

   ref_lineptr->run_pos = ref_lineptr->run_top;
   code_lineptr->run_pos = code_lineptr->run_top;

   colour = WHITE;
   ref_colour = BLACK;

   do {
       ptr = find_node (t4_lineptr, two_tree_top);
       if (ptr == NULL) {
	   if (++seqerrs < 10) {
	       resync (t4_lineptr);
	       return (-2);
           }
	   else {
	       fputs ("decode_fax: too many sequencing errors\n", stderr);
	       return (-1);
	   }
       }

       switch (ptr->value) {

       case -1:
	       colour = undo_uncompressed_mode (code_lineptr, t4_lineptr,
						colour, 1);
	       if (colour < 0) return (colour);
	       break;

       case P:
	       undo_pass_mode (ref_lineptr, code_lineptr);
	       break;

       case H:
	       status = undo_horiz_mode (t4_lineptr, code_lineptr);
	       if (status < 0) return (status);
	       break;

       case EOLN:
	       break;

       default:
	       undo_vert_mode (ref_lineptr, code_lineptr, ptr->value);
	       break;
       }
   } while (ptr->n_type != EOLN);

   /* fill to end of line with current colour */

   length = (int)(PIC_LINESIZE - position + 1);
   if (length != 0)
       put_run (code_lineptr, length, colour);

   while (get_bit (t4_lineptr) != 1)
	;               /* skip fill characters */

   *code_lineptr->run_pos++ = STOP;
   *code_lineptr->run_pos   = STOP;

   return (0);
}


/* ROUTINE:     undo_uncompressed_mode
/*
/* SYNOPSIS:    decodes a section recognised as uncompressed mode.
/*
/* DESCRIPTION: Process uncompressed code words until a terminating
/*              code word is found.
/*
/* RETURNS:     -1 if too many sequencing errors
/*              -2 if resynch performed
/*              else next colour
*/

undo_uncompressed_mode (lineptr, t4_lineptr, colour, twoD)
bit_string * lineptr;
bit_string * t4_lineptr;
int colour;
int twoD;

{
    int black_length;
    int next_colour;
    int white_length;
    int zeros;

    for (;;) {
	zeros = 0;
	while (get_bit(t4_lineptr) != 01) ++zeros;
	if (zeros > 10) {
	    fputs ("decode_fax: bad code word in uncompressed mode string\n",
		   stderr);
	    if (++seqerrs < 10) {
		resync (t4_lineptr);
		return (-2);
	    }
	    else {
		fputs ("decode_fax: too many sequencing errors\n", stderr);
		return (-1);
	    }
        }
	white_length = uc_white_pels[zeros];
	black_length = uc_black_pels[zeros];
	if (white_length > 0) {
	    if (colour == BLACK)
	        --lineptr->run_pos;
	    put_run (lineptr, white_length, WHITE);
	    colour = BLACK;
        }
	if (black_length > 0) {
	    if (colour == WHITE)
	        --lineptr->run_pos;
	    put_run (lineptr, black_length, BLACK);
	    colour = WHITE;
        }
	if (!twoD)
	    PIC_LINESIZE += white_length + black_length;

	if (zeros >= UC_EXIT) {
	    next_colour = (get_bit(t4_lineptr) == 01) ? BLACK : WHITE;
	    if (next_colour != colour)
	        --lineptr->run_pos;
	    return (next_colour);
	}
    }
}

/* ROUTINE:     undo_pass_mode
/*
/* SYNOPSIS:    decodes a section recognised as pass mode.
/*
/* DESCRIPTION: find b2, then write to output the same colour as before
/* up until position b2.
*/

undo_pass_mode (ref_lineptr, code_lineptr)
bit_string * ref_lineptr;
bit_string * code_lineptr;

{
   int length;

   goto_b1 (ref_lineptr);

   if (*ref_lineptr->run_pos < STOP) {
       ++ref_lineptr->run_pos;
       ref_colour = 1 - ref_colour;
    }

   length = (int)(*ref_lineptr->run_pos - position);
   put_run (code_lineptr, length, colour);
   --code_lineptr->run_pos;     /* don't count this as a change */
}




/* ROUTINE:     undo_horiz_mode
/*
/* SYNOPSIS:    decodes a section recognised as horizontal mode.
/*
/* DESCRIPTION: Read two run lengths for the input, and write the appropiate
/* number of 1's or 0's to the output.
/*
/* RETURNS:     0 if successful
/*             -1 if too many sequencing errors
/*             -2 if resynch performed
*/

undo_horiz_mode (t4_lineptr, code_lineptr)

bit_string * t4_lineptr;
bit_string * code_lineptr;

{
    run_type run;

    run = next_run (t4_lineptr, colour);
    if (run.r_type == ERR_RUN) {
	if (++seqerrs < 10) {
	    resync (t4_lineptr);
	    return (-2);
        }
	else {
	    fputs ("decode_fax: too many sequencing errors\n", stderr);
	    return (-1);
        }
    }

    put_run (code_lineptr, run.run_length, colour);

    run = next_run (t4_lineptr, 1 - colour);
    if (run.r_type == ERR_RUN) {
	if (++seqerrs < 10) {
	    resync (t4_lineptr);
	    return (-2);
        }
	else {
	    fputs ("decode_fax: too many sequencing errors\n", stderr);
	    return (-1);
        }
    }

    put_run (code_lineptr, run.run_length, 1 - colour);
    
    return (0);
}


/* ROUTINE:     undo_vert_mode
/*
/* SYNOPSIS:    decodes vertical mode
/*
/* DESCRIPTION: Find b1, the write 1's or 0's upto it allowing for the offset.
*/

undo_vert_mode (ref_lineptr, code_lineptr, offset)

bit_string * ref_lineptr;
bit_string * code_lineptr;
char         offset;

{
   int   length;

   goto_b1 (ref_lineptr);

   length = (*ref_lineptr->run_pos - position) + offset - FIXED_OFFSET;
   put_run (code_lineptr, length , colour);
   colour = 1 - colour;
}



/* ROUTINE:     goto_b1
 *
 * SYNOPSIS:    move the pointer in the reference line to b1
 *
 * DESCRIPTION: b1 is the first changing bit in the reference line
 * of a different colour to a0.  May need to move backwards or forwards
 *
 */

goto_b1 (lineptr)
bit_string * lineptr;

{
   while (*lineptr->run_pos > position) {
       --lineptr->run_pos;
       ref_colour = 1 - ref_colour;
   }

   while (*lineptr->run_pos < position) {
       ++lineptr->run_pos;
       ref_colour = 1 - ref_colour;
   }

   if (ref_colour == colour) {
	++lineptr->run_pos;
	ref_colour = 1 - ref_colour;
   }
   else if (*lineptr->run_pos < STOP)

	/* special case when b1 = a0, and the colours are different,
	   move b1 to the next change of same colour,
	   this must be allowed at the beginning of a line to get a run of
	   zero, as every line must start with a white element */

	if ((*lineptr->run_pos == position) && (*lineptr->run_pos > 1))
	    lineptr->run_pos += 2;
}



/* ROUTINE:     resync
/*
/* SYNOPSIS:    resynchronizes sequencing by locating the next EOL
/*
*/

static void
resync (lineptr)
bit_string * lineptr;

{
    int i;

    for (;;) {
	while (get_bit (lineptr) == 1)
	    ;
	i = 1;
	while (get_bit (lineptr) == 0)
	    ++i;
	if (i > 10) return;
    }
}



/* ROUTINE:     put_run                                                 */
/*                                                                      */
/* SYNOPSIS:    writes a run_length to the indicated bit_string.        */
/*                                                                      */

put_run (lineptr, length, xcolour)
bit_string * lineptr;
int  length;
char xcolour;
{
    register i;
    register l;

/*
    if ( length < 0 ) {
	(void) fprintf (stderr,
			"decode_fax: WARNING - negative run length (%d) detected in line %d\n",
			length, NUMLINES);
    }
*/

    if ( xcolour == WHITE)
	photo_white (length);
    else
	photo_black (length);

    /* now fill line buffer for purpose of decoding 2-d lines */

    if (length > 16) {
	l = length;
	if (lineptr->mask != BIT_MASK) {        /* fill current byte */
	   if (xcolour == WHITE)
	      do {
		 clr_bit (lineptr);
		 --l;
	      } while (lineptr->mask != BIT_MASK);

	   else
	      do {
		 set_bit (lineptr);
		 --l;
	      } while (lineptr->mask != BIT_MASK);
	  }

	/* write out the bytes */

	if (xcolour == WHITE)
	    for (i = 0; i < l / 8; i++)
		*lineptr->dbuf++ = 0;
	else
	    for (i = 0; i < l / 8; i++)
		*lineptr->dbuf++ = 0xff;


	/* put the last few bits into the next byte */

	if (xcolour == WHITE)
	     for (i = 0; i < l % 8; i++)
		clr_bit (lineptr);
	 else
	     for (i = 0; i < l % 8; i++)
		set_bit (lineptr);
    }

    /* length < 16 - can't optimise, so deal with bits */

    else {
	if (xcolour == WHITE) {
	    for (i = 0; i < length; i++) clr_bit (lineptr);
	}
	else {
	    for (i = 0; i < length; i++)
	       set_bit (lineptr);
	}
    }

    position += length;
    *lineptr->run_pos++ = position;
}




/* ROUTINE:     set_doutput;
/*
/* SYNOPSIS:    Initialises the output buffers
*/

set_doutput (lineptr)
bit_string * lineptr;
{
	lineptr->dbuf = lineptr->dbuf_top;
	lineptr->mask = BIT_MASK;
}




/* ROUTINE:     flush_doutput;
/*
/* SYNOPSIS:    flush the output buffer;
*/

flush_doutput (lineptr)
bit_string * lineptr;
{
int count = 0;

	while ( lineptr->mask != BIT_MASK ) {
		clr_bit (lineptr);
		count++;
	}
	photo_white (count);
}



/* ROUTINE:     set_dinput;
/*
/* SYNOPSIS:    Initialises the input buffers
*/

set_dinput (lineptr, length)
bit_string * lineptr;
int	length;
{
    unsigned char cbyte;
    int count;
    int i;

    lineptr->dbuf = lineptr->dbuf_top;

    if (length == 0) {
	if (*lineptr->dbuf++ != 0x03) {
	   (void) fputs ("decode_fax: input stream is not a BIT STRING\n",
			 stderr);
	   return (-1);
	}
	cbyte = *lineptr->dbuf++;
	if (cbyte & 0x80) { /* long form */
	    count = cbyte & 0x7f;
	    if (count > 4) {
		(void) fputs ("decode_fax: length error\n", stderr);	
		return (-1);
	    }
	    for (i = 0; i < count; ++i)
	        ++lineptr->dbuf;
	}
    }

   lineptr->pos = *lineptr->dbuf++;
   lineptr->mask = BIT_MASK;

   return (0);
}

