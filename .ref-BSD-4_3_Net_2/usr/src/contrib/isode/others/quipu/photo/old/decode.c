/* decode.c - the generic decoder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/decode.c,v 7.3 90/09/25 18:44:48 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/decode.c,v 7.3 90/09/25 18:44:48 mrose Exp $
 *
 *
 * $Log:	decode.c,v $
 * Revision 7.3  90/09/25  18:44:48  mrose
 * ...
 * 
 * Revision 7.2  90/09/24  15:36:41  mrose
 * update
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

#define ERR_RUN 0x0f

#ifdef lint
#define FAXDIR "/tmp"
#endif

/* this file contains the main routines for decoding X400 */

extern int PIC_LINESIZE,STOP,NUMLINES;

/* variables for top of the code word trees */
node * bl_tree_top;
node * wt_tree_top;
node * two_tree_top;

unsigned int position;

static char ref_colour;
static char colour;
char * malloc ();
char *bitmap;

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

int decode_t4 (inbuf, winname, length)
char *inbuf, *winname;
int	length;
{
bit_string      code_line,      /* output line */
		ref_line,       /* reference line */
		t4_line;        /* input line  */

int     done = 0;
run_type run;

int        *buffer1, *buffer2;
int        *run_buf1, *run_buf2;
char 	   tag,nextbit;

   if (photo_start (winname) == -1)
	return (-1);

   if (build_trees (FAXDIR) == -1)
	return (-1);

   buffer1      = (int *) malloc (LINEBUF * sizeof(int));
   buffer2      = (int *) malloc (LINEBUF * sizeof(int));
   if (buffer1 == NULL || buffer2 == NULL) {
no_mem: ;
       (void) fprintf (stderr, "PHOTO: out of memory for %s", winname);
losing: ;
       if (buffer1)
	   free (buffer1);
       if (buffer2)
	   free (buffer2);
       return (-1);
   }
   buffer1[0]   = 0;   /* to halt backtracting if needed at start of line */
   buffer2[0]   = 0;
   run_buf1     = buffer1;
   run_buf2     = buffer2;
   ref_line.run_top  = ++run_buf1;
   code_line.run_top = ++run_buf2;

   if ((code_line.dbuf_top  = malloc (BUFSIZ)) == NULL)
       goto no_mem;
   t4_line.dbuf_top = inbuf;

   if (set_dinput  (&t4_line, length) == -1)
	goto losing;
   set_doutput (&code_line);

   /* t4 starts with an initial end of line */
   run = next_run(&t4_line,WHITE);
   if (run.r_type == ERR_RUN)
	goto losing;
   if (run.r_type != EOLN) {
	(void) fprintf (stderr,"PHOTO: Initial end of line missing for %s",
			winname);
	goto losing;
   }
   if ( get_bit(&t4_line) != 1){
	(void) fprintf (stderr,"PHOTO: Initial end of line (2) missing for %s",
			winname);
	goto losing;
   }

   NUMLINES = -1;

   tag = nextbit = get_bit(&t4_line);

   do {
      NUMLINES++;

      position = 1;
      if (nextbit != tag ) 
      {  if (code_line.run_top  == run_buf1)
	 {   ref_line.run_top  = run_buf1;
	     code_line.run_top = run_buf2;
	 }
	 else
	 {   ref_line.run_top  = run_buf2;
	     code_line.run_top = run_buf1;
	 }
	 done = decode_two (&ref_line,&code_line,&t4_line);
      }
      else
      {  code_line.run_pos = code_line.run_top;
	 done = decode_one (&code_line,&t4_line);
      }

      if (done == -1)
	goto losing;

        flush_doutput (&code_line);
	set_doutput (&code_line);
		  
	photo_line_end (&code_line);

   	if (!done)
		nextbit = get_bit(&t4_line);

   } while (! done);

   flush_doutput (&code_line); 
   bitmap = code_line.dbuf_top;

   (void) free ( (char *)buffer1);
   (void) free ( (char *)buffer2);
	
   return (photo_end (winname));

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
run_type result ;
   result.run_length = 0;

   if (xcolour == BLACK) {
      ptr = bl_tree_top;
   } else {
      ptr = wt_tree_top;
   }
      
   if (ptr == NULL) {
	 (void) fprintf (stderr,"PHOTO: tree error");
	 result.r_type = ERR_RUN;
	 return (result);
      }

   do {

      if (get_bit (lineptr) == 0)
	 ptr = ptr->zero;
      else
	 ptr = ptr->one;
	 
      if (ptr == NULL) {
	 /* it may be possible to recover from this in the future */
	 (void) fprintf (stderr,"PHOTO: Sequencing error (1)");
	 result.r_type = ERR_RUN;
	 return (result);
      }

   } while (ptr->n_type == INTERNAL);

   /* if the above value was a make up code, now read the terminal code */
   if (ptr->n_type == MAKE) {
	result.run_length = ptr->value;
	if (xcolour == BLACK) {
	   ptr = bl_tree_top;
	} else {
	   ptr = wt_tree_top;
	}
	   
	if (ptr == NULL) {
	    (void) fprintf (stderr,"PHOTO: tree error");
	    result.r_type = ERR_RUN;
	    return (result);
        }

	do {

	   if (get_bit (lineptr) == 0) {
		ptr = ptr->zero;
	   } else {
		ptr = ptr->one;
	   }

	   if (ptr == NULL) {
  	        /* it may be possible to recover from this ! */
		 (void) fprintf (stderr,"PHOTO: Sequencing error (2)");
		 result.r_type = ERR_RUN;
	 	 return (result);
      	   }
	} while (ptr->n_type == INTERNAL);

    }

    result.run_length += ptr->value;
    result.r_type = ptr->n_type;
    return (result);

}



/* ROUTINE:     decode_one
/*
/* SYNOPSIS:    decodes one line of t4.
/*
/* DESCRIPTION: reads a run, then writes that many bit of the appropiate
/* colour to the output.
*/

decode_one (lineptr, t4_lineptr)
bit_string * lineptr;
bit_string * t4_lineptr;

{
run_type run;
char    xcolour = WHITE;
int  done;
int savelinesize;

   savelinesize = PIC_LINESIZE;
   PIC_LINESIZE = 0;
   run = next_run (t4_lineptr,xcolour);
   if (run.r_type == ERR_RUN)
	return (-1);

   while (run.r_type != EOLN) {
      PIC_LINESIZE += run.run_length;
      put_run (lineptr,run.run_length,xcolour);
      xcolour = 1 - xcolour;
      run = next_run (t4_lineptr,xcolour);
      if (run.r_type == ERR_RUN)	
	return (-1);
   }
   while (get_bit(t4_lineptr) != 01)
	; /* skip fill characters */

   if (lineptr->run_pos == lineptr->run_top){
	done = 1;
	PIC_LINESIZE = savelinesize;
   } else
	done = 0;

   STOP = PIC_LINESIZE + 1;
   *(lineptr->run_pos++) = STOP;
   *(lineptr->run_pos)   = STOP;

   return (done);
}



/* ROUTINE:     decode_two
/*
/* SYNOPSIS:    decodes a two dim line.
/*
/* DESCRIPTION: The binary codes read in are looked up in the decode tree,
/* and the appropiate routine called to decode that mode.
*/

decode_two (ref_lineptr,code_lineptr,t4_lineptr)

bit_string * ref_lineptr;
bit_string * code_lineptr;
bit_string * t4_lineptr;

{
node * ptr;
int done;

   ref_lineptr->run_pos = ref_lineptr->run_top;
   code_lineptr->run_pos = code_lineptr->run_top;
   colour = WHITE;
   ref_colour = BLACK;
   do {
       ptr = two_tree_top;
       do {
	  if (ptr == NULL) {
	      (void) fprintf (stderr,"PHOTO: 2-d line failure");
	      return (-1);
	  }

	  if (get_bit (t4_lineptr) == 0)
	     ptr = ptr->zero;
	  else
	     ptr = ptr->one;
      } while (ptr->n_type == INTERNAL);

      switch (ptr->value) {

	 case P:  undo_pass_mode (ref_lineptr,code_lineptr);
		  break;

	 case H:  if (undo_horiz_mode (t4_lineptr,code_lineptr) == -1)
			return (-1);
		  break;

	 case EOLN: break;

	 default: undo_vert_mode (ref_lineptr,code_lineptr,ptr->value);

	 }

   } while (ptr->n_type != EOLN );

   /* fill to end of line with current colour */
   put_run (code_lineptr,(int)( PIC_LINESIZE - position + 1 ), colour);

   if (code_lineptr->run_pos == code_lineptr->run_top) /* no runs found */
	done = 1;
   else 
	done = 0;  

   while (get_bit (t4_lineptr) != 1)
	;               /* skip fill characters */

   *(code_lineptr->run_pos++) = STOP;
   *(code_lineptr->run_pos)   = STOP;

   return (done);
}


/* ROUTINE:     undo_pass_mode
/*
/* SYNOPSIS:    decodes a section recognised as pass mode.
/*
/* DESCRIPTION: find b2, then write to output the same colour as before
/* up until position b2.
*/

undo_pass_mode (ref_lineptr,code_lineptr)
bit_string * ref_lineptr;
bit_string * code_lineptr;

{
   goto_b1 (ref_lineptr);

   ref_lineptr->run_pos++;
   ref_colour = 1 - ref_colour;

   put_run (code_lineptr,(int) (*(ref_lineptr->run_pos) - position), colour);
   code_lineptr->run_pos--;     /* don't count this as a change */
}




/* ROUTINE:     undo_horiz_mode
/*
/* SYNOPSIS:    decodes a section recognised as horizontal mode.
/*
/* DESCRIPTION: Read two run lengths for the input, and write the appropiate
/* number of 1's or 0's to the output.
*/

undo_horiz_mode (t4_lineptr,code_lineptr)

bit_string * t4_lineptr;
bit_string * code_lineptr;

{
run_type run;

   run = next_run (t4_lineptr,colour);
   if (run.r_type == ERR_RUN)
	return (-1);
   put_run (code_lineptr,run.run_length,colour);

   run = next_run (t4_lineptr,1-colour);
   if (run.r_type == ERR_RUN)
	return (-1);
   put_run (code_lineptr,run.run_length,1-colour);

   return (0);
}


/* ROUTINE:     undo_vert_mode
/*
/* SYNOPSIS:    decodes vertical mode
/*
/* DESCRIPTION: Find b1, the write 1's or 0's upto it allowing for the offset.
*/

undo_vert_mode (ref_lineptr,code_lineptr,offset)

bit_string * ref_lineptr;
bit_string * code_lineptr;
char         offset;

{
int   length;

   /* find b1 */
   goto_b1 (ref_lineptr);

   length = (*ref_lineptr->run_pos - position) + offset - FIXED_OFFSET;
   put_run ( code_lineptr, length , colour);
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

   if ( *lineptr->run_pos > position )
      do
	ref_colour = 1 - ref_colour;
      while ( *--lineptr->run_pos > position ) ;

   if ( *lineptr->run_pos < position )
      do
	ref_colour = 1 - ref_colour;
      while ( *++lineptr->run_pos < position ) ;

   if (ref_colour == colour) {
	lineptr->run_pos++;
	ref_colour = 1 - ref_colour;

   } else
	/* special case when b1 = a0, and the colours are different,
	   move b1 to the next change of same colour,
	   this must be allowed at the beginning of a line to get a run of
	   zero, as every line must start with a white element */
	if ((*lineptr->run_pos == position) && (*lineptr->run_pos >1 )) {
	    lineptr->run_pos++;
	    lineptr->run_pos++;
   }

}


/* ROUTINE:     put_run                                                 */
/*                                                                      */
/* SYNOPSIS:    writes a run_length to the indiacated bit_string,       */
/*                                                                      */

put_run (lineptr, length, xcolour)
bit_string * lineptr;
int  length;
char xcolour;
{
    register i;

    if ( xcolour == WHITE)
	photo_white (length);
    else
	photo_black (length);

    /* now fill line buffer for purpose of decoding 2-d lines */

    if (length > 16)
    {	position += length;
	*lineptr->run_pos++ = position;

	if (lineptr->mask != BIT_MASK) {        /* fill current byte */
	   if (xcolour == WHITE)
	      do {
		 clr_bit (lineptr);
		 length--;
	      } while (lineptr->mask != BIT_MASK);

	   else
	      do {
		 set_bit (lineptr);
		 length--;
	     } while (lineptr->mask != BIT_MASK);
	  }

	/* write out the bytes */
	if (xcolour == WHITE)
	    for (i=0; i<length/8; i++)
		*lineptr->dbuf++ = 0;
	else
	    for (i=0; i<length/8; i++)
		*lineptr->dbuf++ = 0xff;


	/* put the last few bits into the next byte */
	if (xcolour == WHITE)
	     for (i=0; i<length%8; i++)
		clr_bit (lineptr);
	 else
	     for (i=0; i<length%8; i++)
		set_bit (lineptr);
    }
    else
    {    /* length < 16 - can't optimise, so deal with bits */

	if (xcolour == WHITE) {
	    for (i=0; i<length; i++) clr_bit (lineptr);
	    position += length;
	    *lineptr->run_pos++ = position;
	} else {
	    for (i=0; i<length; i++)
	       set_bit (lineptr);
	   position += length;
	   *lineptr->run_pos++ = position;
	}
     
    }

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
    bit_string  temp;
    int i;

   lineptr->dbuf = lineptr->dbuf_top;

    if (length == 0) {
      /* check id + skip length */
      if ( *lineptr->dbuf++ != 0x03 ) {
	 (void) fprintf (stderr,"PHOTO: Not a g3fax bit map");
	 return (-1);
      }

      if ((length = piclen (lineptr->dbuf_top)) == -1)
	 return (-1);
   }

   while ((lineptr->pos = *lineptr->dbuf++) != 0x00)
	 ; /* no op */

   lineptr->mask = BIT_MASK;

   return (0);
}

piclen (s1)
char * s1;
{
int length=0,cnt,i;
char * temp;

       if (*s1 == 0x03) {
       /* we have a coded picture */

	    temp = s1;
	    temp++;
	    cnt = *temp++ & 0x7f;  /*assume len > 127 for now */
	    for (i=0; i<cnt; i++) 
		length = (length << 8) | (*temp++ & 0xff) ;

	    length += 2 + cnt;
	    return (length);

       } else {
	    (void) fprintf (stderr,"PHOTO: length error");	
	    return (-1);
       }

}

