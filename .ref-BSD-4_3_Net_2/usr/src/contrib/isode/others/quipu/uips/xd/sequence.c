/* $Header: /f/osi/others/quipu/uips/xd/RCS/sequence.c,v 7.1 91/02/22 09:33:06 mrose Interim $ */
#ifndef lint
	static char *rcsid = "$Id: sequence.c,v 7.1 91/02/22 09:33:06 mrose Interim $";
#endif
/*
 $Log:	sequence.c,v $
 * Revision 7.1  91/02/22  09:33:06  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/06/12  13:10:52  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:46  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:15  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:17  emsrssn
 * keyboard accelerator now activates button highlight.
 * 
 * search types available is dependent on current position
 * to prevent unreasonable searches.
 * 
 * the help popup changes automatically depending on the 
 * position of the cursor
 * 
 * buttons remain a fixed size when the application is
 * resized
 * 
 * command line options are now handled properly
 * 
 * logging added
 * 
 * "reads" are now sorted to show mail address at top etc.
 * 
 * 
 * Revision 1.2  90/03/09  15:57:34  emsrssn
 * First public distribution
 * 
 * 
 * Revision 1.1  90/03/08  13:18:41  emsrssn
 * Initial revision
 * 
 * 
*/

/* This file contains code to implement the list storage facilities
 * in the modified widget program (renamed SD 5/1/90).
 */

/*    This file was written by Damanjit Mahl @ Brunel University
 *    on 31st October 1989 as part of the modifications made to
 *    the Quipu X.500 widget interface written by Paul Sharpe
 *    at GEC Research, Hirst Research Centre.
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include "sequence.h"

add_seq (seq, str)
D_seq *seq;
char *str;
{
D_seq curr;
      
      if (*seq) {
            for (curr = *seq; curr->next; curr = curr->next) {}
	    curr->next = (struct d_seq *) malloc (sizeof (struct d_seq));
	    curr = curr->next;
	    curr->strlen = strlen (str);
	    curr->dname = malloc ((curr->strlen)+5);
	    (void) strcpy(curr->dname, str);
	    curr->next = 0;
      } else {
	    curr = (struct d_seq *) malloc (sizeof (struct d_seq));
	    curr->strlen = strlen (str);
	    curr->dname = malloc ((curr->strlen)+5);
	    (void) strcpy(curr->dname, str);
	    curr->next = 0;
	    *seq = curr;
      }
}
	          
char *	    
get_from_seq (seq_num, seq_ptr)
int seq_num;
D_seq seq_ptr;
{
      for (; seq_num > 1 && seq_ptr; seq_ptr = seq_ptr->next, seq_num--) {}
      if (seq_ptr)
            return seq_ptr->dname;
      else
	    return 0;
}


free_seq (seq_ptr)
D_seq seq_ptr;
{
	D_seq next_seq;

	while (seq_ptr) {
	  	free(seq_ptr->dname);
		next_seq = seq_ptr->next;
		free((char *) seq_ptr);
		seq_ptr = next_seq;
	}
}
