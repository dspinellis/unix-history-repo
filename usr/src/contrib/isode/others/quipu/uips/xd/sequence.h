/* $Header: /f/osi/others/quipu/uips/xd/RCS/sequence.h,v 7.1 91/02/22 09:33:07 mrose Interim $ */
/*
 $Log:	sequence.h,v $
 * Revision 7.1  91/02/22  09:33:07  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/06/12  13:10:56  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:47  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:16  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:18  emsrssn
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
 * Revision 1.2  90/03/09  15:57:35  emsrssn
 * First public distribution
 * 
 * 
 * Revision 1.1  90/03/08  13:18:48  emsrssn
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

#ifndef SEQ
#define SEQ

#include <strings.h>

#ifdef QUIPU_MALLOC

#else
extern char * malloc ();
extern char * smalloc ();
#endif

typedef struct d_seq{
       char *dname;
       unsigned strlen;
       int seq_num;
       struct d_seq *next;
} d_seq, *D_seq;

#define NULLDS ((D_seq) 0)
char * get_from_seq ();
#endif
