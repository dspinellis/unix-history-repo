#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/sd/RCS/sequence.c,v 7.2 91/02/22 09:32:24 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/sequence.c,v 7.2 91/02/22 09:32:24 mrose Interim $
 */

/*    This file was written by Damanjit Mahl @ Brunel University
 *    as part of the modifications made to
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

extern char * strdup ();

void add_seq (seq, str)
     str_seq *seq;
     char *str;
{
  str_seq curr;
  
  if (*seq) {
    for (curr = *seq; curr->next; curr = curr->next) {}
    curr->next = (struct string_seq *) malloc (sizeof (struct string_seq));
    curr = curr->next;
  } else {
    curr = (struct string_seq *) malloc (sizeof (struct string_seq));
    *seq = curr;
  }

  curr->strlen = strlen(str);
  curr->dname = strdup(str);
  curr->next = 0;
}
	          
char *get_from_seq (seq_num, seq_ptr)
     int seq_num;
     str_seq seq_ptr;
{
  for (; seq_num > 1 && seq_ptr; seq_ptr = seq_ptr->next, seq_num--) {}
  if (seq_ptr)
    return seq_ptr->dname;
  else
    return 0;
}


void free_seq (seq_ptr)
     str_seq seq_ptr;
{
  str_seq next_seq;
  
  while (seq_ptr) {
    free(seq_ptr->dname);
    next_seq = seq_ptr->next;
    free((char *) seq_ptr);
    seq_ptr = next_seq;
  }
}
