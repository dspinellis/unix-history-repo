
#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/pod/RCS/sequence.c,v 7.1 91/02/22 09:31:53 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/pod/RCS/sequence.c,v 7.1 91/02/22 09:31:53 mrose Interim $
 */


#include <malloc.h>
#include <string.h>

#include "sequence.h"

void add_seq (seq, str)
     str_seq *seq;
     char *str;
{
  str_seq curr;

  if (*seq) {
    for (curr = *seq; curr->next; curr = curr->next) {}
    curr->next = (struct str_seq *) malloc (sizeof (struct str_seq));
    curr = curr->next;
    curr->strlen = strlen (str);
    curr->dname = malloc ((curr->strlen)+5);
    (void) strcpy(curr->dname, str);
    curr->next = 0;
  } else {
    curr = (struct str_seq *) malloc (sizeof (struct str_seq));
    curr->strlen = strlen (str);
    curr->dname = malloc ((curr->strlen)+5);
    (void) strcpy(curr->dname, str);
    curr->next = 0;
    *seq = curr;
  }
}
	          
char *get_from_seq (seq_num, seq_ptr)
     int seq_num;
     str_seq seq_ptr;
{
  for (; seq_num > 1 && seq_ptr; seq_ptr = seq_ptr->next, seq_num--) {}
  if (seq_ptr) return seq_ptr->dname;
  else return 0;
}

void free_seq (seq_ptr)
     str_seq seq_ptr;
{
  str_seq next_seq;
  
  while (seq_ptr) {
    free((char *) (seq_ptr->dname));
    next_seq = seq_ptr->next;
    free((char *) seq_ptr);
    seq_ptr = next_seq;
  }
}
