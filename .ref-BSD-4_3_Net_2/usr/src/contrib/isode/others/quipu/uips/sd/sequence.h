/*
 * $Header: /f/osi/others/quipu/uips/sd/RCS/sequence.h,v 7.2 91/02/22 09:32:25 mrose Interim $
 */

/* This file contains code to implement the list storage facilities
 * in the modified widget program (renamed SD 5/1/90).
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

#ifndef STRINGSEQ
#define STRINGSEQ

#include <string.h>

#ifdef QUIPU_MALLOC

#else
extern char * malloc ();
extern char * smalloc ();
#endif

typedef struct string_seq {
  char *dname;
  unsigned strlen;
  int seq_num;
  struct string_seq *next;
} string_seq, *str_seq;

#define NULLDS ((str_seq) 0)
char *get_from_seq ();
void free_seq(), add_seq();

#endif
