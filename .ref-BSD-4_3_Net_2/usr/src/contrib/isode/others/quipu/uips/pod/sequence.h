/*
 * $Header: /f/osi/others/quipu/uips/pod/RCS/sequence.h,v 7.1 91/02/22 09:31:54 mrose Interim $
 */

#ifndef SEQ
#define SEQ

#include <strings.h>
#include "defs.h"

#ifdef QUIPU_MALLOC

#else
extern char * malloc ();
extern char * smalloc ();
#endif

typedef struct str_seq {
  char *dname;
  unsigned strlen;
  struct str_seq *next;
} strSeq, *str_seq;

#define NULLDS ((str_seq) 0)
char *get_from_seq();
void add_seq();
void free_seq();
#endif

