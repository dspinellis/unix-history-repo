/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)get_pattern.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * This is for getting RE and replacement patterns for any command
 * that uses RE's and replacements.
 */

char
*get_pattern(delim, inputt, errnum, flag)

int delim;
FILE *inputt;
int *errnum, flag;

{
  int l_cnt=1;
  static int l_max=510;
  char *l_pat, *l_pat_tmp;

  /* get a "reasonable amount of space for the RE */
  l_pat = (char *)calloc(l_max+2, sizeof(char));
  if (l_pat == NULL)
    {
      *errnum = -3;
      strcpy(help_msg, "out of memory error");
      return(NULL);
    }
  l_pat[0] = delim;

  if ((delim == ' ') || (delim == '\n'))
    {
      if (delim == '\n')
        ungetc(delim, inputt);
      strcpy(help_msg, "illegal delimiter");
      *errnum = -2;
      return(l_pat);
    }
  
  while (1)
       {
         ss = getc(inputt);
         if (ss == '\\')
           {
             ss = getc(inputt);
             if ((ss == delim) || ((flag == 1) && (ss == '\n')))
                 l_pat[l_cnt] = ss;
             else
               {
                 l_pat[l_cnt] = '\\';
                 /*ungetc(ss, inputt);*/
                 l_pat[++l_cnt] = ss;
               }
             goto leap;
           }
         else if ((ss == '\n') || (ss == EOF))
           {
             ungetc(ss, inputt);
             strcpy(help_msg, "no closing delimiter found");
             *errnum = -1;
             l_pat[l_cnt] = '\0'; /* this is done for 's's backward compat. */
             return(l_pat);
           }
         if (ss == delim)
           break;

         l_pat[l_cnt] = ss;

leap:
         if (l_cnt > l_max)
           {
             /* the RE is really long; get more space for it */
             l_max = l_max + 256;
             l_pat_tmp = l_pat;
             l_pat = (char *)calloc(l_max+2, sizeof(char));
             if (l_pat == NULL)
               {
                 *errnum = -3;
                 strcpy(help_msg, "out of memory error");
                 return(NULL);
               }
             bcopy(l_pat_tmp, l_pat, l_cnt);
             free(l_pat_tmp);
           }
         l_cnt++;
       }
  l_pat[l_cnt] = '\0';
  *errnum = 0;
  /* send back the pattern. l_pat[0] has the delimiter in it so the RE
   * really starts at l_pat[1]. It's done this way for the special forms
   * of 's' (substitute).
   */
  return(l_pat);
} /* end-get_pattern */
