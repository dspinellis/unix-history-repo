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
static char sccsid[] = "@(#)w.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"

/*
 * Write the contents of the buffer out to the real file (spec'd or
 * remembered). If 'w' then overwrite, if 'W' append to the file. 'W'
 * is probably _the_ command that most editors don't have, and it's
 * so(!) useful. The 'wq' works as 'w' but 'q' immediately follows.
 * Shame on POSIX for not including 'W' and 'wq', they're not that
 * hard to implement; yaaa! BSD for keeping it! :-)
 */

void
w(inputt, errnum)

FILE *inputt;
int *errnum;

{
  int l_ttl=0, l_q_flag=0, l_sl;
  FILE *fopen(), *fp;
  char *filename_read, *temp;

  if (start_default && End_default)
    {
      start = top;
      End = bottom;
    }
  else if (start_default)
    start = End;
  if (start == NULL)
    {
      strcpy(help_msg, "bad address");
      *errnum = -1;
      return;
    }
  start_default = End_default = 0;

  l_sl = ss;
  ss = getc(inputt);

  if (ss == 'q')  /* "wq" and "Wq" command */
    l_q_flag = 1;
  else
    ungetc(ss, inputt);

  temp = filename(inputt, errnum);
  if (sigint_flag)
    SIGINT_ACTION;
  if (*errnum == 1)
    filename_read = temp;
  else if (*errnum == -2)
    {
      while (((ss = getc(inputt)) != '\n') || (ss == EOF))
           ;
      filename_read = filename_current;
    }
  else if (*errnum < 0)
    return;
  *errnum = 0;

  if (filename_current == NULL)
    {
      if (filename_read == NULL)
        {
          strcpy(help_msg, "no filename given");
          *errnum = -1;
          ungetc('\n', inputt);
          return;
        }
      else
        filename_current = filename_read;
    }

  if (l_sl == 'W')
    fp = fopen(filename_read, "a");
  else
    fp = fopen(filename_read, "w");

  if (fp == NULL) 
    {
      strcpy(help_msg, "cannot write to file");
      *errnum = -1;
      ungetc('\n', inputt);
      return;
    }

  if (sigint_flag)
    goto point;

  /* write it out and get a report on the number of bytes written */
  l_ttl = edwrite(fp, start, End);
  if (explain_flag != 0)  /* for -s option */
    printf("%d\n", l_ttl);
  point:
  fclose(fp);
  if (sigint_flag)
    SIGINT_ACTION;
  if (filename_read != filename_current)
    free(filename_read);
  change_flag = 0L;
  *errnum = 1;
  if (l_q_flag)  /* for "wq" */
    {
      ungetc('\n', inputt);
      ss = (int)'q';
      q(inputt, errnum);
    }
  return;
}

/*
 * Actually writes out the contents of the buffer to the specified
 * STDIO file pointer for the range of lines specified.
 */

int
edwrite(fp, begi, fini)

FILE *fp;
LINE *begi, *fini;

{
  register int l_ttl=0;

  while (1)
       {
         get_line(begi->handle, begi->len);
         /* fwrite is about 20+% faster than fprintf -- no surprise */
         fwrite(text, sizeof(char), begi->len, fp);
         fputc('\n', fp);
         l_ttl = l_ttl + (begi->len) + 1;
         if (begi == fini)
           break;
         else
           begi = begi->below;
         if (sigint_flag)
           return(l_ttl);
       }
  return(l_ttl);
} /* end-write */
