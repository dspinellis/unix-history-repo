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
static char sccsid[] = "@(#)r.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include "ed.h"
#include <a.out.h>
#include <errno.h>

/*
 * This sets up things for the central input routine to place the
 * incoming text at the proper place in the buffer.
 */

void
r(inputt, errnum)

FILE *inputt;
int *errnum;

{
  FILE *fopen(), *l_fp;
  long l_num;
  char *l_filename_read, *l_temp;
  struct stat l_s_buf;
  int l_srv;
  struct exec l_magic;

  if (filename_flag == 1)
    {
      l_filename_read = filename_current;
      filename_flag = 0;
    }
  else
    {
      l_temp = filename(inputt, errnum);
      if (*errnum == 1)
        l_filename_read = l_temp;
      else if (*errnum == -2)
        {
          while (((ss = getc(inputt)) != '\n') || (ss == EOF))
               ;
          l_filename_read = filename_current;
        }
      else if (*errnum < 0)
        return;
      *errnum = 0;
    } /* end-else */

  if (filename_current == NULL)
    {
      if (l_filename_read == NULL)
        {
          strcpy(help_msg, "no filename given");
          *errnum = -1;
          if (ss)
            ungetc('\n', inputt);
          return;
        }
      else
        filename_current = l_filename_read;
    }

  if (sigint_flag)
    SIGINT_ACTION;

  /* determine if the file can be read. If not set the help message
   * to something descritive that the user should understand.
   */
  if (((l_srv = stat(l_filename_read, &l_s_buf)) == -1) || (l_s_buf.st_mode & S_IFDIR))
    {
      if (l_srv == -1)
        {
          switch (errno)
                {
                  case ENOTDIR: strcpy(help_msg, "directory in pathname does not exist");
                                break;
                  case ENOENT: strcpy(help_msg, "file does not exist");
                               break;
                  case EACCES: strcpy(help_msg, "permission lacking to read file");
                               break;
                  case ENAMETOOLONG: strcpy(help_msg, "pathname or component of pathname too long");
                                     break;
                  case EIO: strcpy(help_msg, "I/O error during read");
                            break;
                  case ELOOP: strcpy(help_msg, "too many symbolic links in pathname");
                              break;
                  default: strcpy(help_msg, "unable to read file stat");
                           break;
                }
        }
      else
        strcpy(help_msg, "filename is directory, not a text file");
      printf("?%s\n", l_filename_read);
      *errnum = 0;
      return;
    }

  if ((l_fp = fopen(l_filename_read, "r")) == 0)
    {
      strcpy(help_msg, "permission lacking to read file");
      printf("?%s\n", l_filename_read);
      *errnum = 0;
      return;
    }

  /* there is permission to read the file, but if it's an executable
   * file of the object code and linked type, we really don't want
   * to look at it (according ed spec's).
   */
  if (fread(&l_magic, sizeof(struct exec), 1, l_fp) != 0)
    {
      if (!(N_BADMAG(l_magic)))
        {
          strcpy(help_msg, "unable to read executable file");
          printf("?%s\n", l_filename_read);
          *errnum = 0;
          return;
        }
    }
  fseek(l_fp, 0L, 0);
  if (g_flag == 0)
    u_clr_stk();
  l_num = input_lines(l_fp, errnum);
  if (sigint_flag == 1)
    goto point;
  if (*errnum < 0)
    return;
  *errnum = 0;

  if (explain_flag) /* !=0 */
    printf("%ld\n", l_num);
  if (l_filename_read != filename_current)
    free(l_filename_read);
  point:
  fclose(l_fp);
  change_flag = 1;
  if (sigint_flag)
    SIGINT_ACTION;
  *errnum = 1;
} /* end-r */
