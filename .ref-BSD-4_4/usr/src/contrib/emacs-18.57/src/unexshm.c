/* Attempted unexec for AIX.
   Copyright (c) 1990  Free Software Foundation, Inc.

  This file is not used because it occasionally fails to work.  This
  happens because the bss address when Emacs is run is not always the
  same.  If it happens to be different from what it was
  when Emacs was dumped, the dumped data won't work.
  No one has been able to prevent the address from varying.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This is based on a public domain program written by IBM.  */

/*************** SYSTEM DEFINES *********************************/

#include "config.h"
#include "paths.h"
#include <sys/types.h>
#include <sys/file.h>
#include <fcntl.h>
#include <sys/mode.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <malloc.h>
#include <stdio.h>				/* MWW */
#include "lisp.h"

/*************** LOCAL DEFINES **********************************/

struct data_header		/* saved data header		*/
{
  char *start;			/* dump _data addr		*/
  char *end;			/* dump _end addr		*/
  char *sbrk1;			/* dump original sbrk addr	*/
  char *sbrk2;			/* dump final sbrk addr	*/
  int puresize;			/* size of pure data dumped  */
};

#define EMACSSHMKEY "EMACSSHMKEY"
#define EMACS_DATA_FILE "EMACS-DATA"
#define NEW_SHMGET_FLAGS (IPC_CREAT | S_IWUSR | S_IRUSR \
      | S_IWGRP | S_IRGRP | S_IWOTH | S_IROTH)
#define OLD_SHMAT_FLAGS SHM_RDONLY
#define OLD_SHMGET_FLAGS (S_IRUSR | S_IRGRP | S_IROTH) 
#define OLD_OPEN_FLAGS O_RDONLY
#define NEW_OPEN_FLAGS (O_RDWR | O_CREAT | O_TRUNC)

/*************** EXTERNAL / GLOBAL DATA AREA ********************/

extern char _data;		/* start of data addr		*/
extern char _end;		/* end of all data + 1 addr	*/
static char *original_sbrk;	/* sbrk when dump first run	*/

void
map_in_data (use_dumped_data)
     int use_dumped_data;
{
  int bufsize;			/* malloc buffer size		*/
  struct data_header dh;	/* saved data header		*/
  int fd;			/* saved data file descriptor	*/
  char *finaladdr;		/* last addr in bucket		*/
  char *ipckey = getenv (EMACSSHMKEY); /* env ipc key string	*/
  int length;			/* dumped data lengths		*/
  char *newaddr;		/* new malloc buffer addr	*/
  int numblks;			/* number of remaining mallocs	*/
  int shmid;			/* shared memory id		*/
  key_t shmkey;			/* shared memory key		*/
  /* Note that using malloc here may not be safe.  */
  char name[sizeof (PATH_EXEC) + sizeof (EMACS_DATA_FILE) + 2];

  /* Consume remaining malloc space without increasing		*/
  /* the end of data space					*/
  original_sbrk = sbrk (0);
  for (bufsize = 16; bufsize < getpagesize (); bufsize *= 2)
    {
      while ((newaddr = (char *)malloc (bufsize - 8)) < original_sbrk)
	;
      for (numblks = (getpagesize () / bufsize) - 1; numblks > 0; numblks--)
	malloc (bufsize - 8);
      finaladdr = sbrk (0);
    }
  original_sbrk = sbrk (0);

  /* Determine ipc key from environment or default		*/
  if (ipckey && *ipckey)
    shmkey = atoi (ipckey);
  else
    shmkey = SHMKEY;

  /* If we don't want the dumped data, get an unshared segment.  */
  if (!use_dumped_data)
    {
      shmid = shmget (IPC_PRIVATE, PURESIZE, NEW_SHMGET_FLAGS);
      if (shmid == -1
	  || shmat (shmid, (char *)PURE_SEG_BITS, 0) == -1)
	{
	  fprintf (stderr, "emacs: failure obtaining new unshared memory segment.\n");
	  exit (1);
	}
      return;
    }

  /* Compute the file name with the dumped data.  */
  strcpy (name, PATH_EXEC);
  strcat (name, "/");
  strcat (name, EMACS_DATA_FILE);

  /* Open the file and make sure the addresses have not changed.  */
  fd = open (name, OLD_OPEN_FLAGS, 0);
  if (fd < 0)
    {
      fprintf (stderr, "emacs: failure opening `%s'\n", name);
      exit (1);
    }
  if (read (fd, (char *)&dh, sizeof (dh)) != sizeof (dh)
      || dh.start != &_data
      || dh.end != &_end
      || dh.sbrk1 != original_sbrk
      || dh.puresize != PURESIZE)
    {
      fprintf (stderr, "emacs: header mismatch in `%s'\n", name);
      exit (1);
    }

  /* Load in the unshared contents.  */
  if (!(length = dh.end - dh.start)
      || read (fd, (char *)&_data, length) != length
      || !(length = dh.sbrk2 - dh.sbrk1)
      || brk (dh.sbrk2) == -1
      || read (fd, dh.sbrk1, length) != length)
    {
      fprintf (stderr, "emacs: failure loading unshared data.\n");
      exit (1);
    }

  /* Attach to "pure data" shared memory segment		*/
  if ((shmid = shmget (shmkey, 0, 0)) == -1
      || (newaddr = shmat (shmid, (char *)PURE_SEG_BITS, OLD_SHMAT_FLAGS)) == -1)
    {
      /* We were unable to open an existing segment.  Make a new one.  */
      struct shmid_ds buf;

      /* First get rid of the one we tried to get.  */
      shmdt ((char *)PURE_SEG_BITS);
      shmctl (shmid, IPC_RMID, 0);

      /* If we could not write the data file,
	 don't make a shared segment that we could write.
	 Make an unshared segment instead.  */
      if (access (name, W_OK) != 0)
	{
	  shmid = shmget (IPC_PRIVATE, PURESIZE, NEW_SHMGET_FLAGS);
	  if (shmid == -1
	      || shmat (shmid, (char *)PURE_SEG_BITS, 0) == -1)
	    {
	      fprintf (stderr, "emacs: failure obtaining new unshared memory segment.\n");
	      exit (1);
	    }

	  /* Load the proper data into it.  */
	  if (read (fd, PURE_SEG_BITS, PURESIZE) != PURESIZE)
	    {
	      fprintf (stderr, "emacs: failure loading shared memory data.\n");
	      shmdt ((char *)PURE_SEG_BITS);
	      shmctl (shmid, IPC_RMID, 0);
	      exit (1);
	    }

	  close (fd);
	  return;
	}

      /* Allocate the new shared segment and arrange to write it.  */
      if ((shmid = shmget (shmkey, PURESIZE, NEW_SHMGET_FLAGS)) == -1
	  || shmat (shmid, (char *)PURE_SEG_BITS, 0) == -1)
	{
	  fprintf (stderr, "emacs: failure obtaining new shared memory segment.\n");
	  shmdt ((char *)PURE_SEG_BITS);
	  shmctl (shmid, IPC_RMID, 0);
	  exit (1);
	}

      /* Load the proper data into it.  */
      if (read (fd, PURE_SEG_BITS, PURESIZE) != PURESIZE)
	{
	  fprintf (stderr, "emacs: failure loading shared memory data.\n");
	  shmdt ((char *)PURE_SEG_BITS);
	  shmctl (shmid, IPC_RMID, 0);
	  exit (1);
	}

      /* Detach from the segment and bring it back readonly.  */
      shmdt ((char *)PURE_SEG_BITS);

      shmctl (shmid, IPC_STAT, &buf);
      buf.shm_perm.mode = OLD_SHMGET_FLAGS;
      shmctl (shmid, IPC_SET, &buf);

      newaddr = shmat (shmid, (char *)PURE_SEG_BITS, OLD_SHMAT_FLAGS);
      if (newaddr == -1)
	{
	  fprintf (stderr, "emacs: failure reattaching shared memory segment.\n");
	  shmctl (shmid, IPC_RMID, 0);
	  exit (1);
	}
    }

  close (fd);
}

/* Dump the appropriate parts of memory into a file named NEW
   from which the shared segment can be initialized.  */

void
map_out_data (new)
     char *new;
{
  struct data_header dh;	/* saved data header			*/
  int fd;			/* saved data file descriptor		*/
  int length;			/* dumped data length; */
  int shmid;
  key_t shmkey;			/* shared memory key		*/
  char *ipckey = getenv (EMACSSHMKEY); /* env ipc key string	*/

  /* Determine ipc key from environment or default		*/
  if (ipckey && *ipckey)
    shmkey = atoi (ipckey);
  else
    shmkey = SHMKEY;
				   
  /* Create "saved data" file header */
  dh.start = &_data;
  dh.end = &_end;
  dh.sbrk1 = original_sbrk;
  dh.sbrk2 = sbrk (0);
  dh.puresize = PURESIZE;

  /* Create new "saved data" dump file				*/
  unlink (new);
  fd = open (new, NEW_OPEN_FLAGS, 0666);
  if (fd < 0)
    report_file_error ("Opening dump file", Fcons (build_string (new), Qnil));

  /* Delete obsolete shared segment.  */
  shmid = shmget (shmkey, 0, 0);
  if (shmid != -1)
    {
      if (shmctl (shmid, IPC_RMID, 0) == 0)
	fprintf (stderr, "Destroying existing shared segment\n");
    }


  /* Write saved header and data				*/
  length = sizeof (dh);
  if (write (fd, (char *)&dh, length) != length)
    report_file_error ("Writing dump file header",
		       Fcons (build_string (new), Qnil));
  length = dh.end - dh.start;
  if (write (fd, dh.start, length) != length)
    report_file_error ("Writing low core in dump file",
		       Fcons (build_string (new), Qnil));
  length = dh.sbrk2 - dh.sbrk1;
  if (write (fd, dh.sbrk1, length) != length)
    report_file_error ("Writing heap in dump file",
		       Fcons (build_string (new), Qnil));
  length = PURESIZE;
  if (write (fd, PURE_SEG_BITS, length) != length)
    report_file_error ("Writing pure data in dump file",
		       Fcons (build_string (new), Qnil));
  close (fd);
}
