/*
 * @(#)dir.c 1.4 87/11/06 Public Domain.
 *
 *  A public domain implementation of BSD directory routines for
 *  MS-DOS.  Written by Michael Rendell ({uunet,utai}michael@garfield),
 *  August 1897
 *  Ported to OS/2 by Kai Uwe Rommel
 *  December 1989, February 1990
 *  Change for HPFS support, October 1990
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <ctype.h>

#define INCL_NOPM
#include <os2.h>


#ifndef PERLGLOB
int attributes = A_DIR | A_HIDDEN;


static char *getdirent(char *);
static void free_dircontents(struct _dircontents *);

static HDIR hdir;
static USHORT count;
static FILEFINDBUF find;
static BOOL lower;


DIR *opendir(char *name)
{
  struct stat statb;
  DIR *dirp;
  char c;
  char *s;
  struct _dircontents *dp;
  char nbuf[MAXPATHLEN + 1];

  strcpy(nbuf, name);

  if ( ((c = nbuf[strlen(nbuf) - 1]) == '\\' || c == '/') &&
       (strlen(nbuf) > 1) )
  {
    nbuf[strlen(nbuf) - 1] = 0;

    if ( nbuf[strlen(nbuf) - 1] == ':' )
      strcat(nbuf, "\\.");
  }
  else
    if ( nbuf[strlen(nbuf) - 1] == ':' )
      strcat(nbuf, ".");

  if (stat(nbuf, &statb) < 0 || (statb.st_mode & S_IFMT) != S_IFDIR)
    return NULL;

  if ( (dirp = malloc(sizeof(DIR))) == NULL )
    return NULL;

  if ( nbuf[strlen(nbuf) - 1] == '.' )
    strcpy(nbuf + strlen(nbuf) - 1, "*.*");
  else
    if ( ((c = nbuf[strlen(nbuf) - 1]) == '\\' || c == '/') &&
         (strlen(nbuf) == 1) )
      strcat(nbuf, "*.*");
    else
      strcat(nbuf, "\\*.*");

  dirp -> dd_loc = 0;
  dirp -> dd_contents = dirp -> dd_cp = NULL;

  if ((s = getdirent(nbuf)) == NULL)
    return dirp;

  do
  {
    if (((dp = malloc(sizeof(struct _dircontents))) == NULL) ||
        ((dp -> _d_entry = malloc(strlen(s) + 1)) == NULL)      )
    {
      if (dp)
        free(dp);
      free_dircontents(dirp -> dd_contents);

      return NULL;
    }

    if (dirp -> dd_contents)
      dirp -> dd_cp = dirp -> dd_cp -> _d_next = dp;
    else
      dirp -> dd_contents = dirp -> dd_cp = dp;

    strcpy(dp -> _d_entry, s);
    dp -> _d_next = NULL;

    dp -> _d_size = find.cbFile;
    dp -> _d_mode = find.attrFile;
    dp -> _d_time = *(unsigned *) &(find.ftimeLastWrite);
    dp -> _d_date = *(unsigned *) &(find.fdateLastWrite);
  }
  while ((s = getdirent(NULL)) != NULL);

  dirp -> dd_cp = dirp -> dd_contents;

  return dirp;
}


void closedir(DIR * dirp)
{
  free_dircontents(dirp -> dd_contents);
  free(dirp);
}


struct direct *readdir(DIR * dirp)
{
  static struct direct dp;

  if (dirp -> dd_cp == NULL)
    return NULL;

  dp.d_namlen = dp.d_reclen =
    strlen(strcpy(dp.d_name, dirp -> dd_cp -> _d_entry));

  dp.d_ino = 0;

  dp.d_size = dirp -> dd_cp -> _d_size;
  dp.d_mode = dirp -> dd_cp -> _d_mode;
  dp.d_time = dirp -> dd_cp -> _d_time;
  dp.d_date = dirp -> dd_cp -> _d_date;

  dirp -> dd_cp = dirp -> dd_cp -> _d_next;
  dirp -> dd_loc++;

  return &dp;
}


void seekdir(DIR * dirp, long off)
{
  long i = off;
  struct _dircontents *dp;

  if (off >= 0)
  {
    for (dp = dirp -> dd_contents; --i >= 0 && dp; dp = dp -> _d_next);

    dirp -> dd_loc = off - (i + 1);
    dirp -> dd_cp = dp;
  }
}


long telldir(DIR * dirp)
{
  return dirp -> dd_loc;
}


static void free_dircontents(struct _dircontents * dp)
{
  struct _dircontents *odp;

  while (dp)
  {
    if (dp -> _d_entry)
      free(dp -> _d_entry);

    dp = (odp = dp) -> _d_next;
    free(odp);
  }
}


static
#endif
int IsFileSystemFAT(char *dir)
{
  USHORT nDrive;
  ULONG lMap;
  BYTE bData[64], bName[3];
  USHORT cbData;

  if ( _osmode == DOS_MODE )
    return TRUE;
  else
  {
    /* We separate FAT and HPFS file systems here.
     * Filenames read from a FAT system are converted to lower case
     * while the case of filenames read from a HPFS (and other future
     * file systems, like Unix-compatibles) is preserved.
     */

    if ( isalpha(dir[0]) && (dir[1] == ':') )
      nDrive = toupper(dir[0]) - '@';
    else
      DosQCurDisk(&nDrive, &lMap);

    bName[0] = (char) (nDrive + '@');
    bName[1] = ':';
    bName[2] = 0;

    cbData = sizeof(bData);

    if ( !DosQFSAttach(bName, 0U, 1U, bData, &cbData, 0L) )
      return !strcmp(bData + (*(USHORT *) (bData + 2) + 7), "FAT");
    else
      return FALSE;

    /* End of this ugly code */
  }
}

#ifndef PERLGLOB
static char *getdirent(char *dir)
{
  int done;

  if (dir != NULL)
  {				       /* get first entry */
    lower = IsFileSystemFAT(dir);

    hdir = HDIR_CREATE;
    count = 1;
    done = DosFindFirst(dir, &hdir, attributes,
			&find, sizeof(find), &count, 0L);
  }
  else				       /* get next entry */
    done = DosFindNext(hdir, &find, sizeof(find), &count);

  if ( lower )
    strlwr(find.achName);

  if (done == 0)
    return find.achName;
  else
  {
    DosFindClose(hdir);
    return NULL;
  }
}
#endif
