/*
** primos.c
**
** This file contains emulation routines for some common Unix functions
**
** Author: Peter Eriksson <pen@lysator.liu.se>
*/

#ifdef __50SERIES

#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>


uid_t  primos_uid = 42;
gid_t  primos_gid = 42;
mode_t primos_mode = 600;

/* Dummy do-nothing routine for chmod() */
int chmod(path, mode)
  char *path;
  int mode;
{
   return 0;
}

char *getenv(var)
  char *var;
{
  char buf[256];
  extern char *gvget();
  
  buf[0] = '.';
  strcpy(buf+1, var);

  return gvget(buf);
}


unlink(path)
  char *path;
{
  return delete(path);
}
 
int lstat(path, buf)
  char *path;
  struct stat *buf;
{
  return stat(path, buf);
}

int stat(path, buf)
  char *path;
  struct stat *buf;
{
  buf->st_dev     = 1;
  buf->st_ino     = 1;
  buf->st_nlink   = 1;
  buf->st_uid     = primos_uid;
  buf->st_gid     = primos_gid;
  buf->st_rdev    = 1;
  buf->st_blksize = 2048;

  buf->st_rwlock = frwlock(path);
  switch (buf->st_type = ftype(path))
  {
    case 0:
    case 1:
      /* Regular file (SAM or DAM) */
      buf->st_size   = fsize(path);
      buf->st_mtime  = fdtm(path);

      buf->st_mode = S_IFREG|primos_mode;
      break;

    case 4:
      buf->st_size = 0;
      buf->st_mtime = fdtm(path);

      buf->st_mode = S_IFDIR|primos_mode;
      break;

    case -1:
      return -1;
      
    default:
      buf->st_mode = primos_mode;
      buf->st_size = fsize(path);
      buf->st_mtime = fdtm(path);
  }

  buf->st_blocks = (buf->st_size-1) / buf->st_blksize + 1;

  /* Should be fixed to really fetch these values, but that
   * would require calling some PRIMOS subroutines and I don't have
   * a copy of the Primos Subroutine reference manuals here..
   */
  buf->st_atime = buf->st_mtime;
  buf->st_ctime = buf->st_mtime;
  
  return 0;
}

int fstat(fd, buf)
  int fd;
  struct stat *buf;
{
  char path[1025];

  return stat(getname(fd, path), buf);
}

int ascii2pascii(c)
  int c;
{
  return (c ? (c | 0x80) : '\0');
}


#endif /* __50SERIES */
