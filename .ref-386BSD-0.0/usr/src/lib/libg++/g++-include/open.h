
#ifndef _open_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _open_h 1

#include <File.h>
#include <sys/file.h>           // needed to determine values of O_RDONLY...

/*

 translation stuff for opening files. 

*/


enum sys_open_cmd_io_mode  // These should be correct for most systems
{                        
  sio_read      = O_RDONLY,
  sio_write     = O_WRONLY,
  sio_readwrite = O_RDWR,
  sio_append    = O_APPEND
};

enum sys_open_cmd_access_mode
{
  sa_create     = O_CREAT,
  sa_truncate   = O_TRUNC,
  sa_createonly = O_EXCL | O_CREAT
};

  
int open_cmd_arg(io_mode i, access_mode a); // decode modes
char* fopen_cmd_arg(io_mode i);
int open_cmd_arg(const char* m);

#endif
