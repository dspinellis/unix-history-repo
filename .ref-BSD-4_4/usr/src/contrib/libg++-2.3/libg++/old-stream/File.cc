/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifdef __GNUG__
#pragma implementation
#endif
#include <File.h>
#include <std.h>
#include <stdarg.h>
#include <values.h>
#include <sys/file.h>           // needed to determine values of O_RDONLY...


#ifdef VMS
#include <errno.h>	// needed to get the psect magic loaded
#define	FP	(*fp)
#else
#define	FP	fp
#endif

// error handlers

void verbose_File_error_handler(const char* msg)
{
  perror(msg);
  errno = 0;
}

void quiet_File_error_handler(const char*)
{
  errno = 0;
}

void fatal_File_error_handler(const char* msg)
{
  perror(msg);
  exit(1);
}

one_arg_error_handler_t File_error_handler = verbose_File_error_handler;


one_arg_error_handler_t set_File_error_handler(one_arg_error_handler_t f)
{
  one_arg_error_handler_t old = File_error_handler;
  File_error_handler = f;
  return old;
}


/*

 Opening files. 

 open(filename, io_mode, access_mode) is done via system open 
 command since fopen doesn't handle all of the cases possible 
 with sys open. After a successful open, fdopen is called to 
 attach an _iobuf to the file descriptor.

 All this requires a few decoding routines that can translate among our
 enumerated types, system flags, and fopen modes.

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

  
static int open_cmd_arg(io_mode i, access_mode a) // decode modes
{
  int arg;
  switch(i)
  {
  case io_readonly:   arg = sio_read;                   break;
  case io_writeonly:  arg = sio_write;                  break;
  case io_readwrite:  arg = sio_readwrite;              break;
  case io_appendonly: arg = sio_append | sio_write;     break;
  case io_append:     arg = sio_append | sio_readwrite; break;
  default:            return -1;
  };
  switch(a)
  {
  case a_createonly:  return arg | sa_createonly;
  case a_create:      return arg | sa_create | sa_truncate;
  case a_useonly:     return arg;
  case a_use:         return arg | sa_create;
  default:            return -1;
  }
}

static char* fopen_cmd_arg(io_mode i)
{
  switch(i)
  {
  case io_readonly:  return "r";
  case io_writeonly: return "w";
  case io_readwrite: return "r+";
  case io_appendonly:return "a";
  case io_append:    return "a+";
  default:           return 0;
  }
}


void File::initialize() 
{ 
  fp = 0; nm = 0; stat = 0; state = _bad; rw = 0;
}

// reset class vars after open
// fp->_flag inspection is isolated here

void File::reinitialize(const char* filename)
{
  if (filename != 0)     setname(filename);
  else if (fp == stdin)  setname("(stdin)");
  else if (fp == stdout) setname("(stdout)");
  else if (fp == stderr) setname("(stderr)");
  else if (rw & 4)       setname("(string)");
  else setname(0);

  if (fp != 0)
  {
    state = _good;
    if (FP->_flag & (_IOREAD|_IORW))
      rw |= 01;
    if (FP->_flag & (_IOWRT|_IORW))
      rw |= 02;
    check_state();
  }
  else
  {
    set(_fail); set(_bad);
    error();
  }
}


File& File::open(const char* filename, io_mode m, access_mode a)
{                                   
  close();
  int open_arg = open_cmd_arg(m, a);
  if (open_arg != -1)
  {
    int fd = ::open(filename, open_arg, 0666);
    if (fd >= 0)
      fp = fdopen(fd, fopen_cmd_arg(m));
  }
  reinitialize(filename);
  return *this;
}

File& File::open(const char* filename, const char* m)
{                                   
  close();
  fp = fopen(filename, m);
  reinitialize(filename);
  return *this;
}

File& File::open(FILE* fileptr)
{
  close();
  fp = fileptr;
  reinitialize(0);
  return *this;
}

File& File::open(int filedesc, io_mode m)
{
  close();
  fp = fdopen(filedesc, fopen_cmd_arg(m));
  reinitialize(0);
  return *this;
}

File& File::close()
{
  if (fp != 0)
  {
#ifdef VMS
    if (rw & 4)                 // we own the iobuf, kill it
      delete(*fp);	// kill the _iobuf
#endif
    if (rw & 4)                 // we own the iobuf, kill it
      delete fp;
    else if (fp == stdin || fp == stdout || fp == stderr)
      flush();
    else
      fclose(fp);
  }
  fp = 0;
  rw = 0;
  set(_bad);
  return *this;
}

File& File::remove()
{
  close();
  return failif (nm == 0 || unlink(nm) != 0);
}


File::File()
{ 
  initialize(); 
}

File::File(const char* filename, io_mode m, access_mode a)   
{ 
  initialize(); 
  open(filename, m, a); 
}

File::File(const char* filename, const char* m)   
{ 
  initialize(); 
  open(filename, m); 
}

File::File(int filedesc, io_mode m)
{ 
  initialize(); 
  open(filedesc, m); 
}

File::File(FILE* fileptr)
{ 
  initialize(); 
  open(fileptr); 
}

File::File(int sz, char* buf, io_mode m)
{
  if (m != io_readonly && m != io_writeonly)
    (*File_error_handler) ("invalid io_mode for string IO");
  initialize();
  rw = 4;
#ifdef VMS
  _iobuf	*iob;
  FILE		*f;

  iob = new _iobuf;
  f = new(FILE);
  *f = iob;
  fp = f;
#else
  fp = new _iobuf;
#endif
#ifndef _NFILE
  FP->_file = 255;          // any illegal value
#else
  FP->_file = _NFILE-1;       // The last filedescriptor...
#ifdef BUFEND_ENTRY_TYPE
  _bufendtab[FP->_file] = (BUFEND_ENTRY_TYPE)buf+sz-1;
#endif
#endif
  FP->_ptr = FP->_base = buf;
#ifdef HAVE_BUFSIZ
  FP->_bufsiz = sz;
#endif
  if (m == io_readonly)
  {
    int len = 0;
    while (len < sz && buf[len] != 0) ++len;
    if (len == sz)
      buf[sz - 1] = 0;            // force null-termination!
    FP->_cnt = len;
    FP->_flag = _IOREAD | _IOSTRG | _IOMYBUF;
  }
  else
  {
    bzero(buf, sz);             // so any result will be null-terminated
    FP->_cnt = sz - 1;          // leave at least one null at end
    FP->_flag = _IOWRT | _IOSTRG | _IOMYBUF;
  }
  reinitialize(0);
}

File::~File()
{
  delete(nm);
  close();
}

void File::setname(const char* newname)
{
  if (nm == newname) return;

  if (nm != 0)
    delete(nm);
  if (newname != 0)
  {
    nm = new char[strlen(newname) + 1];
    strcpy(nm, newname);
  }
  else
    nm = 0;
}


File& File::setbuf(int buffer_kind)
{                  
  if (!is_open())
  {
    set(_fail);
    return *this;
  }
  switch(buffer_kind)
  {
  case _IOFBF:       
#ifdef HAVE_SETVBUF
    setvbuf(fp, 0, _IOFBF, 0);
#endif
    break;           
  case _IONBF:       
    ::setbuf(fp, 0); 
    break;
  case _IOLBF:
#ifdef HAVE_SETLINEBUF
    setlinebuf(fp);
#else
#ifdef HAVE_SETVBUF
    setvbuf(fp, 0, _IOLBF, 0);
#endif
#endif    
    break;
  default:
    break;
  }
  return *this;
}

File& File::setbuf(int size, char* buf)
{
  if (!is_open())
  {
    set(_fail);
    return *this;
  }
#ifdef HAVE_SETVBUF
  setvbuf(fp, buf, _IOFBF, size);
#else
  setbuffer(fp, buf, size);
#endif
  return *this;
}

void File::error()
{
  check_state();
  set(_fail);
  if (errno != 0)
  {
    char error_string[400];
    strcpy(error_string, "\nerror in File ");
    if (nm != 0)
      strcat(error_string, nm);
    (*File_error_handler)(error_string);
  }
}


//------------------------------------------------------------------

void File::check_state() // ensure fp & state agree about eof
{
  if (fp != 0)
  {
    if (feof(fp))
      set(_eof);
    else
      unset(_eof);
    if (ferror(fp))
      set(_bad);
  }
}

File& File::put(const char* s)
{ 
  return failif(!writable() || fputs(s, fp) == EOF);
}

File& File::get(char* s, int n, char terminator)
{
  if (!readable())
  {
    set(_fail);
    return *this;
  }

  char ch;
  stat = --n;

  if (n > 0 && (get(ch)))
  {
    if (ch == terminator) {
      unget(ch);
      stat= 0;	// This is not an error condition !
    }
    else
    {
      *s++ = ch; --n;
      while (n > 0 && (get(ch)))
      {
        if (ch == terminator)
        {
          unget(ch);
          break;
        }
        else
        {
          *s++ = ch; --n;
        }
      }
    }
  }

  *s = 0;
  return failif((stat != 0) && ((stat -= n) == 0));
}

File& File::getline(char* s, int n, char terminator)
{
  if (!readable())
  {
    set(_fail);
    return *this;
  }

  char ch;
  stat = --n;

  while (n > 0 && (get(ch)))
  {
    --n;
    if ((*s++ = ch) == terminator)
      break;
  }

  *s = 0;
  return failif((stat != 0) && ((stat -= n) == 0));
}

// from Doug Schmidt

// This should probably be a page size....
#define CHUNK_SIZE 512

/* Reads an arbitrarily long input line terminated by a user-specified
   TERMINATOR.  Super-nifty trick using recursion avoids unnecessary calls
   to NEW! */

char *File::readline (int chunk_number, char terminator) 
{
  char buf[CHUNK_SIZE];
  register char *bufptr = buf;
  register char *ptr;
  char ch;
  int continu;

  while ((continu = !!get(ch)) && ch != terminator) /* fill the current buffer */
    {
      *bufptr++ = ch;
      if (bufptr - buf >= CHUNK_SIZE) /* prepend remainder to ptr buffer */
        {
          if (ptr = readline (chunk_number + 1, terminator))

            for (; bufptr != buf; *--ptr = *--bufptr);

          return ptr;
        }
    }
  if (!continu && bufptr == buf)
    return NULL;

  int size = (chunk_number * CHUNK_SIZE + bufptr - buf) + 1;

  if (ptr = new char[stat = size])
    {

      for (*(ptr += (size - 1)) = '\0'; bufptr != buf; *--ptr = *--bufptr)
        ;

      return ptr;
    } 
  else 
    return NULL;
}

/* Reads an arbitrarily long input line terminated by TERMINATOR.
   This routine allocates its own memory, so the user should
   only supply the address of a (char *). */

File& File::gets(char **s, char terminator)
{
  if (!readable())
  {
    set(_fail);
    return *this;
  }

  return failif(!(*s = readline (0, terminator)));
}
  
#ifndef VMS
File& File::scan(const char* fmt ...)
{
  if (readable())
  {
    va_list args;
    va_start(args, fmt);
#ifndef HAVE_VSCANF
    stat = _doscan(fp, fmt, args);
#else
    stat = vfscanf(fp, fmt, args);
#endif
    va_end(args);
    failif(stat <= 0);
  }
  return *this;
}
#endif

File& File::form(const char* fmt ...)
{
  va_list args;
  va_start(args, fmt);
#ifndef HAVE_VPRINTF
  stat = _doprnt(fmt, args, fp);
#ifdef HAVE_VOID_DOPRNT
  stat = ferror(fp) ? -1 : 0;
#endif
#else
  stat = vfprintf(fp, fmt, args);
#endif
  va_end(args);
  failif(stat < 0);
  return *this;
}

#ifdef VMS
extern "C" {
    unlink(const char *s)
    {
        int remove(const char *);

    	return remove(s);
    }
}
#endif

