/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

#ifdef __GNUG__
#pragma implementation
#endif
#include <File.h>
#include <std.h>
#include <stdarg.h>
#include <values.h>
#include <sys/file.h>           // needed to determine values of O_RDONLY...
#include <open.h>

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
#if 1  /* bsd 4.4 */
    if (FP->_flags & (__SRD|__SRW))
      rw |= 01;
    if (FP->_flags & (__SWR|__SRW|__SAPP))
      rw |= 02;
#else
    if (FP->_flag & (_IOREAD|_IORW))
      rw |= 01;
    if (FP->_flag & (_IOWRT|_IORW|_IOAPPEND))
      rw |= 02;
#endif
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

