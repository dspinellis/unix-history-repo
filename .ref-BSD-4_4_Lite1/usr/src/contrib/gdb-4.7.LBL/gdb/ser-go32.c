/* Remote serial interface for GO32, for GDB, the GNU Debugger.
   Copyright 1992 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This file shows most of the obvious problems of code written for
   the IBM PC.  FIXME.    -- gnu@cygnus.com, Sep92 */

#include "defs.h"
#include "serial.h"

#include <sys/dos.h>

#define SIGNATURE 0x4154
#define VERSION 1
#define OFFSET 0x104

/*#define MONO 1*/

#define dprintf if(0)printf

#ifdef __GNUC__
#define far
#define peek(a,b) (*(unsigned short *)(0xe0000000 + (a)*16 + (b)))
#endif

typedef struct {
  short jmp_op;
  short signature;
  short version;
  short buffer_start;
  short buffer_end;
  short getp;
  short putp;
  short iov;
} ASYNC_STRUCT;

static ASYNC_STRUCT far *async;
static int iov;
#define com_rb	iov
#define com_tb	iov
#define com_ier	iov+1
#define com_ifr	iov+2
#define com_bfr	iov+3
#define com_mcr	iov+4
#define com_lsr	iov+5
#define com_msr	iov+6

static int fd;


#if MONO
#include <sys/pc.h>
static int mono_pos=0;
#define mono_rx 0x07
#define mono_tx 0x70

void
mono_put(char byte, char attr)
{
  ScreenSecondary[320+mono_pos+80] = 0x0720;
  ScreenSecondary[320+mono_pos] = (attr<<8) | (byte&0xff);
  mono_pos = (mono_pos+1) % 1200;
}

#endif

static char far *
aptr(short p)
{
#ifdef __GNUC__
  return (char *)((unsigned)async - OFFSET + p);
#else
  return (char far *)MK_FP(FP_SEG(async), p);
#endif
}

static ASYNC_STRUCT far *
getivec(int which)
{
  ASYNC_STRUCT far *a;

  if (peek(0, which*4) != OFFSET)
    return 0;
#ifdef __GNUC__
  a = (ASYNC_STRUCT *)(0xe0000000 + peek(0, which*4+2)*16 + peek(0, which*4));

#else
  a = (ASYNC_STRUCT far *)MK_FP(peek(0,which*4+2),peek(0,which*4));
#endif
  if (a->signature != SIGNATURE)
    return 0;
  if (a->version != VERSION)
    return 0;
  return a;
}

int
dos_async_init()
{
  int i;
  ASYNC_STRUCT far *a1;
  ASYNC_STRUCT far *a2;
  a1 = getivec(12);
  a2 = getivec(11);
  async = 0;
  if (a1)
    async = a1;
  if (a2)
    async = a2;
  if (a1 && a2)
  {
    if (a1 < a2)
      async = a1;
    else
      async = a2;
  }
  if (async == 0)
  {
    error("GDB can not connect to asynctsr program, check that it is installed\n\
and that serial I/O is not being redirected (perhaps by NFS)\n\n\
example configuration:\n\
C> mode com2:9600,n,8,1,p\n\
C> asynctsr 2\n\
C> gdb \n");

  }
  iov = async->iov;
  outportb(com_ier, 0x0f);
  outportb(com_bfr, 0x03);
  outportb(com_mcr, 0x0b);
  async->getp = async->putp = async->buffer_start;
  
#if MONO
  for (i=0; i<1200; i++)
    ScreenSecondary[320+i] = 0x0720;
#endif
  if (iov > 0x300)
    return 1;
  else
    return 2;
}

void
dos_async_tx(char c)
{
  dprintf("dos_async_tx: enter %x - with IOV %x", c, com_lsr);
  fflush(stdout);
  while (~inportb(com_lsr) & 0x20);
  outportb(com_tb, c);
#if MONO
  mono_put(c, mono_tx);
#endif
  dprintf("exit\n");
}

int
dos_async_ready()
{
  return (async->getp != async->putp);
}

int
dos_async_rx()
{
  char rv;
  dprintf("dos_async_rx: enter - ");
  fflush(stdout);
  while (!dos_async_ready())
   if (kbhit())
   {
     printf("abort!\n");
     return 0;
   }
  dprintf("async=%x getp=%x\n", async, async->getp);
  fflush(stdout);
  rv = *aptr(async->getp++);
#if MONO
  mono_put(rv, mono_rx);
#endif
  if (async->getp >= async->buffer_end)
   async->getp = async->buffer_start;
  dprintf("exit %x\n", rv);
  return rv;
}

int
dos_kb_ready()
{
  return (peek(0x40,0x1a) != peek(0x40,0x1c));
}

int
dos_kb_rx()
{
#ifdef __GNUC__
  return getkey();
#else
  return getch();
#endif
}

int
dosasync_read (int fd, char *buffer, int length, int timeout)
{
  long now, then;
  int l = length;
  time (&now);
  then = now+timeout;
  dprintf("dosasync_read: enter(%d,%d)\n", length, timeout);
  while (l--)
  {
    if (timeout)
    {
      while (!dos_async_ready())
      {
        time (&now);
        if (now == then)
        {
          dprintf("dosasync_read: timeout(%d)\n", length-l-1);
          return length-l-1;
        }
      }
    }
    *buffer++ = dos_async_rx();
  }
  dprintf("dosasync_read: exit %d\n", length);
  return length;
}

int
dosasync_write(int fd, const char *buffer, int length)
{
  int l = length;
  while (l--)
   dos_async_tx(*buffer++);
  return length;
}



char *
strlwr(char *s)
{
  char *p = s;
  while (*s)
  {
    if ((*s >= 'A') && (*s <= 'Z'))
      *s += 'a'-'A';
    s++;
  }
  return p;
}

sigsetmask()
{
}

const char *
serial_default_name ()
{
  return "com1";
}


void
serial_raw ()
{
  /* Always in raw mode */
}


int
serial_open (name)
      const char *name;
{
  fd = dos_async_init();
  if (fd) return 1;
  return 0;
}

int
serial_readchar (to)
      int to;
{
  char buf;
  if (dosasync_read(fd, &buf, 1, to))  
    return buf;
  else
    return -2; /* Timeout, I guess */
}

int
serial_setbaudrate (rate)
      int rate;
{
  return 0;
}

int
serial_nextbaudrate (rate)
      int rate;
{
  return 0;
}

int
serial_write (str, len)
      const char *str;
      int len;
{
  dosasync_write(fd, str, len);
}

int
serial_close ()
{
}
