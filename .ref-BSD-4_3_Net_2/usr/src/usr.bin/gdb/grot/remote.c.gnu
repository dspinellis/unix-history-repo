/* Memory-access and commands for inferior process, for GDB.
   Copyright (C)  1988, 1989 Free Software Foundation, Inc.

This file is part of GDB.

GDB is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GDB; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Remote communication protocol.
   All values are encoded in ascii hex digits.

	Request		Packet

	read registers  g
	reply		XX....X		Each byte of register data
					is described by two hex digits.
					Registers are in the internal order
					for GDB, and the bytes in a register
					are in the same order the machine uses.
			or ENN		for an error.

	write regs	GXX..XX		Each byte of register data
					is described by two hex digits.
	reply		OK		for success
			ENN		for an error

	read mem	mAA..AA,LLLL	AA..AA is address, LLLL is length.
	reply		XX..XX		XX..XX is mem contents
			or ENN		NN is errno

	write mem	MAA..AA,LLLL:XX..XX
					AA..AA is address,
					LLLL is number of bytes,
					XX..XX is data
	reply		OK		for success
			ENN		for an error

	cont		cAA..AA		AA..AA is address to resume
					If AA..AA is omitted,
					resume at same address.

	step		sAA..AA		AA..AA is address to resume
					If AA..AA is omitted,
					resume at same address.

	last signal     ?               Reply the current reason for stopping.
                                        This is the same reply as is generated
					for step or cont : SAA where AA is the
					signal number.

	There is no immediate reply to step or cont.
	The reply comes when the machine stops.
	It is		SAA		AA is the "signal number"

	kill req	k
*/

#include <stdio.h>
#include "defs.h"
#include "param.h"
#include "frame.h"
#include "inferior.h"

#include "wait.h"

#ifdef USG
#include <sys/types.h>
#include <fcntl.h>
#endif

#include <signal.h>
#include <sys/ioctl.h>
#include <sys/file.h>

#ifdef HAVE_TERMIO
#include <termio.h>
#undef TIOCGETP
#define TIOCGETP TCGETA
#undef TIOCSETN
#define TIOCSETN TCSETA
#undef TIOCSETP
#define TIOCSETP TCSETAF
#define TERMINAL struct termio
#else
#include <sgtty.h>
#define TERMINAL struct sgttyb
#endif

static int kiodebug;
static int timeout = 5;

#if 0
int icache;
#endif

/* Descriptor for I/O to remote machine.  Initialize it to -1 so that
   remote_open knows that we don't have a file open when the program
   starts.  */
int remote_desc = -1;

#define	PBUFSIZ	400

/* Maximum number of bytes to read/write at once.  The value here
   is chosen to fill up a packet (the headers account for the 32).  */
#define MAXBUFBYTES ((PBUFSIZ-32)/2)

static void remote_send ();
static void putpkt ();
static void getpkt ();
#if 0
static void dcache_flush ();
#endif


/* Called when SIGALRM signal sent due to alarm() timeout.  */
#ifndef HAVE_TERMIO
void
remote_timer ()
{
  if (kiodebug)
    printf ("remote_timer called\n");

  alarm (timeout);
}
#endif

/* Open a connection to a remote debugger.
   NAME is the filename used for communication.  */

void
remote_open (name, from_tty)
     char *name;
     int from_tty;
{
  TERMINAL sg;

  if (remote_desc >= 0)
    close (remote_desc);

  remote_debugging = 0;
#if 0
  dcache_init ();
#endif

  remote_desc = open (name, O_RDWR);
  if (remote_desc < 0)
    perror_with_name (name);

  ioctl (remote_desc, TIOCGETP, &sg);
#ifdef HAVE_TERMIO
  sg.c_cc[VMIN] = 0;		/* read with timeout.  */
  sg.c_cc[VTIME] = timeout * 10;
  sg.c_lflag &= ~(ICANON | ECHO);
#else
  sg.sg_flags = RAW;
#endif
  ioctl (remote_desc, TIOCSETP, &sg);

  if (from_tty)
    printf ("Remote debugging using %s\n", name);
  remote_debugging = 1;

#ifndef HAVE_TERMIO
#ifndef NO_SIGINTERRUPT
  /* Cause SIGALRM's to make reads fail.  */
  if (siginterrupt (SIGALRM, 1) != 0)
    perror ("remote_open: error in siginterrupt");
#endif

  /* Set up read timeout timer.  */
  if ((void (*)) signal (SIGALRM, remote_timer) == (void (*)) -1)
    perror ("remote_open: error in signal");
#endif

  putpkt ("?");			/* initiate a query from remote machine */
}

/* Close the open connection to the remote debugger.
   Use this when you want to detach and do something else
   with your gdb.  */
void
remote_close (from_tty)
     int from_tty;
{
  if (!remote_debugging)
    error ("Can't close remote connection: not debugging remotely.");
  
  close (remote_desc);		/* This should never be called if
				   there isn't something valid in
				   remote_desc.  */

  /* Do not try to close remote_desc again, later in the program.  */
  remote_desc = -1;

  if (from_tty)
    printf ("Ending remote debugging\n");

  remote_debugging = 0;
}
 
/* Convert hex digit A to a number.  */

static int
fromhex (a)
     int a;
{
  if (a >= '0' && a <= '9')
    return a - '0';
  else if (a >= 'a' && a <= 'f')
    return a - 'a' + 10;
  else
    error ("Reply contains invalid hex digit");
}

/* Convert number NIB to a hex digit.  */

static int
tohex (nib)
     int nib;
{
  if (nib < 10)
    return '0'+nib;
  else
    return 'a'+nib-10;
}

/* Tell the remote machine to resume.  */

int
remote_resume (step, signal)
     int step, signal;
{
  char buf[PBUFSIZ];

#if 0
  dcache_flush ();
#endif

  strcpy (buf, step ? "s": "c");

  putpkt (buf);
}

/* Wait until the remote machine stops, then return,
   storing status in STATUS just as `wait' would.  */

int
remote_wait (status)
     WAITTYPE *status;
{
  unsigned char buf[PBUFSIZ];

  WSETEXIT ((*status), 0);
  getpkt (buf);
  if (buf[0] == 'E')
    error ("Remote failure reply: %s", buf);
  if (buf[0] != 'S')
    error ("Invalid remote reply: %s", buf);
  WSETSTOP ((*status), (((fromhex (buf[1])) << 4) + (fromhex (buf[2]))));
}

/* Read the remote registers into the block REGS.  */

void
remote_fetch_registers (regs)
     char *regs;
{
  char buf[PBUFSIZ];
  int i;
  char *p;

  sprintf (buf, "g");
  remote_send (buf);

  /* Reply describes registers byte by byte,
     each byte encoded as two hex characters.  */

  p = buf;
  for (i = 0; i < REGISTER_BYTES; i++)
    {
      if (p[0] == 0 || p[1] == 0)
	error ("Remote reply is too short: %s", buf);
      regs[i] = fromhex (p[0]) * 16 + fromhex (p[1]);
      p += 2;
    }
}

/* Store the remote registers from the contents of the block REGS.  */

void
remote_store_registers (regs)
     char *regs;
{
  char buf[PBUFSIZ];
  int i;
  char *p;

  buf[0] = 'G';
  
  /* Command describes registers byte by byte,
     each byte encoded as two hex characters.  */

  p = buf + 1;
  for (i = 0; i < REGISTER_BYTES; i++)
    {
      *p++ = tohex ((regs[i] >> 4) & 0xf);
      *p++ = tohex (regs[i] & 0xf);
    }
  *p = '\0';

  remote_send (buf);
}

#if 0
/* Read a word from remote address ADDR and return it.
   This goes through the data cache.  */

int
remote_fetch_word (addr)
     CORE_ADDR addr;
{
  if (icache)
    {
      extern CORE_ADDR text_start, text_end;

      if (addr >= text_start && addr < text_end)
	{
	  int buffer;
	  xfer_core_file (addr, &buffer, sizeof (int));
	  return buffer;
	}
    }
  return dcache_fetch (addr);
}

/* Write a word WORD into remote address ADDR.
   This goes through the data cache.  */

void
remote_store_word (addr, word)
     CORE_ADDR addr;
     int word;
{
  dcache_poke (addr, word);
}
#else /* not 0 */
void remote_fetch_word (addr)
     CORE_ADDR addr;
{
  error ("Internal error: remote_fetch_word is obsolete.\n");
}
void remote_store_word (addr)
     CORE_ADDR addr;
{
  error ("Internal error: remote_store_word is obsolete.\n");
}
#endif /* not 0 */

/* Write memory data directly to the remote machine.
   This does not inform the data cache; the data cache uses this.
   MEMADDR is the address in the remote memory space.
   MYADDR is the address of the buffer in our space.
   LEN is the number of bytes.  */

void
remote_write_bytes (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  char buf[PBUFSIZ];
  int i;
  char *p;

  if (len > PBUFSIZ / 2 - 20)
    abort ();

  sprintf (buf, "M%x,%x:", memaddr, len);

  /* Command describes registers byte by byte,
     each byte encoded as two hex characters.  */

  p = buf + strlen (buf);
  for (i = 0; i < len; i++)
    {
      *p++ = tohex ((myaddr[i] >> 4) & 0xf);
      *p++ = tohex (myaddr[i] & 0xf);
    }
  *p = '\0';

  remote_send (buf);
}

/* Read memory data directly from the remote machine.
   This does not use the data cache; the data cache uses this.
   MEMADDR is the address in the remote memory space.
   MYADDR is the address of the buffer in our space.
   LEN is the number of bytes.  */

void
remote_read_bytes (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  char buf[PBUFSIZ];
  int i;
  char *p;

  if (len > PBUFSIZ / 2 - 1)
    abort ();

  sprintf (buf, "m%x,%x", memaddr, len);
  remote_send (buf);

  /* Reply describes registers byte by byte,
     each byte encoded as two hex characters.  */

  p = buf;
  for (i = 0; i < len; i++)
    {
      if (p[0] == 0 || p[1] == 0)
	error ("Remote reply is too short: %s", buf);
      myaddr[i] = fromhex (p[0]) * 16 + fromhex (p[1]);
      p += 2;
    }
}

/* Read LEN bytes from inferior memory at MEMADDR.  Put the result
   at debugger address MYADDR.  Returns errno value.  */
int
remote_read_inferior_memory(memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  int xfersize;
  while (len > 0)
    {
      if (len > MAXBUFBYTES)
	xfersize = MAXBUFBYTES;
      else
	xfersize = len;

      remote_read_bytes (memaddr, myaddr, xfersize);
      memaddr += xfersize;
      myaddr  += xfersize;
      len     -= xfersize;
    }
  return 0; /* no error */
}

/* Copy LEN bytes of data from debugger memory at MYADDR
   to inferior's memory at MEMADDR.  Returns errno value.  */
int
remote_write_inferior_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  int xfersize;
  while (len > 0)
    {
      if (len > MAXBUFBYTES)
	xfersize = MAXBUFBYTES;
      else
	xfersize = len;
      
      remote_write_bytes(memaddr, myaddr, xfersize);
      
      memaddr += xfersize;
      myaddr  += xfersize;
      len     -= xfersize;
    }
  return 0; /* no error */
}

/*

A debug packet whose contents are <data>
is encapsulated for transmission in the form:

	$ <data> # CSUM1 CSUM2

	<data> must be ASCII alphanumeric and cannot include characters
	'$' or '#'

	CSUM1 and CSUM2 are ascii hex representation of an 8-bit 
	checksum of <data>, the most significant nibble is sent first.
	the hex digits 0-9,a-f are used.

Receiver responds with:

	+	- if CSUM is correct and ready for next packet
	-	- if CSUM is incorrect

*/

static int
readchar ()
{
  char buf;

  buf = '\0';
#ifdef HAVE_TERMIO
  /* termio does the timeout for us.  */
  read (remote_desc, &buf, 1);
#else
  alarm (timeout);
  read (remote_desc, &buf, 1);
  alarm (0);
#endif

  return buf & 0x7f;
}

/* Send the command in BUF to the remote machine,
   and read the reply into BUF.
   Report an error if we get an error reply.  */

static void
remote_send (buf)
     char *buf;
{
  int i;
  putpkt (buf);
  getpkt (buf);

  if (buf[0] == 'E')
    error ("Remote failure reply: %s", buf);
}

/* Send a packet to the remote machine, with error checking.
   The data of the packet is in BUF.  */

static void
putpkt (buf)
     char *buf;
{
  int i;
  unsigned char csum = 0;
  char buf2[500];
  char buf3[1];
  int cnt = strlen (buf);
  char ch;
  char *p;

  /* Copy the packet into buffer BUF2, encapsulating it
     and giving it a checksum.  */

  p = buf2;
  *p++ = '$';

  for (i = 0; i < cnt; i++)
    {
      csum += buf[i];
      *p++ = buf[i];
    }
  *p++ = '#';
  *p++ = tohex ((csum >> 4) & 0xf);
  *p++ = tohex (csum & 0xf);

  /* Send it over and over until we get a positive ack.  */

  do {
    if (kiodebug)
      {
	*p = '\0';
	printf ("Sending packet: %s (%s)\n", buf2, buf);
      }
    write (remote_desc, buf2, p - buf2);

    /* read until either a timeout occurs (\0) or '+' is read */
    do {
      ch = readchar ();
    } while ((ch != '+') && (ch != '\0'));
  } while (ch != '+');
}

/* Read a packet from the remote machine, with error checking,
   and store it in BUF.  */

static void
getpkt (buf)
     char *buf;
{
  char *bp;
  unsigned char csum;
  int c;
  unsigned char c1, c2;
  extern kiodebug;

  /* allow immediate quit while reading from device, it could be hung */
  immediate_quit++;

  while (1)
    {
      /* Force csum to be zero here because of possible error retry.  */
      csum = 0;
      
      while ((c = readchar()) != '$');

      bp = buf;
      while (1)
	{
	  c = readchar ();
	  if (c == '#')
	    break;
	  *bp++ = c;
	  csum += c;
	}
      *bp = 0;

      c1 = fromhex (readchar ());
      c2 = fromhex (readchar ());
      if ((csum & 0xff) == (c1 << 4) + c2)
	break;
      printf ("Bad checksum, sentsum=0x%x, csum=0x%x, buf=%s\n",
	      (c1 << 4) + c2, csum & 0xff, buf);
      write (remote_desc, "-", 1);
    }

  immediate_quit--;

  write (remote_desc, "+", 1);

  if (kiodebug)
    fprintf (stderr,"Packet received :%s\n", buf);
}

/* The data cache leads to incorrect results because it doesn't know about
   volatile variables, thus making it impossible to debug functions which
   use hardware registers.  Therefore it is #if 0'd out.  Effect on
   performance is some, for backtraces of functions with a few
   arguments each.  For functions with many arguments, the stack
   frames don't fit in the cache blocks, which makes the cache less
   helpful.  Disabling the cache is a big performance win for fetching
   large structures, because the cache code fetched data in 16-byte
   chunks.  */
#if 0
/* The data cache records all the data read from the remote machine
   since the last time it stopped.

   Each cache block holds 16 bytes of data
   starting at a multiple-of-16 address.  */

#define DCACHE_SIZE 64		/* Number of cache blocks */

struct dcache_block {
	struct dcache_block *next, *last;
	unsigned int addr;	/* Address for which data is recorded.  */
	int data[4];
};

struct dcache_block dcache_free, dcache_valid;

/* Free all the data cache blocks, thus discarding all cached data.  */ 

static void
dcache_flush ()
{
  register struct dcache_block *db;

  while ((db = dcache_valid.next) != &dcache_valid)
    {
      remque (db);
      insque (db, &dcache_free);
    }
}

/*
 * If addr is present in the dcache, return the address of the block 
 * containing it.
 */

struct dcache_block *
dcache_hit (addr)
{
  register struct dcache_block *db;

  if (addr & 3)
    abort ();

  /* Search all cache blocks for one that is at this address.  */
  db = dcache_valid.next;
  while (db != &dcache_valid)
    {
      if ((addr & 0xfffffff0) == db->addr)
	return db;
      db = db->next;
    }
  return NULL;
}

/*  Return the int data at address ADDR in dcache block DC.  */

int
dcache_value (db, addr)
     struct dcache_block *db;
     unsigned int addr;
{
  if (addr & 3)
    abort ();
  return (db->data[(addr>>2)&3]);
}

/* Get a free cache block, put it on the valid list,
   and return its address.  The caller should store into the block
   the address and data that it describes.  */

struct dcache_block *
dcache_alloc ()
{
  register struct dcache_block *db;

  if ((db = dcache_free.next) == &dcache_free)
    /* If we can't get one from the free list, take last valid */
    db = dcache_valid.last;

  remque (db);
  insque (db, &dcache_valid);
  return (db);
}

/* Return the contents of the word at address ADDR in the remote machine,
   using the data cache.  */

int
dcache_fetch (addr)
     CORE_ADDR addr;
{
  register struct dcache_block *db;

  db = dcache_hit (addr);
  if (db == 0)
    {
      db = dcache_alloc ();
      remote_read_bytes (addr & ~0xf, db->data, 16);
      db->addr = addr & ~0xf;
    }
  return (dcache_value (db, addr));
}

/* Write the word at ADDR both in the data cache and in the remote machine.  */

dcache_poke (addr, data)
     CORE_ADDR addr;
     int data;
{
  register struct dcache_block *db;

  /* First make sure the word is IN the cache.  DB is its cache block.  */
  db = dcache_hit (addr);
  if (db == 0)
    {
      db = dcache_alloc ();
      remote_read_bytes (addr & ~0xf, db->data, 16);
      db->addr = addr & ~0xf;
    }

  /* Modify the word in the cache.  */
  db->data[(addr>>2)&3] = data;

  /* Send the changed word.  */
  remote_write_bytes (addr, &data, 4);
}

/* Initialize the data cache.  */

dcache_init ()
{
  register i;
  register struct dcache_block *db;

  db = (struct dcache_block *) xmalloc (sizeof (struct dcache_block) * 
					DCACHE_SIZE);
  dcache_free.next = dcache_free.last = &dcache_free;
  dcache_valid.next = dcache_valid.last = &dcache_valid;
  for (i=0;i<DCACHE_SIZE;i++,db++)
    insque (db, &dcache_free);
}
#endif /* 0 */
