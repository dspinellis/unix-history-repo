/* dd -- convert a file while copying it.
   Copyright (C) 1985, 1990, 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         1       00140
 * --------------------         -----   ----------------------
 *
 * 20 Apr 93	Dave Burgess	silence compile warnings...
 *
 */

/* Written by Paul Rubin, David MacKenzie, and Stuart Kemp. */

/* Options:

   Numbers can be followed by a multiplier:
   b=512, k=1024, w=2, xm=number m

   if=FILE			Read from FILE instead of stdin.
   of=FILE			Write to FILE instead of stdout; don't
				truncate FILE.
   ibs=BYTES			Read BYTES bytes at a time.
   obs=BYTES			Write BYTES bytes at a time.
   bs=BYTES			Override ibs and obs.
   cbs=BYTES			Convert BYTES bytes at a time.
   skip=BLOCKS			Skip BLOCKS ibs-sized blocks at
				start of input.
   seek=BLOCKS			Skip BLOCKS obs-sized blocks at
				start of output.
   count=BLOCKS			Copy only BLOCKS input blocks.
   conv=CONVERSION[,CONVERSION...]

   Conversions:
   ascii			Convert EBCDIC to ASCII.
   ebcdic			Convert ASCII to EBCDIC.
   ibm				Convert ASCII to alternate EBCDIC.
   block			Pad newline-terminated records to size of
				cbs, replacing newline with trailing spaces.
   unblock			Replace trailing spaces in cbs-sized block
				with newline.
   lcase			Change uppercase characters to lowercase.
   ucase			Change lowercase characters to uppercase.
   swab				Swap every pair of input bytes.
				Unlike the Unix dd, this works when an odd
				number of bytes are read.
   noerror			Continue after read errors.
   sync				Pad every input block to size of ibs with
				trailing NULs. */

#include <stdio.h>
#include <ctype.h>
#define ISLOWER islower
#define ISUPPER isupper
#include <sys/types.h>
#include <signal.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <fcntl.h>
#define	SIGTYPE	void

#define equal(p, q) (strcmp ((p),(q)) == 0)
#define max(a, b) ((a) > (b) ? (a) : (b))
#define output_char(c) \
  do { \
  obuf[oc++] = (c); if (oc >= output_blocksize) write_output (); \
  } while (0)

/* Default input and output blocksize. */
#define DEFAULT_BLOCKSIZE 512

/* Conversions bit masks. */
#define C_ASCII 01
#define C_EBCDIC 02
#define C_IBM 04
#define C_BLOCK 010
#define C_UNBLOCK 020
#define C_LCASE 040
#define C_UCASE 0100
#define C_SWAB 0200
#define C_NOERROR 0400
#define C_NOTRUNC 01000
#define C_SYNC 02000
/* Use separate input and output buffers, and combine partial input blocks. */
#define C_TWOBUFS 04000

/*char *malloc ();*/
SIGTYPE interrupt_handler ();
int bit_count ();
int parse_integer ();
void apply_translations ();
void copy ();
void copy_simple ();
void copy_with_block ();
void copy_with_unblock ();
void error ();
void parse_conversion ();
void print_stats ();
void translate_charset ();
void quit ();
void scanargs ();
void skip ();
void usage ();
void write_output ();

/* The name this program was run with. */
char *program_name;

/* The name of the input file, or NULL for the standard input. */
char *input_file = NULL;

/* The input file descriptor. */
int input_fd = 0;

/* The name of the output file, or NULL for the standard output. */
char *output_file = NULL;

/* The output file descriptor. */
int output_fd = 1;

/* The number of bytes in which atomic reads are done. */
long input_blocksize = -1;

/* The number of bytes in which atomic writes are done. */
long output_blocksize = -1;

/* Conversion buffer size, in bytes.  0 prevents conversions. */
long conversion_blocksize = 0;

/* Skip this many records of `input_blocksize' bytes before input. */
long skip_records = 0;

/* Skip this many records of `output_blocksize' bytes before output. */
long seek_record = 0;

/* Copy only this many records.  <0 means no limit. */
int max_records = -1;

/* Bit vector of conversions to apply. */
int conversions_mask = 0;

/* If nonzero, filter characters through the translation table.  */
int translation_needed = 0;

/* Number of partial blocks written. */
unsigned w_partial = 0;

/* Number of full blocks written. */
unsigned w_full = 0;

/* Number of partial blocks read. */
unsigned r_partial = 0;

/* Number of full blocks read. */
unsigned r_full = 0;

/* Records truncated by conv=block. */
unsigned r_truncate = 0;

/* Output representation of newline and space characters.
   They change if we're converting to EBCDIC.  */
unsigned char newline_character = '\n';
unsigned char space_character = ' ';

struct conversion
{
  char *convname;
  int conversion;
};

struct conversion conversions[] =
{
  "ascii", C_ASCII | C_TWOBUFS,	/* EBCDIC to ASCII. */
  "ebcdic", C_EBCDIC | C_TWOBUFS,	/* ASCII to EBCDIC. */
  "ibm", C_IBM | C_TWOBUFS,	/* Slightly different ASCII to EBCDIC. */
  "block", C_BLOCK | C_TWOBUFS,	/* Variable to fixed length records. */
  "unblock", C_UNBLOCK | C_TWOBUFS,	/* Fixed to variable length records. */
  "lcase", C_LCASE | C_TWOBUFS,	/* Translate upper to lower case. */
  "ucase", C_UCASE | C_TWOBUFS,	/* Translate lower to upper case. */
  "swab", C_SWAB | C_TWOBUFS,	/* Swap bytes of input. */
  "noerror", C_NOERROR,		/* Ignore i/o errors. */
  "notrunc", C_NOTRUNC,		/* Do not truncate output file. */
  "sync", C_SYNC,		/* Pad input records to ibs with NULs. */
  NULL, 0
};

/* Translation table formed by applying successive transformations. */
unsigned char trans_table[256];

unsigned char ascii_to_ebcdic[] =
{
  0, 01, 02, 03, 067, 055, 056, 057,
  026, 05, 045, 013, 014, 015, 016, 017,
  020, 021, 022, 023, 074, 075, 062, 046,
  030, 031, 077, 047, 034, 035, 036, 037,
  0100, 0117, 0177, 0173, 0133, 0154, 0120, 0175,
  0115, 0135, 0134, 0116, 0153, 0140, 0113, 0141,
  0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367,
  0370, 0371, 0172, 0136, 0114, 0176, 0156, 0157,
  0174, 0301, 0302, 0303, 0304, 0305, 0306, 0307,
  0310, 0311, 0321, 0322, 0323, 0324, 0325, 0326,
  0327, 0330, 0331, 0342, 0343, 0344, 0345, 0346,
  0347, 0350, 0351, 0112, 0340, 0132, 0137, 0155,
  0171, 0201, 0202, 0203, 0204, 0205, 0206, 0207,
  0210, 0211, 0221, 0222, 0223, 0224, 0225, 0226,
  0227, 0230, 0231, 0242, 0243, 0244, 0245, 0246,
  0247, 0250, 0251, 0300, 0152, 0320, 0241, 07,
  040, 041, 042, 043, 044, 025, 06, 027,
  050, 051, 052, 053, 054, 011, 012, 033,
  060, 061, 032, 063, 064, 065, 066, 010,
  070, 071, 072, 073, 04, 024, 076, 0341,
  0101, 0102, 0103, 0104, 0105, 0106, 0107, 0110,
  0111, 0121, 0122, 0123, 0124, 0125, 0126, 0127,
  0130, 0131, 0142, 0143, 0144, 0145, 0146, 0147,
  0150, 0151, 0160, 0161, 0162, 0163, 0164, 0165,
  0166, 0167, 0170, 0200, 0212, 0213, 0214, 0215,
  0216, 0217, 0220, 0232, 0233, 0234, 0235, 0236,
  0237, 0240, 0252, 0253, 0254, 0255, 0256, 0257,
  0260, 0261, 0262, 0263, 0264, 0265, 0266, 0267,
  0270, 0271, 0272, 0273, 0274, 0275, 0276, 0277,
  0312, 0313, 0314, 0315, 0316, 0317, 0332, 0333,
  0334, 0335, 0336, 0337, 0352, 0353, 0354, 0355,
  0356, 0357, 0372, 0373, 0374, 0375, 0376, 0377
};

unsigned char ascii_to_ibm[] =
{
  0, 01, 02, 03, 067, 055, 056, 057,
  026, 05, 045, 013, 014, 015, 016, 017,
  020, 021, 022, 023, 074, 075, 062, 046,
  030, 031, 077, 047, 034, 035, 036, 037,
  0100, 0132, 0177, 0173, 0133, 0154, 0120, 0175,
  0115, 0135, 0134, 0116, 0153, 0140, 0113, 0141,
  0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367,
  0370, 0371, 0172, 0136, 0114, 0176, 0156, 0157,
  0174, 0301, 0302, 0303, 0304, 0305, 0306, 0307,
  0310, 0311, 0321, 0322, 0323, 0324, 0325, 0326,
  0327, 0330, 0331, 0342, 0343, 0344, 0345, 0346,
  0347, 0350, 0351, 0255, 0340, 0275, 0137, 0155,
  0171, 0201, 0202, 0203, 0204, 0205, 0206, 0207,
  0210, 0211, 0221, 0222, 0223, 0224, 0225, 0226,
  0227, 0230, 0231, 0242, 0243, 0244, 0245, 0246,
  0247, 0250, 0251, 0300, 0117, 0320, 0241, 07,
  040, 041, 042, 043, 044, 025, 06, 027,
  050, 051, 052, 053, 054, 011, 012, 033,
  060, 061, 032, 063, 064, 065, 066, 010,
  070, 071, 072, 073, 04, 024, 076, 0341,
  0101, 0102, 0103, 0104, 0105, 0106, 0107, 0110,
  0111, 0121, 0122, 0123, 0124, 0125, 0126, 0127,
  0130, 0131, 0142, 0143, 0144, 0145, 0146, 0147,
  0150, 0151, 0160, 0161, 0162, 0163, 0164, 0165,
  0166, 0167, 0170, 0200, 0212, 0213, 0214, 0215,
  0216, 0217, 0220, 0232, 0233, 0234, 0235, 0236,
  0237, 0240, 0252, 0253, 0254, 0255, 0256, 0257,
  0260, 0261, 0262, 0263, 0264, 0265, 0266, 0267,
  0270, 0271, 0272, 0273, 0274, 0275, 0276, 0277,
  0312, 0313, 0314, 0315, 0316, 0317, 0332, 0333,
  0334, 0335, 0336, 0337, 0352, 0353, 0354, 0355,
  0356, 0357, 0372, 0373, 0374, 0375, 0376, 0377
};

unsigned char ebcdic_to_ascii[] =
{
  0, 01, 02, 03, 0234, 011, 0206, 0177,
  0227, 0215, 0216, 013, 014, 015, 016, 017,
  020, 021, 022, 023, 0235, 0205, 010, 0207,
  030, 031, 0222, 0217, 034, 035, 036, 037,
  0200, 0201, 0202, 0203, 0204, 012, 027, 033,
  0210, 0211, 0212, 0213, 0214, 05, 06, 07,
  0220, 0221, 026, 0223, 0224, 0225, 0226, 04,
  0230, 0231, 0232, 0233, 024, 025, 0236, 032,
  040, 0240, 0241, 0242, 0243, 0244, 0245, 0246,
  0247, 0250, 0133, 056, 074, 050, 053, 041,
  046, 0251, 0252, 0253, 0254, 0255, 0256, 0257,
  0260, 0261, 0135, 044, 052, 051, 073, 0136,
  055, 057, 0262, 0263, 0264, 0265, 0266, 0267,
  0270, 0271, 0174, 054, 045, 0137, 076, 077,
  0272, 0273, 0274, 0275, 0276, 0277, 0300, 0301,
  0302, 0140, 072, 043, 0100, 047, 075, 042,
  0303, 0141, 0142, 0143, 0144, 0145, 0146, 0147,
  0150, 0151, 0304, 0305, 0306, 0307, 0310, 0311,
  0312, 0152, 0153, 0154, 0155, 0156, 0157, 0160,
  0161, 0162, 0313, 0314, 0315, 0316, 0317, 0320,
  0321, 0176, 0163, 0164, 0165, 0166, 0167, 0170,
  0171, 0172, 0322, 0323, 0324, 0325, 0326, 0327,
  0330, 0331, 0332, 0333, 0334, 0335, 0336, 0337,
  0340, 0341, 0342, 0343, 0344, 0345, 0346, 0347,
  0173, 0101, 0102, 0103, 0104, 0105, 0106, 0107,
  0110, 0111, 0350, 0351, 0352, 0353, 0354, 0355,
  0175, 0112, 0113, 0114, 0115, 0116, 0117, 0120,
  0121, 0122, 0356, 0357, 0360, 0361, 0362, 0363,
  0134, 0237, 0123, 0124, 0125, 0126, 0127, 0130,
  0131, 0132, 0364, 0365, 0366, 0367, 0370, 0371,
  060, 061, 062, 063, 064, 065, 066, 067,
  070, 071, 0372, 0373, 0374, 0375, 0376, 0377
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  struct sigaction sigact;
  int i;

  program_name = argv[0];

  /* Initialize translation table to identity translation. */
  for (i = 0; i < 256; i++)
    trans_table[i] = i;

  /* Decode arguments. */
  scanargs (argc, argv);
  apply_translations ();

  if (input_file != NULL)
    {
      input_fd = open (input_file, O_RDONLY);
      if (input_fd < 0)
	error (1, errno, "%s", input_file);
    }
  else
    input_file = "standard input";

  if (input_fd == output_fd)
    error (1, 0, "standard %s is closed", input_fd == 0 ? "input" : "output");

  if (output_file != NULL)
    {
      int omode = O_RDWR | O_CREAT;

      if (seek_record == 0 && !(conversions_mask & C_NOTRUNC))
	omode |= O_TRUNC;
      output_fd = open (output_file, omode, 0666);
      if (output_fd < 0)
	error (1, errno, "%s", output_file);
#ifndef FTRUNCATE_MISSING
      if (seek_record > 0 && !(conversions_mask & C_NOTRUNC))
	{
	  if (ftruncate (output_fd, seek_record * output_blocksize) < 0)
	    error (0, errno, "%s", output_file);
	}
#endif
    }
  else
    output_file = "standard output";
  
  sigaction (SIGINT, NULL, &sigact);
  if (sigact.sa_handler != SIG_IGN)
    {
      sigact.sa_handler = interrupt_handler;
      sigemptyset (&sigact.sa_mask);
      sigact.sa_flags = 0;
      sigaction (SIGINT, &sigact, NULL);
    }
  copy ();
}

/* Throw away RECORDS blocks of BLOCKSIZE bytes on file descriptor FDESC,
   which is open with read permission for FILE.  Store up to BLOCKSIZE
   bytes of the data at a time in BUF, if necessary. */

void
skip (fdesc, file, records, blocksize, buf)
     int fdesc;
     char *file;
     long records;
     long blocksize;
     char *buf;
{
  struct stat stats;

  /* Use fstat instead of checking for errno == ESPIPE because
     lseek doesn't work on some special files but doesn't return an
     error, either. */
  if (fstat (fdesc, &stats))
    {
      error (0, errno, "%s", file);
      quit (1);
    }

  if (S_ISREG (stats.st_mode))
    {
      if (lseek (fdesc, records * blocksize, SEEK_SET) < 0)
	{
	  error (0, errno, "%s", file);
	  quit (1);
	}
    }
  else
    {
      while (records-- > 0)
	{
	  if (read (fdesc, buf, blocksize) < 0)
	    {
	      error (0, errno, "%s", file);
	      quit (1);
	    }
	  /* FIXME If fewer bytes were read than requested, meaning that
	     EOF was reached, POSIX wants the output file padded with NULs. */
	}
    }
}

/* Apply the character-set translations specified by the user
   to the NREAD bytes in BUF.  */

void
translate_buffer (buf, nread)
     unsigned char *buf;
     int nread;
{
  register unsigned char *cp;
  register int i;

  for (i = nread, cp = buf; i; i--, cp++)
    *cp = trans_table[*cp];
}

/* If nonnzero, the last char from the previous call to `swab_buffer'
   is saved in `saved_char'.  */
int char_is_saved = 0;

/* Odd char from previous call.  */
unsigned char saved_char;

/* Swap NREAD bytes in BUF, plus possibly an initial char from the
   previous call.  If NREAD is odd, save the last char for the
   next call.   Return the new start of the BUF buffer.  */

unsigned char *
swab_buffer (buf, nread)
     unsigned char *buf;
     int *nread;
{
  unsigned char *bufstart = buf;
  register unsigned char *cp;
  register int i;

  /* Is a char left from last time?  */
  if (char_is_saved)
    {
      *--bufstart = saved_char;
      *nread++;
      char_is_saved = 0;
    }

  if (*nread & 1)
    {
      /* An odd number of chars are in the buffer.  */
      saved_char = bufstart[--*nread];
      char_is_saved = 1;
    }

  /* Do the byte-swapping by moving every second character two
     positions toward the end, working from the end of the buffer
     toward the beginning.  This way we only move half of the data.  */

  cp = bufstart + *nread;	/* Start one char past the last.  */
  for (i = *nread / 2; i; i--, cp -= 2)
    *cp = *(cp - 2);

  return ++bufstart;
}

/* Output buffer. */
unsigned char *obuf;

/* Current index into `obuf'. */
int oc = 0;

/* Index into current line, for `conv=block' and `conv=unblock'.  */
int col = 0;

/* The main loop.  */

void
copy ()
{
  unsigned char *ibuf, *bufstart; /* Input buffer. */
  int nread;			/* Bytes read in the current block. */
  int exit_status = 0;

  /* Leave an extra byte at the beginning and end of `ibuf' for conv=swab.  */
  ibuf = (unsigned char *) malloc (input_blocksize + 2) + 1;
  if (conversions_mask & C_TWOBUFS)
    obuf = (unsigned char *) malloc (output_blocksize);
  else
    obuf = ibuf;

  if (skip_records > 0)
    skip (input_fd, input_file, skip_records, input_blocksize, ibuf);

  if (seek_record > 0)
    skip (output_fd, output_file, seek_record, output_blocksize, obuf);

  if (max_records == 0)
    quit (exit_status);

  while (1)
    {
      if (max_records >= 0 && r_partial + r_full >= max_records)
	break;

      /* Zero the buffer before reading, so that if we get a read error,
	 whatever data we are able to read is followed by zeros.
	 This minimizes data loss. */
      if ((conversions_mask & C_SYNC) && (conversions_mask & C_NOERROR))
	bzero (ibuf, input_blocksize);

      nread = read (input_fd, ibuf, input_blocksize);

      if (nread == 0)
	break;			/* EOF.  */

      if (nread < 0)
	{
	  error (0, errno, "%s", input_file);
	  if (conversions_mask & C_NOERROR)
	    {
	      print_stats ();
	      /* Seek past the bad block if possible. */
	      lseek (input_fd, input_blocksize, SEEK_CUR);
	      if (conversions_mask & C_SYNC)
		/* Replace the missing input with null bytes and
		   proceed normally.  */
		nread = 0;
	      else
		continue;
	    }
	  else
	    {
	      /* Write any partial block. */
	      exit_status = 2;
	      break;
	    }
	}

      if (nread < input_blocksize)
	{
	  r_partial++;
	  if (conversions_mask & C_SYNC)
	    {
	      if (!(conversions_mask & C_NOERROR))
		/* If C_NOERROR, we zeroed the block before reading. */
		bzero (ibuf + nread, input_blocksize - nread);
	      nread = input_blocksize;
	    }
	}
      else
	r_full++;

      if (ibuf == obuf)		/* If not C_TWOBUFS. */
	{
	  int nwritten = write (output_fd, obuf, nread);
	  if (nwritten != nread)
	    {
	      error (0, errno, "%s", output_file);
	      if (nwritten > 0)
		w_partial++;
	      quit (1);
	    }
	  else if (nread == input_blocksize)
	    w_full++;
	  else
	    w_partial++;
	  continue;
	}

      /* Do any translations on the whole buffer at once.  */

      if (translation_needed)
	translate_buffer (ibuf, nread);

      if (conversions_mask & C_SWAB)
	bufstart = swab_buffer (ibuf, &nread);
      else
	bufstart = ibuf;

      if (conversions_mask & C_BLOCK)
        copy_with_block (bufstart, nread);
      else if (conversions_mask & C_UNBLOCK)
	copy_with_unblock (bufstart, nread);
      else
	copy_simple (bufstart, nread);
    }

  /* If we have a char left as a result of conv=swab, output it.  */
  if (char_is_saved)
    {
      if (conversions_mask & C_BLOCK)
        copy_with_block (&saved_char, 1);
      else if (conversions_mask & C_UNBLOCK)
	copy_with_unblock (&saved_char, 1);
      else
	output_char (saved_char);
    }

  if ((conversions_mask & C_BLOCK) && col > 0)
    {
      /* If the final input line didn't end with a '\n', pad
	 the output block to `conversion_blocksize' chars.  */
      int pending_spaces = max (0, conversion_blocksize - col);
      while (pending_spaces--)
	output_char (space_character);
    }

  if ((conversions_mask & C_UNBLOCK) && col == conversion_blocksize)
    /* Add a final '\n' if there are exactly `conversion_blocksize'
       characters in the final record. */
    output_char (newline_character);

  /* Write out the last block. */
  if (oc > 0)
    {
      int nwritten = write (output_fd, obuf, oc);
      if (nwritten > 0)
	w_partial++;
      if (nwritten != oc)
	{
	  error (0, errno, "%s", output_file);
	  quit (1);
	}
    }

  free (ibuf - 1);
  if (obuf != ibuf)
    free (obuf);

  quit (exit_status);
}

/* Copy NREAD bytes of BUF, with no conversions.  */

void
copy_simple (buf, nread)
     unsigned char *buf;
     int nread;
{
  int nfree;			/* Number of unused bytes in `obuf'.  */
  unsigned char *start = buf; /* First uncopied char in BUF.  */

  do
    {
      nfree = output_blocksize - oc;
      if (nfree > nread)
	nfree = nread;

      bcopy (start, obuf + oc, nfree);
	    
      nread -= nfree;		/* Update the number of bytes left to copy. */
      start += nfree;
      oc += nfree;
      if (oc >= output_blocksize)
	write_output ();
    }
  while (nread > 0);
}

/* Copy NREAD bytes of BUF, doing conv=block
   (pad newline-terminated records to `conversion_blocksize',
   replacing the newline with trailing spaces).  */

void
copy_with_block (buf, nread)
     unsigned char *buf;
     int nread;
{
  register int i;

  for (i = nread; i; i--, buf++)
    {
      if (*buf == newline_character)
	{
	  int pending_spaces = max (0, conversion_blocksize - col);
	  while (pending_spaces--)
	    output_char (space_character);
	  col = 0;
	}
      else
	{
	  if (col == conversion_blocksize)
	    r_truncate++;
	  else if (col < conversion_blocksize)
	    output_char (*buf);
	  col++;
	}
    }
}

/* Copy NREAD bytes of BUF, doing conv=unblock
   (replace trailing spaces in `conversion_blocksize'-sized records
   with a newline).  */

void
copy_with_unblock (buf, nread)
     unsigned char *buf;
     int nread;
{
  register int i;
  register unsigned char c;
  static int pending_spaces = 0;

  for (i = 0; i < nread; i++)
    {
      c = buf[i];

      if (col++ >= conversion_blocksize)
	{
	  col = pending_spaces = 0; /* Wipe out any pending spaces.  */
	  i--;			/* Push the char back; get it later. */
	  output_char (newline_character);
	}
      else if (c == space_character)
	pending_spaces++;
      else
	{
	  if (pending_spaces)
	    {
	      /* `c' is the character after a run of spaces that were not
		 at the end of the conversion buffer.  Output them.  */
	      while (pending_spaces--)
		output_char (space_character);
	    }
	  output_char (c);
	}
    }
}

/* Write, then empty, the output buffer `obuf'. */

void
write_output ()
{
  int nwritten = write (output_fd, obuf, output_blocksize);
  if (nwritten != output_blocksize)
    {
      error (0, errno, "%s", output_file);
      if (nwritten > 0)
	w_partial++;
      quit (1);
    }
  else
    w_full++;
  oc = 0;
}

void
scanargs (argc, argv)
     int argc;
     char **argv;
{
  int i, n;

  for (i = 1; i < argc; i++)
    {
      char *name, *val;

      name = argv[i];
      val = index (name, '=');
      if (val == NULL)
	usage ("unrecognized option `%s'", name);
      *val++ = '\0';

      if (equal (name, "if"))
	input_file = val;
      else if (equal (name, "of"))
	output_file = val;
      else if (equal (name, "conv"))
	parse_conversion (val);
      else
	{
	  n = parse_integer (val);
	  if (n < 0)
	    error (1, 0, "invalid number `%s'", val);

	  if (equal (name, "ibs"))
	    {
	      input_blocksize = n;
	      conversions_mask |= C_TWOBUFS;
	    }
	  else if (equal (name, "obs"))
	    {
	      output_blocksize = n;
	      conversions_mask |= C_TWOBUFS;
	    }
	  else if (equal (name, "bs"))
	    output_blocksize = input_blocksize = n;
	  else if (equal (name, "cbs"))
	    conversion_blocksize = n;
	  else if (equal (name, "skip"))
	    skip_records = n;
	  else if (equal (name, "seek"))
	    seek_record = n;
	  else if (equal (name, "count"))
	    max_records = n;
	  else
	    usage ("unrecognized option `%s=%s'", name, val);
	}
    }

  /* If bs= was given, both `input_blocksize' and `output_blocksize' will
     have been set to non-negative values.  If either has not been set,
     bs= was not given, so make sure two buffers are used. */
  if (input_blocksize == -1 || output_blocksize == -1)
    conversions_mask |= C_TWOBUFS;
  if (input_blocksize == -1)
    input_blocksize = DEFAULT_BLOCKSIZE;
  if (output_blocksize == -1)
    output_blocksize = DEFAULT_BLOCKSIZE;
  if (conversion_blocksize == 0)
    conversions_mask &= ~(C_BLOCK | C_UNBLOCK);
}

/* Return the value of STR, interpreted as a non-negative decimal integer,
   optionally multiplied by various values.
   Return -1 if STR does not represent a number in this format. */

int
parse_integer (str)
     char *str;
{
  register int n = 0;
  register int temp;
  register char *p = str;

  while (isdigit (*p))
    {
      n = n * 10 + *p - '0';
      p++;
    }
loop:
  switch (*p++)
    {
    case '\0':
      return n;
    case 'b':
      n *= 512;
      goto loop;
    case 'k':
      n *= 1024;
      goto loop;
    case 'w':
      n *= 2;
      goto loop;
    case 'x':
      temp = parse_integer (p);
      if (temp == -1)
	return -1;
      n *= temp;
      break;
    default:
      return -1;
    }
  return n;
}

/* Interpret one "conv=..." option. */

void
parse_conversion (str)
     char *str;
{
  char *new;
  int i;

  do
    {
      new = index (str, ',');
      if (new != NULL)
	*new++ = '\0';
      for (i = 0; conversions[i].convname != NULL; i++)
	if (equal (conversions[i].convname, str))
	  {
	    conversions_mask |= conversions[i].conversion;
	    break;
	  }
      if (conversions[i].convname == NULL)
	{
	  usage ("%s: invalid conversion", str);
	  exit (1);
	}
      str = new;
  } while (new != NULL);
}

/* Fix up translation table. */

void
apply_translations ()
{
  int i;

#define MX(a) (bit_count (conversions_mask & (a)))
  if ((MX (C_ASCII | C_EBCDIC | C_IBM) > 1)
      || (MX (C_BLOCK | C_UNBLOCK) > 1)
      || (MX (C_LCASE | C_UCASE) > 1)
      || (MX (C_UNBLOCK | C_SYNC) > 1))
    {
      error (1, 0, "\
only one conv in {ascii,ebcdic,ibm}, {lcase,ucase}, {block,unblock}, {unblock,sync}");
    }
#undef MX

  if (conversions_mask & C_ASCII)
    translate_charset (ebcdic_to_ascii);

  if (conversions_mask & C_UCASE)
    {
      for (i = 0; i < 256; i++)
	if (ISLOWER (trans_table[i]))
	  trans_table[i] = toupper (trans_table[i]);
      translation_needed = 1;
    }
  else if (conversions_mask & C_LCASE)
    {
      for (i = 0; i < 256; i++)
	if (ISUPPER (trans_table[i]))
	  trans_table[i] = tolower (trans_table[i]);
      translation_needed = 1;
    }

  if (conversions_mask & C_EBCDIC)
    {
      translate_charset (ascii_to_ebcdic);
      newline_character = ascii_to_ebcdic['\n'];
      space_character = ascii_to_ebcdic[' '];
    }
  else if (conversions_mask & C_IBM)
    {
      translate_charset (ascii_to_ibm);
      newline_character = ascii_to_ibm['\n'];
      space_character = ascii_to_ibm[' '];
    }
}

void
translate_charset (new_trans)
     unsigned char *new_trans;
{
  int i;

  for (i = 0; i < 256; i++)
    trans_table[i] = new_trans[trans_table[i]];
  translation_needed = 1;
}

/* Return the number of 1 bits in `i'. */

int
bit_count (i)
     register unsigned int i;
{
  register int set_bits;

  for (set_bits = 0; i != 0; set_bits++)
    i &= i - 1;
  return set_bits;
}

void
print_stats ()
{
  fprintf (stderr, "%u+%u records in\n", r_full, r_partial);
  fprintf (stderr, "%u+%u records out\n", w_full, w_partial);
  if (r_truncate > 0)
    fprintf (stderr, "%u truncated block%s\n", r_truncate,
	     r_truncate == 1 ? "" : "s");
}

void
quit (code)
     int code;
{
  int errcode = code ? code : 1;
  print_stats ();
  if (close (input_fd) < 0)
    error (errcode, errno, "%s", input_file);
  if (close (output_fd) < 0)
    error (errcode, errno, "%s", output_file);
  exit (code);
}

SIGTYPE
interrupt_handler ()
{
  quit (1);
}

void
usage (string, arg0, arg1)
     char *string, *arg0, *arg1;
{
  fprintf (stderr, "%s: ", program_name);
  fprintf (stderr, string, arg0, arg1);
  fprintf (stderr, "\n");
  fprintf (stderr, "\
Usage: %s [if=file] [of=file] [ibs=bytes] [obs=bytes] [bs=bytes] [cbs=bytes]\n\
       [skip=blocks] [seek=blocks] [count=blocks]\n\
       [conv={ascii,ebcdic,ibm,block,unblock,lcase,ucase,swab,noerror,notrunc,\n\
       sync}]\n\
Numbers can be followed by a multiplier:\n\
b=512, k=1024, w=2, xm=number m\n",
	   program_name);
  exit (1);
}

void
error(n,e,s,s1) 
int n, e, s1;
char *s;
{
  if(e) 
    fprintf(stderr,"error %d:", e);
  fprintf(stderr,s, s1);
  if(n) 
    exit(n);
}
