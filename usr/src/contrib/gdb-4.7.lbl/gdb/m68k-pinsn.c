/* Print Motorola 68k instructions for GDB, the GNU debugger.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

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

#include "defs.h"
#include "symtab.h"
#include "opcode/m68k.h"
#include "gdbcore.h"
#include "ieee-float.h"

/* Local function prototypes */

static int
fetch_arg PARAMS ((unsigned char *, int, int));

static void
print_base PARAMS ((int, int, FILE *));

static unsigned char *
print_indexed PARAMS ((int, unsigned char *, CORE_ADDR, FILE *));

static unsigned char *
print_insn_arg PARAMS ((char *, unsigned char *, unsigned char *, CORE_ADDR,
			FILE *));

/* 68k instructions are never longer than this many bytes.  */
#define MAXLEN 22

/* Number of elements in the opcode table.  */
#define NOPCODES (sizeof m68k_opcodes / sizeof m68k_opcodes[0])

const char * const fpcr_names[] = {
  "", "fpiar", "fpsr", "fpiar/fpsr", "fpcr",
  "fpiar/fpcr", "fpsr/fpcr", "fpiar/fpsr/fpcr"};

/* Define accessors for 68K's 1, 2, and 4-byte signed quantities.
   The _SHIFT values move the quantity to the high order end of an
   `int' value, so it will sign-extend.  Probably a few more casts
   are needed to make it compile without warnings on finicky systems.  */
#define	BITS_PER_BYTE	8
#define	BYTE_SHIFT (BITS_PER_BYTE * ((sizeof (int)) - 1))
#define	WORD_SHIFT (BITS_PER_BYTE * ((sizeof (int)) - 2))
#define	LONG_SHIFT (BITS_PER_BYTE * ((sizeof (int)) - 4))

#define NEXTBYTE(p)  (p += 2, ((int)(p[-1]) << BYTE_SHIFT) >> BYTE_SHIFT)

#define NEXTWORD(p)  \
  (p += 2, (((int)((p[-2] << 8) + p[-1])) << WORD_SHIFT) >> WORD_SHIFT)

#define NEXTLONG(p)  \
  (p += 4, (((int)((((((p[-4] << 8) + p[-3]) << 8) + p[-2]) << 8) + p[-1])) \
				   << LONG_SHIFT) >> LONG_SHIFT)

/* Ecch -- assumes host == target float formats.  FIXME.  */
#define NEXTSINGLE(p) \
  (p += 4, *((float *)(p - 4)))

#define NEXTDOUBLE(p) \
  (p += 8, *((double *)(p - 8)))

/* Print the m68k instruction at address MEMADDR in debugged memory,
   on STREAM.  Returns length of the instruction, in bytes.  */

int
print_insn (memaddr, stream)
     CORE_ADDR memaddr;
     FILE *stream;
{
  unsigned char buffer[MAXLEN];
  register int i;
  register unsigned char *p;
  register char *d;
  register int bestmask;
  int best;

  read_memory (memaddr, (char *) buffer, MAXLEN);

  bestmask = 0;
  best = -1;
  for (i = 0; i < NOPCODES; i++)
    {
      register unsigned int opcode = m68k_opcodes[i].opcode;
      register unsigned int match = m68k_opcodes[i].match;
      if (((0xff & buffer[0] & (match >> 24)) == (0xff & (opcode >> 24)))
	  && ((0xff & buffer[1] & (match >> 16)) == (0xff & (opcode >> 16)))
	  && ((0xff & buffer[2] & (match >> 8)) == (0xff & (opcode >> 8)))
	  && ((0xff & buffer[3] & match) == (0xff & opcode)))
	{
	  /* Don't use for printout the variants of divul and divsl
	     that have the same register number in two places.
	     The more general variants will match instead.  */
	  for (d = m68k_opcodes[i].args; *d; d += 2)
	    if (d[1] == 'D')
	      break;

	  /* Don't use for printout the variants of most floating
	     point coprocessor instructions which use the same
	     register number in two places, as above. */
	  if (*d == 0)
	    for (d = m68k_opcodes[i].args; *d; d += 2)
	      if (d[1] == 't')
		break;

	  if (*d == 0 && match > bestmask)
	    {
	      best = i;
	      bestmask = match;
	    }
	}
    }

  /* Handle undefined instructions.  */
  if (best < 0)
    {
      fprintf_filtered (stream, "0%o", (buffer[0] << 8) + buffer[1]);
      return 2;
    }

  fprintf_filtered (stream, "%s", m68k_opcodes[best].name);

  /* Point at first word of argument data,
     and at descriptor for first argument.  */
  p = buffer + 2;
  
  /* Why do this this way? -MelloN */
  for (d = m68k_opcodes[best].args; *d; d += 2)
    {
      if (d[0] == '#')
	{
	  if (d[1] == 'l' && p - buffer < 6)
	    p = buffer + 6;
	  else if (p - buffer < 4 && d[1] != 'C' && d[1] != '8' )
	    p = buffer + 4;
	}
      if (d[1] >= '1' && d[1] <= '3' && p - buffer < 4)
	p = buffer + 4;
      if (d[1] >= '4' && d[1] <= '6' && p - buffer < 6)
	p = buffer + 6;
      if ((d[0] == 'L' || d[0] == 'l') && d[1] == 'w' && p - buffer < 4)
	p = buffer + 4;
    }

  d = m68k_opcodes[best].args;

  if (*d)
    fputs_filtered (" ", stream);

  while (*d)
    {
      p = print_insn_arg (d, buffer, p, memaddr + p - buffer, stream);
      d += 2;
      if (*d && *(d - 2) != 'I' && *d != 'k')
	fputs_filtered (",", stream);
    }
  return p - buffer;
}

static unsigned char *
print_insn_arg (d, buffer, p, addr, stream)
     char *d;
     unsigned char *buffer;
     register unsigned char *p;
     CORE_ADDR addr;		/* PC for this arg to be relative to */
     FILE *stream;
{
  register int val;
  register int place = d[1];
  int regno;
  register const char *regname;
  register unsigned char *p1;
  double flval;
  int flt_p;

  switch (*d)
    {
    case 'c':		/* cache identifier */
      {
        static char *cacheFieldName[] = { "NOP", "dc", "ic", "bc" };
        val = fetch_arg (buffer, place, 2);
        fprintf_filtered (stream, cacheFieldName[val]);
        break;
      }

    case 'a':		/* address register indirect only. Cf. case '+'. */
      {
        fprintf_filtered (stream,
			  "%s@",
			  reg_names [fetch_arg (buffer, place, 3) + 8]);
        break;
      }

    case '_':		/* 32-bit absolute address for move16. */
      {
        val = NEXTLONG (p);
        fprintf_filtered (stream, "@#");
	print_address (val, stream);
        break;
      }

    case 'C':
      fprintf_filtered (stream, "ccr");
      break;

    case 'S':
      fprintf_filtered (stream, "sr");
      break;

    case 'U':
      fprintf_filtered (stream, "usp");
      break;

    case 'J':
      {
	static struct { char *name; int value; } names[]
	  = {{"sfc", 0x000}, {"dfc", 0x001}, {"cacr", 0x002},
	     {"tc",  0x003}, {"itt0",0x004}, {"itt1", 0x005},
             {"dtt0",0x006}, {"dtt1",0x007},
	     {"usp", 0x800}, {"vbr", 0x801}, {"caar", 0x802},
	     {"msp", 0x803}, {"isp", 0x804}, {"mmusr",0x805},
             {"urp", 0x806}, {"srp", 0x807}};

	val = fetch_arg (buffer, place, 12);
	for (regno = sizeof names / sizeof names[0] - 1; regno >= 0; regno--)
	  if (names[regno].value == val)
	    {
	      fprintf_filtered (stream, names[regno].name);
	      break;
	    }
	if (regno < 0)
	  fprintf_filtered (stream, "%d", val);
      }
      break;

    case 'Q':
      val = fetch_arg (buffer, place, 3);
      /* 0 means 8, except for the bkpt instruction... */
      if (val == 0 && d[1] != 's')
	val = 8;
      fprintf_filtered (stream, "#%d", val);
      break;

    case 'M':
      val = fetch_arg (buffer, place, 8);
      if (val & 0x80)
	val = val - 0x100;
      fprintf_filtered (stream, "#%d", val);
      break;

    case 'T':
      val = fetch_arg (buffer, place, 4);
      fprintf_filtered (stream, "#%d", val);
      break;

    case 'D':
      fprintf_filtered (stream, "%s", reg_names[fetch_arg (buffer, place, 3)]);
      break;

    case 'A':
      fprintf_filtered (stream, "%s",
			reg_names[fetch_arg (buffer, place, 3) + 010]);
      break;

    case 'R':
      fprintf_filtered (stream, "%s", reg_names[fetch_arg (buffer, place, 4)]);
      break;

    case 'r':
      fprintf_filtered (stream, "%s@", reg_names[fetch_arg (buffer, place, 4)]);
      break;

    case 'F':
      fprintf_filtered (stream, "fp%d", fetch_arg (buffer, place, 3));
      break;

    case 'O':
      val = fetch_arg (buffer, place, 6);
      if (val & 0x20)
	fprintf_filtered (stream, "%s", reg_names [val & 7]);
      else
	fprintf_filtered (stream, "%d", val);
      break;

    case '+':
      fprintf_filtered (stream, "%s@+",
			reg_names[fetch_arg (buffer, place, 3) + 8]);
      break;

    case '-':
      fprintf_filtered (stream, "%s@-",
	       reg_names[fetch_arg (buffer, place, 3) + 8]);
      break;

    case 'k':
      if (place == 'k')
	fprintf_filtered (stream, "{%s}", reg_names[fetch_arg (buffer, place, 3)]);
      else if (place == 'C')
	{
	  val = fetch_arg (buffer, place, 7);
	  if ( val > 63 )		/* This is a signed constant. */
	    val -= 128;
	  fprintf_filtered (stream, "{#%d}", val);
	}
      else
	error ("Invalid arg format in opcode table: \"%c%c\".",
	       *d, place);
      break;

    case '#':
    case '^':
      p1 = buffer + (*d == '#' ? 2 : 4);
      if (place == 's')
	val = fetch_arg (buffer, place, 4);
      else if (place == 'C')
	val = fetch_arg (buffer, place, 7);
      else if (place == '8')
	val = fetch_arg (buffer, place, 3);
      else if (place == '3')
	val = fetch_arg (buffer, place, 8);
      else if (place == 'b')
	val = NEXTBYTE (p1);
      else if (place == 'w')
	val = NEXTWORD (p1);
      else if (place == 'l')
	val = NEXTLONG (p1);
      else
	error ("Invalid arg format in opcode table: \"%c%c\".",
	       *d, place);
      fprintf_filtered (stream, "#%d", val);
      break;

    case 'B':
      if (place == 'b')
	val = NEXTBYTE (p);
      else if (place == 'B')
	val = NEXTBYTE (buffer);	/* from the opcode word */
      else if (place == 'w' || place == 'W')
	val = NEXTWORD (p);
      else if (place == 'l' || place == 'L')
	val = NEXTLONG (p);
      else if (place == 'g')
	{
	  val = NEXTBYTE (buffer);
	  if (val == 0)
	    val = NEXTWORD (p);
	  else if (val == -1)
	    val = NEXTLONG (p);
	}
      else if (place == 'c')
	{
	  if (buffer[1] & 0x40)		/* If bit six is one, long offset */
	    val = NEXTLONG (p);
	  else
	    val = NEXTWORD (p);
	}
      else
	error ("Invalid arg format in opcode table: \"%c%c\".",
	       *d, place);

      print_address (addr + val, stream);
      break;

    case 'd':
      val = NEXTWORD (p);
      fprintf_filtered (stream, "%s@(%d)",
			reg_names[fetch_arg (buffer, place, 3)], val);
      break;

    case 's':
      fprintf_filtered (stream, "%s",
			fpcr_names[fetch_arg (buffer, place, 3)]);
      break;

    case 'I':
      val = fetch_arg (buffer, 'd', 3);		  /* Get coprocessor ID... */
      if (val != 1)				/* Unusual coprocessor ID? */
	fprintf_filtered (stream, "(cpid=%d) ", val);
      if (place == 'i')
	p += 2;			     /* Skip coprocessor extended operands */
      break;

    case '*':
    case '~':
    case '%':
    case ';':
    case '@':
    case '!':
    case '$':
    case '?':
    case '/':
    case '&':
    case '`':

      if (place == 'd')
	{
	  val = fetch_arg (buffer, 'x', 6);
	  val = ((val & 7) << 3) + ((val >> 3) & 7);
	}
      else
	val = fetch_arg (buffer, 's', 6);

      /* Get register number assuming address register.  */
      regno = (val & 7) + 8;
      regname = reg_names[regno];
      switch (val >> 3)
	{
	case 0:
	  fprintf_filtered (stream, "%s", reg_names[val]);
	  break;

	case 1:
	  fprintf_filtered (stream, "%s", regname);
	  break;

	case 2:
	  fprintf_filtered (stream, "%s@", regname);
	  break;

	case 3:
	  fprintf_filtered (stream, "%s@+", regname);
	  break;

	case 4:
	  fprintf_filtered (stream, "%s@-", regname);
	  break;

	case 5:
	  val = NEXTWORD (p);
	  fprintf_filtered (stream, "%s@(%d)", regname, val);
	  break;

	case 6:
	  p = print_indexed (regno, p, addr, stream);
	  break;

	case 7:
	  switch (val & 7)
	    {
	    case 0:
	      val = NEXTWORD (p);
	      fprintf_filtered (stream, "@#");
	      print_address (val, stream);
	      break;

	    case 1:
	      val = NEXTLONG (p);
	      fprintf_filtered (stream, "@#");
	      print_address (val, stream);
	      break;

	    case 2:
	      val = NEXTWORD (p);
	      print_address (addr + val, stream);
	      break;

	    case 3:
	      p = print_indexed (-1, p, addr, stream);
	      break;

	    case 4:
	      flt_p = 1;	/* Assume it's a float... */
	      switch( place )
	      {
		case 'b':
		  val = NEXTBYTE (p);
		  flt_p = 0;
		  break;

		case 'w':
		  val = NEXTWORD (p);
		  flt_p = 0;
		  break;

		case 'l':
		  val = NEXTLONG (p);
		  flt_p = 0;
		  break;

		case 'f':
		  flval = NEXTSINGLE(p);
		  break;

		case 'F':
		  flval = NEXTDOUBLE(p);
		  break;

#ifdef HAVE_68881
		case 'x':
		  ieee_extended_to_double (&ext_format_68881, p, &flval);
		  p += 12;
		  break;
#endif

		case 'p':
		  p += 12;
		  flval = 0;	/* FIXME, handle packed decimal someday.  */
		  break;

		default:
		  error ("Invalid arg format in opcode table: \"%c%c\".",
		       *d, place);
	      }
	      if ( flt_p )	/* Print a float? */
		fprintf_filtered (stream, "#%g", flval);
	      else
		fprintf_filtered (stream, "#%d", val);
	      break;

	    default:
	      fprintf_filtered (stream, "<invalid address mode 0%o>", val);
	    }
	}
      break;

    case 'L':
    case 'l':
	if (place == 'w')
	  {
	    char doneany;
	    p1 = buffer + 2;
	    val = NEXTWORD (p1);
	    /* Move the pointer ahead if this point is farther ahead
	       than the last.  */
	    p = p1 > p ? p1 : p;
	    if (val == 0)
	      {
		fputs_filtered ("#0", stream);
		break;
	      }
	    if (*d == 'l')
	      {
		register int newval = 0;
		for (regno = 0; regno < 16; ++regno)
		  if (val & (0x8000 >> regno))
		    newval |= 1 << regno;
		val = newval;
	      }
	    val &= 0xffff;
	    doneany = 0;
	    for (regno = 0; regno < 16; ++regno)
	      if (val & (1 << regno))
		{
		  int first_regno;
		  if (doneany)
		    fputs_filtered ("/", stream);
		  doneany = 1;
		  fprintf_filtered (stream, "%s", reg_names[regno]);
		  first_regno = regno;
		  while (val & (1 << (regno + 1)))
		    ++regno;
		  if (regno > first_regno)
		    fprintf_filtered (stream, "-%s", reg_names[regno]);
		}
	  }
	else if (place == '3')
	  {
	    /* `fmovem' insn.  */
	    char doneany;
	    val = fetch_arg (buffer, place, 8);
	    if (val == 0)
	      {
		fputs_filtered ("#0", stream);
		break;
	      }
	    if (*d == 'l')
	      {
		register int newval = 0;
		for (regno = 0; regno < 8; ++regno)
		  if (val & (0x80 >> regno))
		    newval |= 1 << regno;
		val = newval;
	      }
	    val &= 0xff;
	    doneany = 0;
	    for (regno = 0; regno < 8; ++regno)
	      if (val & (1 << regno))
		{
		  int first_regno;
		  if (doneany)
		    fputs_filtered ("/", stream);
		  doneany = 1;
		  fprintf_filtered (stream, "fp%d", regno);
		  first_regno = regno;
		  while (val & (1 << (regno + 1)))
		    ++regno;
		  if (regno > first_regno)
		    fprintf_filtered (stream, "-fp%d", regno);
		}
	  }
	else
	  goto de_fault;
      break;

    default:  de_fault:
      error ("Invalid arg format in opcode table: \"%c\".", *d);
    }

  return (unsigned char *) p;
}

/* Fetch BITS bits from a position in the instruction specified by CODE.
   CODE is a "place to put an argument", or 'x' for a destination
   that is a general address (mode and register).
   BUFFER contains the instruction.  */

static int
fetch_arg (buffer, code, bits)
     unsigned char *buffer;
     int code;
     int bits;
{
  register int val;
  switch (code)
    {
    case 's':
      val = buffer[1];
      break;

    case 'd':			/* Destination, for register or quick.  */
      val = (buffer[0] << 8) + buffer[1];
      val >>= 9;
      break;

    case 'x':			/* Destination, for general arg */
      val = (buffer[0] << 8) + buffer[1];
      val >>= 6;
      break;

    case 'k':
      val = (buffer[3] >> 4);
      break;

    case 'C':
      val = buffer[3];
      break;

    case '1':
      val = (buffer[2] << 8) + buffer[3];
      val >>= 12;
      break;

    case '2':
      val = (buffer[2] << 8) + buffer[3];
      val >>= 6;
      break;

    case '3':
    case 'j':
      val = (buffer[2] << 8) + buffer[3];
      break;

    case '4':
      val = (buffer[4] << 8) + buffer[5];
      val >>= 12;
      break;

    case '5':
      val = (buffer[4] << 8) + buffer[5];
      val >>= 6;
      break;

    case '6':
      val = (buffer[4] << 8) + buffer[5];
      break;

    case '7':
      val = (buffer[2] << 8) + buffer[3];
      val >>= 7;
      break;
      
    case '8':
      val = (buffer[2] << 8) + buffer[3];
      val >>= 10;
      break;

    case 'e':
      val = (buffer[1] >> 6);
      break;

    default:
      abort ();
    }

  switch (bits)
    {
    case 2:
      return val & 3;
    case 3:
      return val & 7;
    case 4:
      return val & 017;
    case 5:
      return val & 037;
    case 6:
      return val & 077;
    case 7:
      return val & 0177;
    case 8:
      return val & 0377;
    case 12:
      return val & 07777;
    default:
      abort ();
    }
}

/* Print an indexed argument.  The base register is BASEREG (-1 for pc).
   P points to extension word, in buffer.
   ADDR is the nominal core address of that extension word.  */

static unsigned char *
print_indexed (basereg, p, addr, stream)
     int basereg;
     unsigned char *p;
     CORE_ADDR addr;
     FILE *stream;
{
  register int word;
  static char *scales[] = {"", "*2", "*4", "*8"};
  register int base_disp;
  register int outer_disp;
  char buf[40];

  word = NEXTWORD (p);

  /* Generate the text for the index register.
     Where this will be output is not yet determined.  */
  sprintf (buf, "[%s.%c%s]",
	   reg_names[(word >> 12) & 0xf],
	   (word & 0x800) ? 'l' : 'w',
	   scales[(word >> 9) & 3]);

  /* Handle the 68000 style of indexing.  */

  if ((word & 0x100) == 0)
    {
      print_base (basereg,
		  ((word & 0x80) ? word | 0xff00 : word & 0xff)
		  + ((basereg == -1) ? addr : 0),
		  stream);
      fputs_filtered (buf, stream);
      return p;
    }

  /* Handle the generalized kind.  */
  /* First, compute the displacement to add to the base register.  */

  if (word & 0200)
    basereg = -2;
  if (word & 0100)
    buf[0] = 0;
  base_disp = 0;
  switch ((word >> 4) & 3)
    {
    case 2:
      base_disp = NEXTWORD (p);
      break;
    case 3:
      base_disp = NEXTLONG (p);
    }
  if (basereg == -1)
    base_disp += addr;

  /* Handle single-level case (not indirect) */

  if ((word & 7) == 0)
    {
      print_base (basereg, base_disp, stream);
      fputs_filtered (buf, stream);
      return p;
    }

  /* Two level.  Compute displacement to add after indirection.  */

  outer_disp = 0;
  switch (word & 3)
    {
    case 2:
      outer_disp = NEXTWORD (p);
      break;
    case 3:
      outer_disp = NEXTLONG (p);
    }

  fprintf_filtered (stream, "%d(", outer_disp);
  print_base (basereg, base_disp, stream);

  /* If postindexed, print the closeparen before the index.  */
  if (word & 4)
    fprintf_filtered (stream, ")%s", buf);
  /* If preindexed, print the closeparen after the index.  */
  else
    fprintf_filtered (stream, "%s)", buf);

  return p;
}

/* Print a base register REGNO and displacement DISP, on STREAM.
   REGNO = -1 for pc, -2 for none (suppressed).  */

static void
print_base (regno, disp, stream)
     int regno;
     int disp;
     FILE *stream;
{
  if (regno == -2)
    fprintf_filtered (stream, "%d", disp);
  else if (regno == -1)
    fprintf_filtered (stream, "0x%x", disp);
  else
    fprintf_filtered (stream, "%d(%s)", disp, reg_names[regno]);
}
