/* Medium-level subroutines: convert bit-field store and extract
   and shifts, multiplies and divides to rtl instructions.
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "insn-config.h"
#include "expr.h"
#include "recog.h"

static rtx extract_split_bit_field ();
static rtx extract_fixed_bit_field ();
static void store_split_bit_field ();
static void store_fixed_bit_field ();

/* Return an rtx representing minus the value of X.
   MODE is the intended mode of the result,
   useful if X is a CONST_INT.  */

rtx
negate_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  if (GET_CODE (x) == CONST_INT)
    {
      int val = - INTVAL (x);
      if (GET_MODE_BITSIZE (mode) < HOST_BITS_PER_INT)
	{
	  /* Sign extend the value from the bits that are significant.  */
	  if (val & (1 << (GET_MODE_BITSIZE (mode) - 1)))
	    val |= (-1) << GET_MODE_BITSIZE (mode);
	  else
	    val &= (1 << GET_MODE_BITSIZE (mode)) - 1;
	}
      return gen_rtx (CONST_INT, VOIDmode, val);
    }
  else
    return expand_unop (GET_MODE (x), neg_optab, x, 0, 0);
}

/* Generate code to store value from rtx VALUE
   into a bit-field within structure STR_RTX
   containing BITSIZE bits starting at bit BITNUM.
   FIELDMODE is the machine-mode of the FIELD_DECL node for this field.
   ALIGN is the alignment that STR_RTX is known to have, measured in bytes.
   TOTAL_SIZE is the size of the structure in bytes, or -1 if unknown.  */

rtx
store_bit_field (str_rtx, bitsize, bitnum, fieldmode, value, align, total_size)
     rtx str_rtx;
     register int bitsize;
     int bitnum;
     enum machine_mode fieldmode;
     rtx value;
     int align;
     int total_size;
{
  int unit = (GET_CODE (str_rtx) == MEM) ? BITS_PER_UNIT : BITS_PER_WORD;
  register int offset = bitnum / unit;
  register int bitpos = bitnum % unit;
  register rtx op0 = str_rtx;
  rtx value1;

  /* At this point, BITPOS counts within UNIT for a memref.
     For a register or a subreg, it actually counts within the width
     of the mode of OP0.  However, BITNUM never exceeds that width,
     so the % operation above never really does anything.

     We will adjust BITPOS later to count properly within UNIT
     in the case of a register.  */

  /* Discount the part of the structure before the desired byte.
     We need to know how many bytes are safe to reference after it.  */
  if (total_size >= 0)
    total_size -= (bitpos / BIGGEST_ALIGNMENT
		   * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  while (GET_CODE (op0) == SUBREG)
    {
#ifdef BYTES_BIG_ENDIAN
      /* Keep BITPOS counting within the size of op0.  */
      bitpos += (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op0)))
		 - GET_MODE_BITSIZE (GET_MODE (op0)));
#endif
      offset += SUBREG_WORD (op0);
      op0 = SUBREG_REG (op0);
    }

  value = protect_from_queue (value, 0);

  if (flag_force_mem)
    value = force_not_mem (value);

  if (GET_MODE_SIZE (fieldmode) >= UNITS_PER_WORD
      && GET_MODE_BITSIZE (fieldmode) == bitsize
      && bitpos % BITS_PER_WORD == 0
      && GET_CODE (op0) == REG)
    {
      /* Storing in a full-word or multi-word field in a register
	 can be done with just SUBREG.  */
      if (GET_MODE (op0) != fieldmode)
	op0 = gen_rtx (SUBREG, fieldmode, op0, offset);
      emit_move_insn (op0, value);
      return value;
    }

#ifdef BYTES_BIG_ENDIAN
  /* If OP0 is a register, BITPOS must count within UNIT, which should be SI.
     But as we have it, it counts within whatever size OP0 now has.
     These are not the same, so convert if big-endian.  */
  if (GET_CODE (op0) != MEM && unit > GET_MODE_BITSIZE (GET_MODE (op0)))
    {
      bitpos += unit - GET_MODE_BITSIZE (GET_MODE (op0));
      /* Change the mode now so we don't adjust BITPOS again.  */
      if (GET_CODE (op0) == SUBREG)
	PUT_MODE (op0, SImode);
      else
	op0 = gen_rtx (SUBREG, SImode, op0, 0);
    }
#endif

  /* Storing an lsb-aligned field in a register
     can be done with a movestrict instruction.  */

  if (GET_CODE (op0) != MEM
#ifdef BYTES_BIG_ENDIAN
      && bitpos + bitsize == unit
#else
      && bitpos == 0
#endif
      && (GET_MODE (op0) == fieldmode
	  || (movstrict_optab->handlers[(int) fieldmode].insn_code
	      != CODE_FOR_nothing)))
    {
      /* Get appropriate low part of the value being stored.  */
      if (GET_CODE (value) == CONST_INT || GET_CODE (value) == REG)
	value = gen_lowpart (fieldmode, value);
      else if (!(GET_CODE (value) == SYMBOL_REF
		 || GET_CODE (value) == LABEL_REF
		 || GET_CODE (value) == CONST))
	value = convert_to_mode (fieldmode, value, 0);

      if (GET_MODE (op0) == fieldmode)
	emit_move_insn (op0, value);
      else
	{
	  if (GET_CODE (op0) == SUBREG)
	    PUT_MODE (op0, fieldmode);
	  else
	    op0 = gen_rtx (SUBREG, fieldmode, op0, offset);
	  emit_insn (GEN_FCN (movstrict_optab->handlers[(int) fieldmode].insn_code)
		     (op0, value));
	}

      return value;
    }

  /* Handle fields bigger than a word.  */

  if (bitsize > BITS_PER_WORD)
    {
      int low_size = BITS_PER_WORD;
      int low_pos = bitpos + offset * unit;
      int high_size = bitsize - low_size;
      int high_pos;
#ifdef BYTES_BIG_ENDIAN
      high_pos = low_pos;
      low_pos += high_size;
#else
      high_pos = low_pos + low_size;
#endif

      value = force_reg (GET_MODE (value), value); 
      store_bit_field (op0, low_size, low_pos, SImode,
		       gen_lowpart (SImode, value), align, total_size);
      store_bit_field (op0, high_size, high_pos, SImode,
		       gen_highpart (SImode, value), align, total_size);
      return value;
    }

  /* From here on we can assume that the field to be stored in is an integer,
     since it is shorter than a word.  */

  /* OFFSET is the number of words or bytes (UNIT says which)
     from STR_RTX to the first word or byte containing part of the field.  */

  if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
    {
      /* If not in memory, merge in the offset now.  */
      if (offset != 0
	  || GET_MODE_SIZE (GET_MODE (op0)) > GET_MODE_SIZE (SImode))
	{
	  if (GET_CODE (op0) == SUBREG)
	    SUBREG_WORD (op0) += offset;
	  else
	    op0 = gen_rtx (SUBREG, SImode, op0, offset);
	}
      offset = 0;
    }
  else
    {
      op0 = protect_from_queue (op0, 1);
    }

  /* Now OFFSET is nonzero only if OP0 is memory
     and is therefore always measured in bytes.  */

#ifdef HAVE_insv
  if (HAVE_insv
      && !(bitsize == 1 && GET_CODE (value) == CONST_INT))
    {
      int xbitpos = bitpos;
      rtx xop0 = op0;
      rtx last = get_last_insn ();
      rtx pat;

      /* If this machine's insv can only insert into a register,
	 copy OP0 into a register and save it back later.  */
      if (GET_CODE (op0) == MEM
	  && ! (*insn_operand_predicate[(int) CODE_FOR_insv][0]) (op0, VOIDmode))
	{
	  rtx tempreg;
	  enum machine_mode trymode, bestmode = VOIDmode, insn_mode;
	  /* Don't use a mode bigger than the one of the value to be stored.
	     That mode must be okay, since a bit field can be that big.  */
	  int maxsize
	    = GET_MODE_SIZE (insn_operand_mode[(int) CODE_FOR_insv][3]);
	  /* This used to use the mode desired for operand 0,
	     but that is normally QImode on most machines,
	     and QImode won't work for fields that cross byte
	     boundaries.  */

	  /* Also don't use a mode bigger than the structure.  */
	  if (total_size >= 0 && maxsize > total_size)
	    maxsize = total_size;

	  /* Find biggest machine mode we can safely use
	     to fetch from this structure.
	     But don't use a bigger mode than the insn wants.  */
	  for (trymode = QImode;
	       trymode && GET_MODE_SIZE (trymode) <= maxsize;
	       trymode = GET_MODE_WIDER_MODE (trymode))
	    if (GET_MODE_SIZE (trymode) <= align
		|| align == BIGGEST_ALIGNMENT / BITS_PER_UNIT)
	      bestmode = trymode;
	  if (! bestmode)
	    abort ();
	  /* Adjust address to point to the containing unit of that mode.  */
	  unit = GET_MODE_BITSIZE (bestmode);
	  /* Compute offset as multiple of this unit, counting in bytes.  */
	  offset = (bitnum / unit) * GET_MODE_SIZE (bestmode);
	  bitpos = bitnum % unit;
	  op0 = change_address (op0, bestmode, 
				plus_constant (XEXP (op0, 0), offset));

	  /* Fetch that unit, store the bitfield in it, then store the unit.  */
	  tempreg = copy_to_reg (op0);
	  /* To actually store in TEMPREG,
	     look at it in the mode this insn calls for.
	     (Probably SImode.)  */
	  insn_mode = SImode;
#ifdef BYTES_BIG_ENDIAN
	  if (GET_MODE_BITSIZE (insn_mode) > unit)
	    bitpos += GET_MODE_BITSIZE (insn_mode) - unit;
#endif
	  store_bit_field (gen_rtx (SUBREG, insn_mode, tempreg, 0),
			   bitsize, bitpos, fieldmode, value,
			   align, total_size);
	  emit_move_insn (op0, tempreg);
	  return value;
	}

      /* Add OFFSET into OP0's address.  */
      if (GET_CODE (xop0) == MEM)
	xop0 = change_address (xop0, QImode,
			       plus_constant (XEXP (xop0, 0), offset));

      /* If xop0 is a register, we need it in SImode
	 to make it acceptable to the format of insv.  */
      if (GET_CODE (xop0) == SUBREG)
	PUT_MODE (xop0, SImode);
      if (GET_CODE (xop0) == REG && GET_MODE (xop0) != SImode)
	{
#ifdef BYTES_BIG_ENDIAN
	  xbitpos += (GET_MODE_BITSIZE (SImode)
		      - GET_MODE_BITSIZE (GET_MODE (xop0)));
#endif
	  xop0 = gen_rtx (SUBREG, SImode, xop0, 0);
	}

      /* Convert VALUE to SImode (which insv insn wants) in VALUE1.  */
      value1 = value;
      if (GET_MODE (value) != SImode)
	{
	  if (GET_MODE_BITSIZE (GET_MODE (value)) >= bitsize)
	    {
	      /* Optimization: Don't bother really extending VALUE
		 if it has all the bits we will actually use.  */

	      /* Avoid making subreg of a subreg, or of a mem.  */
	      if (GET_CODE (value1) != REG)
		value1 = copy_to_reg (value1);
	      value1 = gen_rtx (SUBREG, SImode, value1, 0);
	    }
	  else if (!CONSTANT_P (value))
	    /* Parse phase is supposed to make VALUE's data type
	       match that of the component reference, which is a type
	       at least as wide as the field; so VALUE should have
	       a mode that corresponds to that type.  */
	    abort ();
	}

      /* If this machine's insv insists on a register,
	 get VALUE1 into a register.  */
      if (! (*insn_operand_predicate[(int) CODE_FOR_insv][3]) (value1, SImode))
	value1 = force_reg (SImode, value1);

      /* On big-endian machines, we count bits from the most significant.
	 If the bit field insn does not, we must invert.  */

#if defined (BITS_BIG_ENDIAN) != defined (BYTES_BIG_ENDIAN)
      xbitpos = unit - 1 - xbitpos;
#endif

      pat = gen_insv (xop0,
		      gen_rtx (CONST_INT, VOIDmode, bitsize),
		      gen_rtx (CONST_INT, VOIDmode, xbitpos),
		      value1);
      if (pat)
	emit_insn (pat);
      else
        {
	  delete_insns_since (last);
	  store_fixed_bit_field (op0, offset, bitsize, bitpos, value, align);
	}
    }
  else
#endif
    /* Insv is not available; store using shifts and boolean ops.  */
    store_fixed_bit_field (op0, offset, bitsize, bitpos, value, align);
  return value;
}

/* Use shifts and boolean operations to store VALUE
   into a bit field of width BITSIZE
   in a memory location specified by OP0 except offset by OFFSET bytes.
     (OFFSET must be 0 if OP0 is a register.)
   The field starts at position BITPOS within the byte.
    (If OP0 is a register, it may be SImode or a narrower mode,
     but BITPOS still counts within a full word,
     which is significant on bigendian machines.)
   STRUCT_ALIGN is the alignment the structure is known to have (in bytes).

   Note that protect_from_queue has already been done on OP0 and VALUE.  */

static void
store_fixed_bit_field (op0, offset, bitsize, bitpos, value, struct_align)
     register rtx op0;
     register int offset, bitsize, bitpos;
     register rtx value;
     int struct_align;
{
  register enum machine_mode mode;
  int total_bits = BITS_PER_WORD;
  rtx subtarget;
  int all_zero = 0;
  int all_one = 0;

  /* Add OFFSET to OP0's address (if it is in memory)
     and if a single byte contains the whole bit field
     change OP0 to a byte.  */

  /* There is a case not handled here:
     a structure with a known alignment of just a halfword
     and a field split across two aligned halfwords within the structure.
     Or likewise a structure with a known alignment of just a byte
     and a field split across two bytes.
     Such cases are not supposed to be able to occur.  */

  if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
    {
      if (offset != 0)
	abort ();
      /* Special treatment for a bit field split across two registers.  */
      if (bitsize + bitpos > BITS_PER_WORD)
	{
	  store_split_bit_field (op0, bitsize, bitpos, value, BITS_PER_WORD);
	  return;
	}
    }
  else if (bitsize + bitpos <= BITS_PER_UNIT
	   && (! SLOW_BYTE_ACCESS
	       || (struct_align == 1
		   && BIGGEST_ALIGNMENT > 1)))
    {
      /* It fits in one byte, and either bytes are fast
	 or the alignment won't let us use anything bigger.  */
      total_bits = BITS_PER_UNIT;
      op0 = change_address (op0, QImode, 
			    plus_constant (XEXP (op0, 0), offset));
    }
  else if ((bitsize + bitpos + (offset % GET_MODE_SIZE (HImode)) * BITS_PER_UNIT
	    <= GET_MODE_BITSIZE (HImode))
	   /* If halfwords are fast, use them whenever valid.  */
	   && (! SLOW_BYTE_ACCESS
	       /* Use halfwords if larger is invalid due to alignment.  */
	       || (struct_align == GET_MODE_SIZE (HImode)
		   && BIGGEST_ALIGNMENT > GET_MODE_SIZE (HImode))))
    {
      /* It fits in an aligned halfword within the structure,
	 and either halfwords are fast
	 or the alignment won't let us use anything bigger.  */
      total_bits = GET_MODE_BITSIZE (HImode);

      /* Get ref to halfword containing the field.  */
      bitpos += (offset % (total_bits / BITS_PER_UNIT)) * BITS_PER_UNIT;
      offset -= (offset % (total_bits / BITS_PER_UNIT));
      op0 = change_address (op0, HImode, 
			    plus_constant (XEXP (op0, 0), offset));
    }
  else
    {
      /* Get ref to an aligned word containing the field.  */
      /* Adjust BITPOS to be position within a word,
	 and OFFSET to be the offset of that word.
	 Then alter OP0 to refer to that word.  */
      bitpos += (offset % (BITS_PER_WORD / BITS_PER_UNIT)) * BITS_PER_UNIT;
      offset -= (offset % (BITS_PER_WORD / BITS_PER_UNIT));
      op0 = change_address (op0, SImode,
			    plus_constant (XEXP (op0, 0), offset));

      /* Special treatment for a bit field split across two aligned words.  */
      if (bitsize + bitpos > BITS_PER_WORD)
	{
	  store_split_bit_field (op0, bitsize, bitpos, value, struct_align);
	  return;
	}
    }

  mode = GET_MODE (op0);

  /* Now MODE is either QImode, HImode or SImode for a MEM as OP0,
     or is SImode for a REG as OP0.  TOTAL_BITS corresponds.
     The bit field is contained entirely within OP0.
     BITPOS is the starting bit number within OP0.
     (OP0's mode may actually be narrower than MODE.)  */

#ifdef BYTES_BIG_ENDIAN
  /* BITPOS is the distance between our msb
     and that of the containing datum.
     Convert it to the distance from the lsb.  */

  bitpos = total_bits - bitsize - bitpos;
#endif
  /* Now BITPOS is always the distance between our lsb
     and that of OP0.  */

  /* Shift VALUE left by BITPOS bits.  If VALUE is not constant,
     we must first convert its mode to MODE.  */

  if (GET_CODE (value) == CONST_INT)
    {
      register int v = INTVAL (value);

      if (bitsize < HOST_BITS_PER_INT)
	v &= (1 << bitsize) - 1;

      if (v == 0)
	all_zero = 1;
      else if (bitsize < HOST_BITS_PER_INT && v == (1 << bitsize) - 1)
	all_one = 1;

      value = gen_rtx (CONST_INT, VOIDmode, v << bitpos);
    }
  else
    {
      int must_and = (GET_MODE_BITSIZE (GET_MODE (value)) != bitsize);

      if (GET_MODE (value) != mode)
	{
	  if ((GET_CODE (value) == REG || GET_CODE (value) == SUBREG)
	      && GET_MODE_SIZE (mode) < GET_MODE_SIZE (GET_MODE (value)))
	    value = gen_lowpart (mode, value);
	  else
	    value = convert_to_mode (mode, value, 1);
	}

      if (must_and && bitsize < HOST_BITS_PER_INT)
	value = expand_bit_and (mode, value,
				gen_rtx (CONST_INT, VOIDmode,
					 (1 << bitsize) - 1),
				0);
      if (bitpos > 0)
	value = expand_shift (LSHIFT_EXPR, mode, value,
			      build_int_2 (bitpos, 0), 0, 1);
    }

  /* Now clear the chosen bits in OP0,
     except that if VALUE is -1 we need not bother.  */

  subtarget = op0;

  if (! all_one)
    subtarget = expand_bit_and (mode, op0,
				gen_rtx (CONST_INT, VOIDmode, 
					 (~ (((unsigned) ~0
					      >> (HOST_BITS_PER_INT - bitsize))
					     << bitpos))
					 & ((GET_MODE_BITSIZE (mode)
					     == HOST_BITS_PER_INT)
					    ? -1
					    : ((1 << GET_MODE_BITSIZE (mode)) - 1))),
				subtarget);

  /* Now logical-or VALUE into OP0, unless it is zero.  */

  if (! all_zero)
    subtarget = expand_binop (mode, ior_optab, subtarget, value,
			      op0, 1, OPTAB_LIB_WIDEN);
  if (op0 != subtarget)
    emit_move_insn (op0, subtarget);
}

/* Store a bit field that is split across two words.

   OP0 is the REG, SUBREG or MEM rtx for the first of the two words.
   BITSIZE is the field width; BITPOS the position of its first bit
   (within the word).
   VALUE is the value to store.  */

static void
store_split_bit_field (op0, bitsize, bitpos, value, align)
     rtx op0;
     int bitsize, bitpos;
     rtx value;
     int align;
{
  /* BITSIZE_1 is size of the part in the first word.  */
  int bitsize_1 = BITS_PER_WORD - bitpos;
  /* BITSIZE_2 is size of the rest (in the following word).  */
  int bitsize_2 = bitsize - bitsize_1;
  rtx part1, part2;

  /* Alignment of VALUE, after conversion.  */
  int valalign = GET_MODE_SIZE (SImode);

  if (GET_MODE (value) != VOIDmode)
    value = convert_to_mode (SImode, value, 1);
  if (CONSTANT_P (value) && GET_CODE (value) != CONST_INT)
    value = copy_to_reg (value);

  /* Split the value into two parts:
     PART1 gets that which goes in the first word; PART2 the other.  */
#ifdef BYTES_BIG_ENDIAN
  /* PART1 gets the more significant part.  */
  if (GET_CODE (value) == CONST_INT)
    {
      part1 = gen_rtx (CONST_INT, VOIDmode,
		       (unsigned) (INTVAL (value)) >> bitsize_2);
      part2 = gen_rtx (CONST_INT, VOIDmode,
		       (unsigned) (INTVAL (value)) & ((1 << bitsize_2) - 1));
    }
  else
    {
      part1 = extract_fixed_bit_field (SImode, value, 0, bitsize_1,
				       BITS_PER_WORD - bitsize, 0, 1, valalign);
      part2 = extract_fixed_bit_field (SImode, value, 0, bitsize_2,
				       BITS_PER_WORD - bitsize_2, 0, 1, valalign);
    }
#else
  /* PART1 gets the less significant part.  */
  if (GET_CODE (value) == CONST_INT)
    {
      part1 = gen_rtx (CONST_INT, VOIDmode,
		       (unsigned) (INTVAL (value)) & ((1 << bitsize_1) - 1));
      part2 = gen_rtx (CONST_INT, VOIDmode,
		       (unsigned) (INTVAL (value)) >> bitsize_1);
    }
  else
    {
      part1 = extract_fixed_bit_field (SImode, value, 0, bitsize_1, 0,
				       0, 1, valalign);
      part2 = extract_fixed_bit_field (SImode, value, 0, bitsize_2,
				       bitsize_1, 0, 1, valalign);
    }
#endif

  /* Store PART1 into the first word.  */
  store_fixed_bit_field (op0, 0, bitsize_1, bitpos, part1, align);

  /* Offset op0 to get to the following word.  */
  if (GET_CODE (op0) == MEM)
    op0 = change_address (op0, SImode,
			  plus_constant (XEXP (op0, 0), UNITS_PER_WORD));
  else if (GET_CODE (op0) == REG)
    op0 = gen_rtx (SUBREG, SImode, op0, 1);
  else if (GET_CODE (op0) == SUBREG)
    op0 = gen_rtx (SUBREG, SImode, SUBREG_REG (op0), SUBREG_WORD (op0) + 1);

  /* Store PART2 into the second word.  */
  store_fixed_bit_field (op0, 0, bitsize_2, 0, part2, align);
}

/* Generate code to extract a byte-field from STR_RTX
   containing BITSIZE bits, starting at BITNUM,
   and put it in TARGET if possible (if TARGET is nonzero).
   Regardless of TARGET, we return the rtx for where the value is placed.
   It may be a QUEUED.

   STR_RTX is the structure containing the byte (a REG or MEM).
   UNSIGNEDP is nonzero if this is an unsigned bit field.
   MODE is the natural mode of the field value once extracted.
   TMODE is the mode the caller would like the value to have;
   but the value may be returned with type MODE instead.

   ALIGN is the alignment that STR_RTX is known to have, measured in bytes.
   TOTAL_SIZE is the total size in bytes of the structure, if known.
   Otherwise it is -1.

   If a TARGET is specified and we can store in it at no extra cost,
   we do so, and return TARGET.
   Otherwise, we return a REG of mode TMODE or MODE, with TMODE preferred
   if they are equally easy.  */

rtx
extract_bit_field (str_rtx, bitsize, bitnum, unsignedp,
		   target, mode, tmode, align, total_size)
     rtx str_rtx;
     register int bitsize;
     int bitnum;
     int unsignedp;
     rtx target;
     enum machine_mode mode, tmode;
     int align;
     int total_size;
{
  int unit = (GET_CODE (str_rtx) == MEM) ? BITS_PER_UNIT : BITS_PER_WORD;
  register int offset = bitnum / unit;
  register int bitpos = bitnum % unit;
  register rtx op0 = str_rtx;
  rtx spec_target = target;
  rtx bitsize_rtx, bitpos_rtx;
  rtx spec_target_subreg = 0;

  /* Discount the part of the structure before the desired byte.
     We need to know how many bytes are safe to reference after it.  */
  if (total_size >= 0)
    total_size -= (bitpos / BIGGEST_ALIGNMENT
		   * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  if (tmode == VOIDmode)
    tmode = mode;

  while (GET_CODE (op0) == SUBREG)
    {
#ifdef BYTES_BIG_ENDIAN
      /* Keep BITPOS counting within the size of op0.  */
      bitpos += (GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op0)))
		 - GET_MODE_BITSIZE (GET_MODE (op0)));
#endif
      offset += SUBREG_WORD (op0);
      op0 = SUBREG_REG (op0);
    }
  
#ifdef BYTES_BIG_ENDIAN
  /* If OP0 is a register, BITPOS must count within a word.
     But as we have it, it counts within whatever size OP0 now has.
     On a bigendian machine, these are not the same, so convert.  */
  if (GET_CODE (op0) != MEM && unit > GET_MODE_BITSIZE (GET_MODE (op0)))
    {
      bitpos += unit - GET_MODE_BITSIZE (GET_MODE (op0));
      /* Change the mode now so we don't adjust BITPOS again.  */
      if (GET_CODE (op0) == SUBREG)
	PUT_MODE (op0, SImode);
      else
	op0 = gen_rtx (SUBREG, SImode, op0, 0);
    }
#endif

  /* Extracting a full-word or multi-word value
     from a structure in a register.
     This can be done with just SUBREG.
     So too extracting a subword value in
     the least significant part of the register.  */

  if (GET_CODE (op0) == REG
      && ((bitsize >= BITS_PER_WORD && bitsize == GET_MODE_BITSIZE (mode)
	   && bitpos % BITS_PER_WORD == 0)
	  || ((bitsize == GET_MODE_BITSIZE (mode)
	       || bitsize == GET_MODE_BITSIZE (QImode)
	       || bitsize == GET_MODE_BITSIZE (HImode))
#ifdef BYTES_BIG_ENDIAN
	      && bitpos + bitsize == BITS_PER_WORD
#else
	      && bitpos == 0
#endif
	      )))
    {
      enum machine_mode mode1 = mode;

      if (bitsize == GET_MODE_BITSIZE (QImode))
	mode1 = QImode;
      if (bitsize == GET_MODE_BITSIZE (HImode))
	mode1 = HImode;

      if (mode1 != GET_MODE (op0))
	{
	  if (GET_CODE (op0) == SUBREG)
	    PUT_MODE (op0, mode1);
	  else
	    op0 = gen_rtx (SUBREG, mode1, op0, offset);
	}

      if (mode1 != mode)
	return convert_to_mode (tmode, op0, unsignedp);
      return op0;
    }

  /* Handle fields bigger than a word.  */
  
  if (bitsize > BITS_PER_WORD)
    {
      int low_size = BITS_PER_WORD;
      int low_pos = bitpos + offset * unit;
      rtx target_low_part, low_part;
      int high_size = bitsize - low_size;
      int high_pos;
      rtx target_high_part, high_part;
#ifdef BYTES_BIG_ENDIAN
      high_pos = low_pos;
      low_pos += high_size;
#else
      high_pos = low_pos + low_size;
#endif

      if (target == 0 || GET_CODE (target) != REG)
	target = gen_reg_rtx (mode);

      /* Extract the low part of the bitfield, and make sure
	 to store it in the low part of TARGET.  */
      target_low_part = gen_lowpart (SImode, target);
      low_part = extract_bit_field (op0, low_size, low_pos, 1,
				    target_low_part, SImode, SImode,
				    align, total_size);
      if (low_part != target_low_part)
	emit_move_insn (target_low_part, low_part);

      /* Likewise for the high part.  */
      target_high_part = gen_highpart (SImode, target);
      high_part = extract_bit_field (op0, high_size, high_pos, unsignedp,
				     target_high_part, SImode, SImode,
				     align, total_size);
      if (high_part != target_high_part)
	emit_move_insn (target_high_part, high_part);

      return target;
    }

  /* From here on we know the desired field is smaller than a word
     so we can assume it is an integer.  So we can safely extract it as one
     size of integer, if necessary, and then truncate or extend
     to the size that is wanted.  */

  /* OFFSET is the number of words or bytes (UNIT says which)
     from STR_RTX to the first word or byte containing part of the field.  */

  if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
    {
      /* If not in memory, merge in the offset now.  */
      if (offset != 0
	  || GET_MODE_SIZE (GET_MODE (op0)) > GET_MODE_SIZE (SImode))
	{
	  if (GET_CODE (op0) == SUBREG)
	    SUBREG_WORD (op0) += offset;
	  else
	    op0 = gen_rtx (SUBREG, SImode, op0, offset);
	}
      offset = 0;
    }
  else
    {
      op0 = protect_from_queue (str_rtx, 1);
    }

  /* Now OFFSET is nonzero only for memory operands.  */

  if (unsignedp)
    {
#ifdef HAVE_extzv
      if (HAVE_extzv)
	{
	  int xbitpos = bitpos, xoffset = offset;
	  rtx last = get_last_insn();
	  rtx xop0 = op0;
	  rtx xtarget = target;
	  rtx xspec_target = spec_target;
	  rtx xspec_target_subreg = spec_target_subreg;
	  rtx pat;

	  if (GET_CODE (xop0) == MEM)
	    {
	      /* Is the memory operand acceptable?  */
	      if (! ((*insn_operand_predicate[(int) CODE_FOR_extzv][1])
		     (xop0, GET_MODE (xop0))))
		{
		  /* No, load into a reg and extract from there.  */
		  enum machine_mode bestmode = VOIDmode, trymode;
		  /* Don't use a mode bigger than the one of the value
		     to be fetched.  That mode must be okay,
		     since a bit field can be that big.  */
		  int maxsize
		    = GET_MODE_SIZE (insn_operand_mode[(int) CODE_FOR_extzv][0]);
		  /* This used to use the mode desired for operand 1,
		     but that is normally QImode on most machines,
		     and QImode won't work for fields that cross byte
		     boundaries.  */

		  /* Also don't use a mode bigger than the structure.  */
		  if (total_size >= 0 && maxsize > total_size)
		    maxsize = total_size;

		  /* Find biggest machine mode we can safely use
		     to fetch from this structure.
		     But don't use a bigger mode than the insn wants.  */
		  for (trymode = QImode;
		       trymode && GET_MODE_SIZE (trymode) <= maxsize;
		       trymode = GET_MODE_WIDER_MODE (trymode))
		    if (GET_MODE_SIZE (trymode) <= align
			|| align == BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		      bestmode = trymode;
		  if (! bestmode)
		    abort ();
		  unit = GET_MODE_BITSIZE (bestmode);

		  /* Compute offset as multiple of this unit,
		     counting in bytes.  */
		  xoffset = (bitnum / unit) * GET_MODE_SIZE (bestmode);
		  xbitpos = bitnum % unit;
		  xop0 = change_address (xop0, bestmode,
					 plus_constant (XEXP (xop0, 0),
							xoffset));
		  /* Fetch it to a register in that size.  */
		  xop0 = force_reg (bestmode, xop0);

		  /* Now ref the register in the mode extzv wants.  */
		  /* We used to use the mode from operand 1 in the md,
		     but that is often QImode because that's needed for MEM.
		     Here we need SImode instead.  */
		  if (bestmode != SImode)
		    xop0 = gen_rtx (SUBREG, SImode, xop0, 0);
#ifdef BYTES_BIG_ENDIAN
		  if (GET_MODE_BITSIZE (GET_MODE (xop0)) > unit)
		    xbitpos += GET_MODE_BITSIZE (GET_MODE (xop0)) - unit;
#endif
		}
	      else
		/* Get ref to first byte containing part of the field.  */
		xop0 = change_address (xop0, QImode,
				       plus_constant (XEXP (xop0, 0), xoffset));
	    }

	  /* If op0 is a register, we need it in SImode
	     to make it acceptable to the format of extzv.  */
	  if (GET_CODE (xop0) == SUBREG && GET_MODE (xop0) != SImode)
	    abort ();
	  if (GET_CODE (xop0) == REG && GET_MODE (xop0) != SImode)
	    {
#ifdef BYTES_BIG_ENDIAN
	      xbitpos += (GET_MODE_BITSIZE (SImode)
			  - GET_MODE_BITSIZE (GET_MODE (xop0)));
#endif
	      xop0 = gen_rtx (SUBREG, SImode, xop0, 0);
	    }

	  if (xtarget == 0
	      || (flag_force_mem && GET_CODE (xtarget) == MEM))
	    xtarget = xspec_target = gen_reg_rtx (tmode);

	  if (GET_MODE (xtarget) != SImode)
	    {
	      if (GET_CODE (xtarget) == REG)
		xspec_target_subreg = xtarget = gen_lowpart (SImode, xtarget);
	      else
		xtarget = gen_reg_rtx (SImode);
	    }

	  /* If this machine's extzv insists on a register target,
	     make sure we have one.  */
	  if (! (*insn_operand_predicate[(int) CODE_FOR_extzv][0]) (xtarget, SImode))
	    xtarget = gen_reg_rtx (SImode);

	  /* On big-endian machines, we count bits from the most significant.
	     If the bit field insn does not, we must invert.  */
#if defined (BITS_BIG_ENDIAN) != defined (BYTES_BIG_ENDIAN)
	  xbitpos = unit - 1 - xbitpos;
#endif

	  bitsize_rtx = gen_rtx (CONST_INT, VOIDmode, bitsize);
	  bitpos_rtx = gen_rtx (CONST_INT, VOIDmode, xbitpos);

	  pat = gen_extzv (protect_from_queue (xtarget, 1),
			   xop0, bitsize_rtx, bitpos_rtx);
	  if (pat)
	    {
	      emit_insn (pat);
	      target = xtarget;
	      spec_target = xspec_target;
	      spec_target_subreg = xspec_target_subreg;
	    }
	  else
	    {
	      delete_insns_since (last);
	      target = extract_fixed_bit_field (tmode, op0, offset, bitsize,
						bitpos, target, 1, align);
	    }
	}
      else
#endif
	target = extract_fixed_bit_field (tmode, op0, offset, bitsize, bitpos,
					  target, 1, align);
    }
  else
    {
#ifdef HAVE_extv
      if (HAVE_extv)
	{
	  int xbitpos = bitpos, xoffset = offset;
	  rtx last = get_last_insn();
	  rtx xop0 = op0, xtarget = target;
	  rtx xspec_target = spec_target;
	  rtx xspec_target_subreg = spec_target_subreg;
	  rtx pat;

	  if (GET_CODE (xop0) == MEM)
	    {
	      /* Is the memory operand acceptable?  */
	      if (! ((*insn_operand_predicate[(int) CODE_FOR_extv][1])
		     (xop0, GET_MODE (xop0))))
		{
		  /* No, load into a reg and extract from there.  */
		  enum machine_mode bestmode = VOIDmode, trymode;
		  /* Don't use a mode bigger than the one of the value
		     to be fetched.  That mode must be okay,
		     since a bit field can be that big.  */
		  int maxsize
		    = GET_MODE_SIZE (insn_operand_mode[(int) CODE_FOR_extv][0]);
		  /* This used to use the mode desired for operand 1,
		     but that is normally QImode on most machines,
		     and QImode won't work for fields that cross byte
		     boundaries.  */

		  /* Also don't use a mode bigger than the structure.  */
		  if (total_size >= 0 && maxsize > total_size)
		    maxsize = total_size;

		  /* Find biggest machine mode we can safely use
		     to fetch from this structure.
		     But don't use a bigger mode than the insn wants.  */
		  for (trymode = QImode;
		       trymode && GET_MODE_SIZE (trymode) <= maxsize;
		       trymode = GET_MODE_WIDER_MODE (trymode))
		    if (GET_MODE_SIZE (trymode) <= align
			|| align == BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		      bestmode = trymode;
		  if (! bestmode)
		    abort ();
		  unit = GET_MODE_BITSIZE (bestmode);

		  /* Compute offset as multiple of this unit,
		     counting in bytes.  */
		  xoffset = (bitnum / unit) * GET_MODE_SIZE (bestmode);
		  xbitpos = bitnum % unit;
		  xop0 = change_address (xop0, bestmode,
					 plus_constant (XEXP (xop0, 0),
							xoffset));
		  /* Fetch it to a register in that size.  */
		  xop0 = force_reg (bestmode, xop0);

		  /* Now ref the register in the mode extv wants.  */
		  /* We used to use the mode from operand 1 in the md,
		     but that is often QImode because that's needed for MEM.
		     Here we need SImode instead.  */
		  if (bestmode != SImode)
		    xop0 = gen_rtx (SUBREG, SImode, xop0, 0);
#ifdef BYTES_BIG_ENDIAN
		  if (GET_MODE_BITSIZE (GET_MODE (xop0)) > unit)
		    xbitpos += GET_MODE_BITSIZE (GET_MODE (xop0)) - unit;
#endif
		}
	      else
		/* Get ref to first byte containing part of the field.  */
		xop0 = change_address (xop0, QImode,
				       plus_constant (XEXP (xop0, 0), xoffset));
	    }

	  /* If op0 is a register, we need it in SImode
	     to make it acceptable to the format of extv.  */
	  if (GET_CODE (xop0) == SUBREG && GET_MODE (xop0) != SImode)
	    abort ();
	  if (GET_CODE (xop0) == REG && GET_MODE (xop0) != SImode)
	    {
#ifdef BYTES_BIG_ENDIAN
	      xbitpos += (GET_MODE_BITSIZE (SImode)
			  - GET_MODE_BITSIZE (GET_MODE (xop0)));
#endif
	      xop0 = gen_rtx (SUBREG, SImode, xop0, 0);
	    }

	  if (xtarget == 0
	      || (flag_force_mem && GET_CODE (xtarget) == MEM))
	    xtarget = xspec_target = gen_reg_rtx (tmode);

	  if (GET_MODE (xtarget) != SImode)
	    {
	      if (GET_CODE (xtarget) == REG)
		xspec_target_subreg = xtarget = gen_lowpart (SImode, xtarget);
	      else
		xtarget = gen_reg_rtx (SImode);
	    }

	  /* If this machine's extv insists on a register target,
	     make sure we have one.  */
	  if (! (*insn_operand_predicate[(int) CODE_FOR_extv][0]) (xtarget, SImode))
	    xtarget = gen_reg_rtx (SImode);

	  /* On big-endian machines, we count bits from the most significant.
	     If the bit field insn does not, we must invert.  */
#if defined (BITS_BIG_ENDIAN) != defined (BYTES_BIG_ENDIAN)
	  xbitpos = unit - 1 - xbitpos;
#endif

	  bitsize_rtx = gen_rtx (CONST_INT, VOIDmode, bitsize);
	  bitpos_rtx = gen_rtx (CONST_INT, VOIDmode, xbitpos);

	  pat = gen_extv (protect_from_queue (xtarget, 1),
			  xop0, bitsize_rtx, bitpos_rtx);
	  if (pat)
	    {
	      emit_insn (pat);
	      target = xtarget;
	      spec_target = xspec_target;
	      spec_target_subreg = xspec_target_subreg;
	    }
	  else
	    {
	      delete_insns_since (last);
	      target = extract_fixed_bit_field (tmode, op0, offset, bitsize,
						bitpos, target, 0, align);
	    }
	} 
      else
#endif
	target = extract_fixed_bit_field (tmode, op0, offset, bitsize, bitpos,
					  target, 0, align);
    }
  if (target == spec_target)
    return target;
  if (target == spec_target_subreg)
    return spec_target;
  if (GET_MODE (target) != tmode && GET_MODE (target) != mode)
    return convert_to_mode (tmode, target, unsignedp);
  return target;
}

/* Extract a bit field using shifts and boolean operations
   Returns an rtx to represent the value.
   OP0 addresses a register (word) or memory (byte).
   BITPOS says which bit within the word or byte the bit field starts in.
   OFFSET says how many bytes farther the bit field starts;
    it is 0 if OP0 is a register.
   BITSIZE says how many bits long the bit field is.
    (If OP0 is a register, it may be narrower than SImode,
     but BITPOS still counts within a full word,
     which is significant on bigendian machines.)

   UNSIGNEDP is nonzero for an unsigned bit field (don't sign-extend value).
   If TARGET is nonzero, attempts to store the value there
   and return TARGET, but this is not guaranteed.
   If TARGET is not used, create a pseudo-reg of mode TMODE for the value.

   ALIGN is the alignment that STR_RTX is known to have, measured in bytes.  */

static rtx
extract_fixed_bit_field (tmode, op0, offset, bitsize, bitpos,
			 target, unsignedp, align)
     enum machine_mode tmode;
     register rtx op0, target;
     register int offset, bitsize, bitpos;
     int unsignedp;
     int align;
{
  int total_bits = BITS_PER_WORD;
  enum machine_mode mode;

  if (GET_CODE (op0) == SUBREG || GET_CODE (op0) == REG)
    {
      /* Special treatment for a bit field split across two registers.  */
      if (bitsize + bitpos > BITS_PER_WORD)
	return extract_split_bit_field (op0, bitsize, bitpos,
					unsignedp, align);
    }
  else if (bitsize + bitpos <= BITS_PER_UNIT
	   && (! SLOW_BYTE_ACCESS
	       || (align == 1
		   && BIGGEST_ALIGNMENT > 1)))
    {
      /* It fits in one byte, and either bytes are fast
	 or the alignment won't let us use anything bigger.  */
      total_bits = BITS_PER_UNIT;
      op0 = change_address (op0, QImode, 
			    plus_constant (XEXP (op0, 0), offset));
    }
  else if ((bitsize + bitpos + (offset % GET_MODE_SIZE (HImode)) * BITS_PER_UNIT
	    <= GET_MODE_BITSIZE (HImode))
	   /* If halfwords are fast, use them whenever valid.  */
	   && (! SLOW_BYTE_ACCESS
	       /* Use halfwords if larger is invalid due to alignment.  */
	       || (align == GET_MODE_SIZE (HImode)
		   && BIGGEST_ALIGNMENT > GET_MODE_SIZE (HImode))))
    {
      /* It fits in an aligned halfword, and either halfwords are fast
	 or the alignment won't let us use anything bigger.  */
      total_bits = GET_MODE_BITSIZE (HImode);

      /* Get ref to halfword containing the field.  */
      bitpos += (offset % (total_bits / BITS_PER_UNIT)) * BITS_PER_UNIT;
      offset -= (offset % (total_bits / BITS_PER_UNIT));
      op0 = change_address (op0, HImode, 
			    plus_constant (XEXP (op0, 0), offset));
    }
  else
    {
      /* Get ref to word containing the field.  */
      /* Adjust BITPOS to be position within a word,
	 and OFFSET to be the offset of that word.  */
      bitpos += (offset % (BITS_PER_WORD / BITS_PER_UNIT)) * BITS_PER_UNIT;
      offset -= (offset % (BITS_PER_WORD / BITS_PER_UNIT));
      op0 = change_address (op0, SImode,
			    plus_constant (XEXP (op0, 0), offset));

      /* Special treatment for a bit field split across two words.  */
      if (bitsize + bitpos > BITS_PER_WORD)
	return extract_split_bit_field (op0, bitsize, bitpos,
					unsignedp, align);
    }

  mode = GET_MODE (op0);

#ifdef BYTES_BIG_ENDIAN
  /* BITPOS is the distance between our msb and that of OP0.
     Convert it to the distance from the lsb.  */

  bitpos = total_bits - bitsize - bitpos;
#endif
  /* Now BITPOS is always the distance between the field's lsb and that of OP0.
     We have reduced the big-endian case to the little-endian case.  */

  if (unsignedp)
    {
      if (bitpos)
	{
	  /* If the field does not already start at the lsb,
	     shift it so it does.  */
	  tree amount = build_int_2 (bitpos, 0);
	  /* Maybe propagate the target for the shift.  */
	  /* But not if we will return it--could confuse integrate.c.  */
	  rtx subtarget = (target != 0 && GET_CODE (target) == REG
			   && !REG_FUNCTION_VALUE_P (target)
			   ? target : 0);
	  if (tmode != mode) subtarget = 0;
	  op0 = expand_shift (RSHIFT_EXPR, mode, op0, amount, subtarget, 1);
	}
      /* Convert the value to the desired mode.  */
      if (mode != tmode)
	op0 = convert_to_mode (tmode, op0, 1);

      /* Unless the msb of the field used to be the msb when we shifted,
	 mask out the upper bits.  */

      if ((GET_MODE_BITSIZE (mode) != bitpos + bitsize
#if 0
#ifdef SLOW_ZERO_EXTEND
	   /* Always generate an `and' if
	      we just zero-extended op0 and SLOW_ZERO_EXTEND, since it
	      will combine fruitfully with the zero-extend. */
	   || tmode != mode
#endif
#endif
	   )
	  && bitsize < HOST_BITS_PER_INT)
	return expand_bit_and (GET_MODE (op0), op0,
			       gen_rtx (CONST_INT, VOIDmode, (1 << bitsize) - 1),
			       target);
      return op0;
    }

  /* To extract a signed bit-field, first shift its msb to the msb of the word,
     then arithmetic-shift its lsb to the lsb of the word.  */
  op0 = force_reg (mode, op0);
  if (mode != tmode)
    target = 0;
  if (GET_MODE_BITSIZE (QImode) < GET_MODE_BITSIZE (mode)
      && GET_MODE_BITSIZE (QImode) >= bitsize + bitpos)
    mode = QImode, op0 = convert_to_mode (QImode, op0, 0);
  if (GET_MODE_BITSIZE (HImode) < GET_MODE_BITSIZE (mode)
      && GET_MODE_BITSIZE (HImode) >= bitsize + bitpos)
    mode = HImode, op0 = convert_to_mode (HImode, op0, 0);
  if (GET_MODE_BITSIZE (mode) != (bitsize + bitpos))
    {
      tree amount = build_int_2 (GET_MODE_BITSIZE (mode) - (bitsize + bitpos), 0);
      /* Maybe propagate the target for the shift.  */
      /* But not if we will return the result--could confuse integrate.c.  */
      rtx subtarget = (target != 0 && GET_CODE (target) == REG
		       && ! REG_FUNCTION_VALUE_P (target)
		       ? target : 0);
      op0 = expand_shift (LSHIFT_EXPR, mode, op0, amount, subtarget, 1);
    }

  return expand_shift (RSHIFT_EXPR, mode, op0,
		       build_int_2 (GET_MODE_BITSIZE (mode) - bitsize, 0), 
		       target, 0);
}

/* Extract a bit field that is split across two words
   and return an RTX for the result.

   OP0 is the REG, SUBREG or MEM rtx for the first of the two words.
   BITSIZE is the field width; BITPOS, position of its first bit, in the word.
   UNSIGNEDP is 1 if should zero-extend the contents; else sign-extend.  */

static rtx
extract_split_bit_field (op0, bitsize, bitpos, unsignedp, align)
     rtx op0;
     int bitsize, bitpos, unsignedp, align;
{
  /* BITSIZE_1 is size of the part in the first word.  */
  int bitsize_1 = BITS_PER_WORD - bitpos;
  /* BITSIZE_2 is size of the rest (in the following word).  */
  int bitsize_2 = bitsize - bitsize_1;
  rtx part1, part2, result;

  /* Get the part of the bit field from the first word.  */
  part1 = extract_fixed_bit_field (SImode, op0, 0, bitsize_1, bitpos,
				   0, 1, align);

  /* Offset op0 by 1 word to get to the following one.  */
  if (GET_CODE (op0) == MEM)
    op0 = change_address (op0, SImode,
			  plus_constant (XEXP (op0, 0), UNITS_PER_WORD));
  else if (GET_CODE (op0) == REG)
    op0 = gen_rtx (SUBREG, SImode, op0, 1);
  else
    op0 = gen_rtx (SUBREG, SImode, SUBREG_REG (op0), SUBREG_WORD (op0) + 1);

  /* Get the part of the bit field from the second word.  */
  part2 = extract_fixed_bit_field (SImode, op0, 0, bitsize_2, 0, 0, 1, align);

  /* Shift the more significant part up to fit above the other part.  */
#ifdef BYTES_BIG_ENDIAN
  part1 = expand_shift (LSHIFT_EXPR, SImode, part1,
			build_int_2 (bitsize_2, 0), 0, 1);
#else
  part2 = expand_shift (LSHIFT_EXPR, SImode, part2,
			build_int_2 (bitsize_1, 0), 0, 1);
#endif

  /* Combine the two parts with bitwise or.  This works
     because we extracted both parts as unsigned bit fields.  */
  result = expand_binop (SImode, ior_optab, part1, part2, 0, 1,
			 OPTAB_LIB_WIDEN);

  /* Unsigned bit field: we are done.  */
  if (unsignedp)
    return result;
  /* Signed bit field: sign-extend with two arithmetic shifts.  */
  result = expand_shift (LSHIFT_EXPR, SImode, result,
			 build_int_2 (BITS_PER_WORD - bitsize, 0), 0, 0);
  return expand_shift (RSHIFT_EXPR, SImode, result,
		       build_int_2 (BITS_PER_WORD - bitsize, 0), 0, 0);
}

/* Add INC into TARGET.  */

void
expand_inc (target, inc)
     rtx target, inc;
{
  rtx value = expand_binop (GET_MODE (target), add_optab,
			    target, inc,
			    target, 0, OPTAB_LIB_WIDEN);
  if (value != target)
    emit_move_insn (target, value);
}

/* Subtract INC from TARGET.  */

void
expand_dec (target, dec)
     rtx target, dec;
{
  rtx value = expand_binop (GET_MODE (target), sub_optab,
			    target, dec,
			    target, 0, OPTAB_LIB_WIDEN);
  if (value != target)
    emit_move_insn (target, value);
}

/* Output a shift instruction for expression code CODE,
   with SHIFTED being the rtx for the value to shift,
   and AMOUNT the tree for the amount to shift by.
   Store the result in the rtx TARGET, if that is convenient.
   If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
   Return the rtx for where the value is.  */

/* Pastel, for shifts, converts shift count to SImode here
   independent of the mode being shifted.
   Should that be done in an earlier pass?
   It turns out not to matter for C.  */

rtx
expand_shift (code, mode, shifted, amount, target, unsignedp)
     enum tree_code code;
     register enum machine_mode mode;
     rtx shifted;
     tree amount;
     register rtx target;
     int unsignedp;
{
  register rtx op1, temp = 0;
  register int left = (code == LSHIFT_EXPR || code == LROTATE_EXPR);
  int try;
  int rotate = code == LROTATE_EXPR || code == RROTATE_EXPR;
  rtx last;

  /* Previously detected shift-counts computed by NEGATE_EXPR
     and shifted in the other direction; but that does not work
     on all machines.  */

  op1 = expand_expr (amount, 0, VOIDmode, 0);

  last = get_last_insn ();

  for (try = 0; temp == 0 && try < 3; try++)
    {
      enum optab_methods methods;
      delete_insns_since (last);

      if (try == 0)
	methods = OPTAB_DIRECT;
      else if (try == 1)
	methods = OPTAB_WIDEN;
      else
	methods = OPTAB_LIB_WIDEN;

      if (rotate)
	{
	  /* Widening does not work for rotation.  */
	  if (methods != OPTAB_DIRECT)
	    methods = OPTAB_LIB;

	  temp = expand_binop (mode,
			       left ? rotl_optab : rotr_optab,
			       shifted, op1, target, -1, methods);
	}
      else if (unsignedp)
	{
	  temp = expand_binop (mode,
			       left ? lshl_optab : lshr_optab,
			       shifted, op1, target, unsignedp, methods);
	  if (temp == 0 && left)
	    temp = expand_binop (mode, ashl_optab,
				 shifted, op1, target, unsignedp, methods);
	  if (temp != 0)
	    return temp;
	}
      /* Do arithmetic shifts.
	 Also, if we are going to widen the operand, we can just as well
	 use an arithmetic right-shift instead of a logical one.  */
      if (! rotate && (! unsignedp || (! left && methods == OPTAB_WIDEN)))
	{
	  enum optab_methods methods1 = methods;

	  /* If trying to widen a log shift to an arithmetic shift,
	     don't accept an arithmetic shift of the same size.  */
	  if (unsignedp)
	    methods1 = OPTAB_MUST_WIDEN;

	  /* Arithmetic shift */

	  temp = expand_binop (mode,
			       left ? ashl_optab : ashr_optab,
			       shifted, op1, target, unsignedp, methods1);
	  if (temp != 0)
	    return temp;
	}

      if (unsignedp)
	{
	  /* No logical shift insn in either direction =>
	     try a bit-field extract instruction if we have one.  */
#ifdef HAVE_extzv
#ifndef BITS_BIG_ENDIAN
	  if (HAVE_extzv && !left
	      && ((methods == OPTAB_DIRECT && mode == SImode)
		  || (methods == OPTAB_WIDEN
		      && GET_MODE_SIZE (mode) < GET_MODE_SIZE (SImode))))
	    {
	      rtx shifted1 = convert_to_mode (SImode, shifted, 1);
	      rtx target1 = target;

	      /* If -fforce-mem, don't let the operand be in memory.  */
	      if (flag_force_mem && GET_CODE (shifted1) == MEM)
		shifted1 = force_not_mem (shifted1);

	      /* If this machine's extzv insists on a register for
		 operand 1, arrange for that.  */
	      if (! ((*insn_operand_predicate[(int) CODE_FOR_extzv][1])
		     (shifted1, SImode)))
		shifted1 = force_reg (SImode, shifted1);

	      /* If we don't have or cannot use a suggested target,
		 make a place for the result, in the proper mode.  */
	      if (methods == OPTAB_WIDEN || target1 == 0
		  || ! ((*insn_operand_predicate[(int) CODE_FOR_extzv][0])
			(target1, SImode)))
		target1 = gen_reg_rtx (SImode);

	      op1 = convert_to_mode (SImode, op1, 0);

	      /* If this machine's extzv insists on a register for
		 operand 3, arrange for that.  */
	      if (! ((*insn_operand_predicate[(int) CODE_FOR_extzv][3])
		     (op1, SImode)))
		op1 = force_reg (SImode, op1);

	      op1 = protect_from_queue (op1, 1);

	      /* TEMP gets the width of the bit field to extract:
		 wordsize minus # bits to shift by.  */
	      if (GET_CODE (op1) == CONST_INT)
		temp = gen_rtx (CONST_INT, VOIDmode,
				(GET_MODE_BITSIZE (mode) - INTVAL (op1)));
	      else
		temp = expand_binop (SImode, sub_optab,
				     gen_rtx (CONST_INT, VOIDmode,
					      GET_MODE_BITSIZE (mode)),
				     op1, gen_reg_rtx (SImode),
				     0, OPTAB_LIB_WIDEN);
	      /* Now extract with width TEMP, omitting OP1 least sig bits.  */
	      emit_insn (gen_extzv (protect_from_queue (target1, 1),
				    protect_from_queue (shifted1, 0),
				    temp, op1));
	      return convert_to_mode (mode, target1, 1);
	    }
	  /* Can also do logical shift with signed bit-field extract
	     followed by inserting the bit-field at a different position.
	     That strategy is not yet implemented.  */
#endif /* not BITS_BIG_ENDIAN */
#endif /* HAVE_extzv */
	  /* We have failed to generate the logical shift and will abort.  */
	}
    }
  if (temp == 0)
    abort ();
  return temp;
}

/* Output an instruction or two to bitwise-and OP0 with OP1
   in mode MODE, with output to TARGET if convenient and TARGET is not zero.
   Returns where the result is.  */
/* This function used to do more; now it could be eliminated.  */

rtx
expand_bit_and (mode, op0, op1, target)
     enum machine_mode mode;
     rtx op0, op1, target;
{
  register rtx temp;

  /* First try to open-code it directly.  */
  temp = expand_binop (mode, and_optab, op0, op1, target, 1, OPTAB_LIB_WIDEN);
  if (temp == 0)
    abort ();
  return temp;
}

/* Perform a multiplication and return an rtx for the result.
   MODE is mode of value; OP0 and OP1 are what to multiply (rtx's);
   TARGET is a suggestion for where to store the result (an rtx).

   We check specially for a constant integer as OP1.
   If you want this check for OP0 as well, then before calling
   you should swap the two operands if OP0 would be constant.  */

rtx
expand_mult (mode, op0, op1, target, unsignedp)
     enum machine_mode mode;
     register rtx op0, op1, target;
     int unsignedp;
{
  /* Don't use the function value register as a target
     since we have to read it as well as write it,
     and function-inlining gets confused by this.  */
  if (target && REG_P (target) && REG_FUNCTION_VALUE_P (target))
    target = 0;

  if (GET_CODE (op1) == CONST_INT)
    {
      register int foo;
      int bar;
      int negate = INTVAL (op1) < 0;
      int absval = INTVAL (op1) * (negate ? -1 : 1);

      /* Is multiplier a power of 2, or minus that?  */
      foo = exact_log2 (absval);
      if (foo >= 0)
	{
	  rtx tem;
	  if (foo == 0)
	    tem = op0;
	  else
	    tem = expand_shift (LSHIFT_EXPR, mode, op0,
				build_int_2 (foo, 0),
				target, 0);
	  return (negate
		  ? expand_unop (mode, neg_optab, tem, target, 0)
		  : tem);
	}
      /* Is multiplier a sum of two powers of 2, or minus that?  */
      bar = floor_log2 (absval);
      foo = exact_log2 (absval - (1 << bar));
      if (bar >= 0 && foo >= 0)
	{
	  rtx tem =
	    force_operand (gen_rtx (PLUS, mode,
				    expand_shift (LSHIFT_EXPR, mode, op0,
				   		  build_int_2 (bar - foo, 0),
				   		  0, 0),
				    op0),
			   ((foo == 0 && ! negate) ? target : 0));

	  if (foo != 0)
	    tem = expand_shift (LSHIFT_EXPR, mode, tem,
				build_int_2 (foo, 0),
				negate ? 0 : target, 0);

	  return negate ? expand_unop (mode, neg_optab, tem, target, 0) : tem;
	}
    }
  /* This used to use umul_optab if unsigned,
     but I think that for non-widening multiply there is no difference
     between signed and unsigned.  */
  op0 = expand_binop (mode, smul_optab,
		      op0, op1, target, unsignedp, OPTAB_LIB_WIDEN);
  if (op0 == 0)
    abort ();
  return op0;
}

/* Emit the code to divide OP0 by OP1, putting the result in TARGET
   if that is convenient, and returning where the result is.
   You may request either the quotient or the remainder as the result;
   specify REM_FLAG nonzero to get the remainder.

   CODE is the expression code for which kind of division this is;
   it controls how rounding is done.  MODE is the machine mode to use.
   UNSIGNEDP nonzero means do unsigned division.  */

/* ??? For CEIL_MOD_EXPR, can compute incorrect remainder with ANDI
   and then correct it by or'ing in missing high bits
   if result of ANDI is nonzero.
   For ROUND_MOD_EXPR, can use ANDI and then sign-extend the result.
   This could optimize to a bfexts instruction.
   But C doesn't use these operations, so their optimizations are
   left for later.  */

rtx
expand_divmod (rem_flag, code, mode, op0, op1, target, unsignedp)
     int rem_flag;
     enum tree_code code;
     enum machine_mode mode;
     register rtx op0, op1, target;
     int unsignedp;
{
  register rtx temp;
  int log = -1;
  int can_clobber_op0;
  int mod_insn_no_good = 0;
  rtx adjusted_op0 = op0;

  /* Don't use the function value register as a target
     since we have to read it as well as write it,
     and function-inlining gets confused by this.  */
  if (target && REG_P (target) && REG_FUNCTION_VALUE_P (target))
    target = 0;

  /* Don't clobber an operand while doing a multi-step calculation.  */
  if (target)
    if ((rem_flag && (reg_mentioned_p (target, op0)
		      || (GET_CODE (op0) == MEM && GET_CODE (target) == MEM)))
	|| reg_mentioned_p (target, op1)
	|| (GET_CODE (op1) == MEM && GET_CODE (target) == MEM))
      target = 0;

  if (target == 0)
    target = gen_reg_rtx (mode);

  can_clobber_op0 = (GET_CODE (op0) == REG && op0 == target);

  if (GET_CODE (op1) == CONST_INT)
    log = exact_log2 (INTVAL (op1));

  /* If log is >= 0, we are dividing by 2**log, and will do it by shifting,
     which is really floor-division.  Otherwise we will really do a divide,
     and we assume that is trunc-division.

     We must correct the dividend by adding or subtracting something
     based on the divisor, in order to do the kind of rounding specified
     by CODE.  The correction depends on what kind of rounding is actually
     available, and that depends on whether we will shift or divide.  */

  switch (code)
    {
    case TRUNC_MOD_EXPR:
    case TRUNC_DIV_EXPR:
      if (log >= 0 && ! unsignedp)
	{
	  rtx label = gen_label_rtx ();
	  if (! can_clobber_op0)
	    adjusted_op0 = copy_to_suggested_reg (adjusted_op0, target);
	  emit_cmp_insn (adjusted_op0, const0_rtx, 0, 0, 0);
	  emit_jump_insn (gen_bge (label));
	  expand_inc (adjusted_op0, plus_constant (op1, -1));
	  emit_label (label);
	  mod_insn_no_good = 1;
	}
      break;

    case FLOOR_DIV_EXPR:
    case FLOOR_MOD_EXPR:
      if (log < 0 && ! unsignedp)
	{
	  rtx label = gen_label_rtx ();
	  if (! can_clobber_op0)
	    adjusted_op0 = copy_to_suggested_reg (adjusted_op0, target);
	  emit_cmp_insn (adjusted_op0, const0_rtx, 0, 0, 0);
	  emit_jump_insn (gen_bge (label));
	  expand_dec (adjusted_op0, op1);
	  expand_inc (adjusted_op0, const1_rtx);
	  emit_label (label);
	  mod_insn_no_good = 1;
	}
      break;

    case CEIL_DIV_EXPR:
    case CEIL_MOD_EXPR:
      if (! can_clobber_op0)
	adjusted_op0 = copy_to_suggested_reg (adjusted_op0, target);
      if (log < 0)
	{
	  rtx label = 0;
	  if (! unsignedp)
	    {
	      label = gen_label_rtx ();
	      emit_cmp_insn (adjusted_op0, const0_rtx, 0, 0, 0);
	      emit_jump_insn (gen_ble (label));
	    }
	  expand_inc (adjusted_op0, op1);
	  expand_dec (adjusted_op0, const1_rtx);
	  if (! unsignedp)
	    emit_label (label);
	}
      else
	{
	  adjusted_op0 = expand_binop (GET_MODE (target), add_optab,
				       adjusted_op0, plus_constant (op1, -1),
				       0, 0, OPTAB_LIB_WIDEN);
	}
      mod_insn_no_good = 1;
      break;

    case ROUND_DIV_EXPR:
    case ROUND_MOD_EXPR:
      if (! can_clobber_op0)
	adjusted_op0 = copy_to_suggested_reg (adjusted_op0, target);
      if (log < 0)
	{
	  op1 = expand_shift (RSHIFT_EXPR, mode, op1, integer_one_node, 0, 0);
	  if (! unsignedp)
	    {
	      rtx label = gen_label_rtx ();
	      emit_cmp_insn (adjusted_op0, const0_rtx, 0, 0, 0);
	      emit_jump_insn (gen_bge (label));
	      expand_unop (mode, neg_optab, op1, op1, 0);
	      emit_label (label);
	    }
	  expand_inc (adjusted_op0, op1);
	}
      else
	{
	  op1 = gen_rtx (CONST_INT, VOIDmode, INTVAL (op1) / 2);
	  expand_inc (adjusted_op0, op1);
	}
      mod_insn_no_good = 1;
      break;
    }

  if (rem_flag && !mod_insn_no_good)
    {
      /* Try to produce the remainder directly */
      if (log >= 0)
	{
	  return expand_bit_and (mode, adjusted_op0,
				 gen_rtx (CONST_INT, VOIDmode,
					  INTVAL (op1) - 1),
				 target);
	}
      else
	{
	  /* See if we can do remainder without a library call.  */
	  temp = sign_expand_binop (mode, umod_optab, smod_optab,
				    adjusted_op0, op1, target,
				    unsignedp, OPTAB_WIDEN);
	  if (temp != 0)
	    return temp;
	  /* No luck there.
	     Can we do remainder and divide at once without a library call?  */
	  temp = gen_reg_rtx (mode);
	  if (expand_twoval_binop (unsignedp ? udivmod_optab : sdivmod_optab,
				   adjusted_op0, op1,
				   0, temp, unsignedp))
	    return temp;
	  temp = 0;
	}
    }

  /* Produce the quotient.  */
  if (log >= 0)
    temp = expand_shift (RSHIFT_EXPR, mode, adjusted_op0,
			 build_int_2 (exact_log2 (INTVAL (op1)), 0),
			 target, unsignedp);
  else if (rem_flag && !mod_insn_no_good)
    /* If producing quotient in order to subtract for remainder,
       and a remainder subroutine would be ok,
       don't use a divide subroutine.  */
    temp = sign_expand_binop (mode, udiv_optab, sdiv_optab,
			      adjusted_op0, op1, target,
			      unsignedp, OPTAB_WIDEN);
  else
    {
      /* Try a quotient insn, but not a library call.  */
      temp = sign_expand_binop (mode, udiv_optab, sdiv_optab,
				adjusted_op0, op1, target,
				unsignedp, OPTAB_WIDEN);
      if (temp == 0)
	{
	  /* No luck there.  Try a quotient-and-remainder insn,
	     keeping the quotient alone.  */
	  temp = gen_reg_rtx (mode);
	  if (! expand_twoval_binop (unsignedp ? udivmod_optab : sdivmod_optab,
				     adjusted_op0, op1,
				     temp, 0, unsignedp))
	    temp = 0;
	}

      /* If still no luck, use a library call.  */
      if (temp == 0)
	temp = sign_expand_binop (mode, udiv_optab, sdiv_optab,
				  adjusted_op0, op1, target,
				  unsignedp, OPTAB_LIB_WIDEN);
    }

  /* If we really want the remainder, get it by subtraction.  */
  if (rem_flag)
    {
      if (temp == 0)
	{
	  /* No divide instruction either.  Use library for remainder.  */
	  temp = sign_expand_binop (mode, umod_optab, smod_optab,
				    op0, op1, target,
				    unsignedp, OPTAB_LIB_WIDEN);
	}
      else
	{
	  /* We divided.  Now finish doing X - Y * (X / Y).  */
	  temp = expand_mult (mode, temp, op1, temp, unsignedp);
	  if (! temp) abort ();
	  temp = expand_binop (mode, sub_optab, op0,
			       temp, target, unsignedp, OPTAB_LIB_WIDEN);
	}
    }

  if (temp == 0)
    abort ();
  return temp;
}

/* Return a tree node with data type TYPE, describing the value of X.
   Usually this is an RTL_EXPR, if there is no obvious better choice.  */

static tree
make_tree (type, x)
     tree type;
     rtx x;
{
  tree t;
  switch (GET_CODE (x))
    {
    case CONST_INT:
      t = build_int_2 (INTVAL (x), 0);
      TREE_TYPE (t) = type;
      return fold (t);

    default:
      t = make_node (RTL_EXPR);
      TREE_TYPE (t) = type;
      RTL_EXPR_RTL (t) = x;
      /* There are no insns to be output
	 when this rtl_expr is used.  */
      RTL_EXPR_SEQUENCE (t) = 0;
      return t;
    }
}

/* Return an rtx representing the value of X * MULT + ADD.
   MODE is the machine mode for the computation.
   UNSIGNEDP is non-zero to do unsigned multiplication.
   This may emit insns.  */

rtx
expand_mult_add (x, mult, add, mode, unsignedp)
     rtx x, mult, add;
     enum machine_mode mode;
     int unsignedp;
{
  tree type = type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
  tree prod = fold (build (MULT_EXPR, type, make_tree (type, x),
			   make_tree (type, mult)));
  tree sum = fold (build (PLUS_EXPR, type, prod, make_tree (type, add)));
  return expand_expr (sum, 0, VOIDmode, 0);
}
