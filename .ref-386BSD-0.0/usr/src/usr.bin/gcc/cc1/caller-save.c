/* Save and restore call-clobbered registers which are live across a call.
   Copyright (C) 1989 Free Software Foundation, Inc.

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
#include "insn-config.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "reload.h"
#include "recog.h"
#include "basic-block.h"

/* Set of hard regs currently live (during scan of all insns).  */

static HARD_REG_SET hard_regs_live;

/* The block of storage on the stack where regs are saved */

static rtx save_block_addr;
static int save_block_size;

/* A REG rtx for each hard register that has been saved.  */

static rtx save_reg_rtx[FIRST_PSEUDO_REGISTER];

static void set_reg_live ();
static void clear_reg_live ();
static void insert_call_saves ();
static void emit_mult_save ();
static void emit_mult_restore ();
static rtx grow_save_block ();
static enum machine_mode choose_hard_reg_mode ();

/* Find the places where hard regs are live across calls and save them.  */

save_call_clobbered_regs ()
{
  rtx insn;
  int b;

  if (obey_regdecls)
    return;
  
  save_block_size = 0;
  save_block_addr = 0;
  bzero (save_reg_rtx, sizeof save_reg_rtx);

  for (b = 0; b < n_basic_blocks; b++)
    {
      regset regs_live = basic_block_live_at_start[b];
      int offset, bit, i;

      /* Compute hard regs live at start of block -- this is the
	 real hard regs marked live, plus live pseudo regs that
	 have been renumbered to hard regs.  */

#ifdef HARD_REG_SET
      hard_regs_live = *regs_live;
#else
      COPY_HARD_REG_SET (hard_regs_live, regs_live);
#endif

      for (offset = 0, i = 0; offset < regset_size; offset++)
	{
	  if (regs_live[offset] == 0)
	    i += HOST_BITS_PER_INT;
	  else
	    for (bit = 1; bit && i < max_regno; bit <<= 1, i++)
	      if ((regs_live[offset] & bit) && reg_renumber[i] >= 0)
		SET_HARD_REG_BIT (hard_regs_live, reg_renumber[i]);
	}

      /* Now scan the insns in the block, keeping track of what hard
	 regs are live as we go.  When we see a call, save the live
	 call-clobbered hard regs.  */

      for (insn = basic_block_head[b]; TRUE; insn = NEXT_INSN (insn))
	{
	  RTX_CODE code = GET_CODE (insn);

	  if (code == CALL_INSN)
	    insert_call_saves (insn);

	  if (code == INSN || code == CALL_INSN || code == JUMP_INSN)
	    {
	      rtx link;

	      /* NB: the normal procedure is to first enliven any
		 registers set by insn, then deaden any registers that
		 had their last use at insn.  This is incorrect now,
		 since multiple pseudos may have been mapped to the
		 same hard reg, and the death notes are ambiguous.  So
		 it must be done in the other, safe, order.  */

	      for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_DEAD)
		  clear_reg_live (XEXP (link, 0));

	      note_stores (PATTERN (insn), set_reg_live);
	    }

	  if (insn == basic_block_end[b])
	    break;
	}
    }
}

/* Here from note_stores when an insn stores a value in a register.
   Set the proper bit or bits in hard_regs_live.  */

static void
set_reg_live (reg, setter)
     rtx reg, setter;
{
  register int regno;

  /* WORD is which word of a multi-register group is being stored.
     For the case where the store is actually into a SUBREG of REG.
     Except we don't use it; I believe the entire REG needs to be
     live.  */
  int word = 0;

  if (GET_CODE (reg) == SUBREG)
    {
      word = SUBREG_WORD (reg);
      reg = SUBREG_REG (reg);
    }

  if (GET_CODE (reg) != REG)
    return;

  regno = REGNO (reg);

  /* For pseudo reg, see if it has been assigned a hardware reg.  */
  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno] /* + word */;

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (regno < FIRST_PSEUDO_REGISTER && ! call_fixed_regs[regno])
    {
      register int last = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (regno < last)
	{
	  SET_HARD_REG_BIT (hard_regs_live, regno);
	  regno++;
	}
    }
}

/* Here when a REG_DEAD note records the last use of a reg.  Clear
   the appropriate bit or bits in hard_regs_live.  */

static void
clear_reg_live (reg)
     rtx reg;
{
  register int regno = REGNO (reg);

  /* For pseudo reg, see if it has been assigned a hardware reg.  */
  if (reg_renumber[regno] >= 0)
    regno = reg_renumber[regno];

  /* Handle hardware regs (and pseudos allocated to hard regs).  */
  if (regno < FIRST_PSEUDO_REGISTER && ! call_fixed_regs[regno])
    {
      /* Pseudo regs already assigned hardware regs are treated
	 almost the same as explicit hardware regs.  */
      register int last = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      while (regno < last)
	{
	  CLEAR_HARD_REG_BIT (hard_regs_live, regno);
	  regno++;
	}
    }
}      

/* Insert insns to save and restore live call-clobbered regs around
   call insn INSN.  */

static void
insert_call_saves (insn)
     rtx insn;
{
  int regno;
  int save_block_size_needed;
  int save_block_offset[FIRST_PSEUDO_REGISTER];

  save_block_size_needed = 0;
  
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
    {
      save_block_offset[regno] = -1;
      if (call_used_regs[regno] && ! call_fixed_regs[regno]
	  && TEST_HARD_REG_BIT (hard_regs_live, regno))
	{
	  enum machine_mode mode = choose_hard_reg_mode (regno);
	  int align = GET_MODE_UNIT_SIZE (mode);
	  if (align > BIGGEST_ALIGNMENT / BITS_PER_UNIT)
	    align = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
	  save_block_size_needed =
	    ((save_block_size_needed + align - 1) / align) * align;
	  save_block_offset[regno] = save_block_size_needed;
	  save_block_size_needed += GET_MODE_SIZE (mode);
	  if (! save_reg_rtx[regno])
	    save_reg_rtx[regno] = gen_rtx (REG, mode, regno);
	}
    }

  if (save_block_size < save_block_size_needed)
    save_block_addr = grow_save_block (save_block_addr,
				       save_block_size_needed);
  emit_mult_save (insn, save_block_addr, save_block_offset);
  emit_mult_restore (insn, save_block_addr, save_block_offset);
}

/* Emit a string of stores to save the hard regs listed in
   OFFSET[] at address ADDR.  Emit them before INSN.
   OFFSET[reg] is -1 if reg should not be saved, or a
   suitably-aligned offset from ADDR.  
   The offsets actually used do not have to be those listed
   in OFFSET, but should fit in a block of the same size.  */

static void
emit_mult_save (insn, addr, offset)
     rtx insn, addr;
     int offset[];
{
  int regno;
  /* A register to use as a temporary for address calculations.  */
  rtx tempreg;
  /* A register that could be used as that temp if we save and restore it.  */
  rtx can_push_reg;
  /* Nonzero means we need to save a register to use it as TEMPREG.  */
  int needpush;
  /* The amount the stack is decremented to save that register (if we do).  */
  int decrement;
  /* Record which regs we save, in case we branch to retry.  */
  char already_saved[FIRST_PSEUDO_REGISTER];

  bzero (already_saved, sizeof already_saved);

  /* Hair is needed because sometimes the addresses to save in are
     not valid (offsets too big).
     So we need a reg, TEMPREG, to compute addresses in.

     We look first for an empty reg to use.
     Sometimes no reg is empty.  Then we push a reg, use it, and pop it.

     Sometimes the only reg to push and pop this way is one we want to save.
     We can't save it while using it as a temporary.
     So we save all the other registers, pop it, and go back to `retry'.
     At that point, only this reg remains to be saved;
     all the others already saved are empty.
     So one of them can be the temporary for this one.  */

  /* Sometimes we can't save all the regs conveniently at once, just some.
     If that happens, we branch back here to save the rest.  */
 retry:
  needpush = 0;
  tempreg = 0;
  can_push_reg = 0;

  /* Set NEEDPUSH if any save-addresses are not valid memory addresses.
     If any register is available, record it in TEMPREG.
     If any register doesn't need saving here, record it in CAN_PUSH_REG.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
    {
      if (offset[regno] >= 0 && ! already_saved[regno])
	{
	  rtx reg = save_reg_rtx[regno];
	  rtx addr1 = plus_constant (addr, offset[regno]);
	  if (memory_address_p (GET_MODE (reg), addr1))
	    needpush = 1;
	}

      /* A call-clobbered reg that is dead, or already saved,
	 can be used as a temporary for sure, at no extra cost.  */
      if (tempreg == 0 && call_used_regs[regno] && ! fixed_regs[regno]
	  && !(offset[regno] >= 0 && ! already_saved[regno])
	  && HARD_REGNO_MODE_OK (regno, Pmode))
	{
	  tempreg = gen_rtx (REG, Pmode, regno);
	  /* Don't use it if not valid for addressing.  */
	  if (! strict_memory_address_p (QImode, tempreg))
	    tempreg = 0;
	}

      /* A call-saved reg can be a temporary if we push and pop it.  */
      if (can_push_reg == 0 && ! call_used_regs[regno]
	  && HARD_REGNO_MODE_OK (regno, Pmode))
	{
	  can_push_reg = gen_rtx (REG, Pmode, regno);
	  /* Don't use it if not valid for addressing.  */
	  if (! strict_memory_address_p (QImode, can_push_reg))
	    can_push_reg = 0;
	}
    }

  /* Clear NEEDPUSH if we already found an empty reg.  */
  if (tempreg != 0)
    needpush = 0;

  /* If we need a temp reg and none is free, make one free.  */
  if (needpush)
    {
      /* Choose a reg, preferably not among those it is our job to save.  */
      if (can_push_reg != 0)
	tempreg = can_push_reg;
      else
	{
	  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
	    if (offset[regno] >= 0 && !already_saved[regno]
		&& HARD_REGNO_MODE_OK (regno, Pmode))
	      {
		tempreg = gen_rtx (REG, Pmode, regno);
		/* Don't use it if not valid for addressing.  */
		if (! strict_memory_address_p (QImode, tempreg))
		  tempreg = 0;
		else
		  break;
	      }
	}

      /* Push it on the stack.  */
#ifdef STACK_GROWS_DOWNWARD
      decrement = UNITS_PER_WORD;
#else
      decrement = - UNITS_PER_WORD;
#endif

      emit_insn_before (gen_add2_insn (stack_pointer_rtx,
				       gen_rtx (CONST_INT, VOIDmode, -decrement)),
			insn);
      emit_insn_before (gen_move_insn (gen_rtx (MEM, Pmode, stack_pointer_rtx),
				       tempreg),
			insn);
    }

  /* Save the regs we are supposed to save, aside from TEMPREG.
     Use TEMPREG for address calculations when needed.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
    if (offset[regno] >= 0 && ! already_saved[regno]
	&& tempreg != 0 && REGNO (tempreg) != regno)
      {
	rtx reg = save_reg_rtx[regno];
	rtx addr1 = plus_constant (addr, offset[regno]);
	rtx temp;
	if (! memory_address_p (GET_MODE (reg), addr1))
	  {
	    if (GET_CODE (addr1) != PLUS)
	      abort ();
	    if (GET_CODE (XEXP (addr1, 1)) != CONST_INT
		|| GET_CODE (XEXP (addr1, 0)) != REG)
	      abort ();
	    emit_insn_before (gen_move_insn (tempreg, XEXP (addr1, 0)), insn);
	    emit_insn_before (gen_add2_insn (tempreg, XEXP (addr1, 1)), insn);
	    addr1 = tempreg;
	  }
	temp = gen_rtx (MEM, GET_MODE (reg), addr1);
	emit_insn_before (gen_move_insn (temp, reg), insn);
	already_saved[regno] = 1;
      }

  /* If we pushed TEMPREG to make it free, pop it.  */
  if (needpush)
    {
      emit_insn_before (gen_move_insn (tempreg,
				       gen_rtx (MEM, Pmode, stack_pointer_rtx)),
			insn);
      emit_insn_before (gen_add2_insn (stack_pointer_rtx,
				       gen_rtx (CONST_INT, VOIDmode, decrement)),
			insn);
    }

  /* If TEMPREG itself needs saving, go back and save it.
     There are plenty of free regs now, those already saved.  */
  if (tempreg != 0
      && offset[REGNO (tempreg)] >= 0 && ! already_saved[REGNO (tempreg)])
    goto retry;
}

/* Emit a string of loads to restore the hard regs listed in
   OFFSET[] from address ADDR; insert the loads after INSN.
   OFFSET[reg] is -1 if reg should not be loaded, or a
   suitably-aligned offset from ADDR.  
   The offsets actually used do not need to be those provided in
   OFFSET, but should agree with whatever emit_mult_save does.  */

static void
emit_mult_restore (insn, addr, offset)
     rtx insn, addr;
     int offset[];
{
  int regno;

  /* Number of regs now needing to be restored.  */
  int restore_count;
  /* A register to use as a temporary for address calculations.  */
  rtx tempreg;
  /* A register available for that purpose but less desirable.  */
  rtx maybe_tempreg;
  /* A register that could be used as that temp if we push and pop it.  */
  rtx can_push_reg;
  /* Nonzero means we need to push and pop a register to use it as TEMPREG.  */
  int needpush;
  /* The amount the stack is decremented to save that register (if we do).  */
  int decrement;
  /* Record which regs we restore, in case we branch to retry.  */
  char already_restored[FIRST_PSEUDO_REGISTER];

  bzero (already_restored, sizeof already_restored);

  /* Note: INSN can't be the last insn, since if it were,
     no regs would live across it.  */
  insn = NEXT_INSN (insn);
  if (insn == 0)
    abort ();
  /* Now we can insert before INSN.
     That is convenient because we can insert them in the order
     that they should ultimately appear.  */

  /* Hair is needed because sometimes the addresses to restore from are
     not valid (offsets too big).
     So we need a reg, TEMPREG, to compute addresses in.

     We look first for an empty reg to use.
     Sometimes no reg is empty.  Then we push a reg, use it, and pop it.

     If all the suitable regs need to be restored,
     that strategy won't work.  So we restore all but one, using that one
     as a temporary.  Then we jump to `retry' to restore that one,
     pushing and popping another (already restored) as a temporary.  */

 retry:
  needpush = 0;
  tempreg = 0;
  can_push_reg = 0;
  restore_count = 0;

  /* Set NEEDPUSH if any restore-addresses are not valid memory addresses.
     If any register is available, record it in TEMPREG.
     Otherwise, one register yet to be restored goes in MAYBE_TEMPREG,
     and can be used as TEMPREG for any other regs to be restored.
     If any register doesn't need restoring, record it in CAN_PUSH_REG.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
    {
      if (offset[regno] >= 0 && ! already_restored[regno])
	{
	  rtx reg = save_reg_rtx[regno];
	  rtx addr1 = plus_constant (addr, offset[regno]);

	  restore_count++;

	  if (memory_address_p (GET_MODE (reg), addr1))
	    needpush = 1;

	  /* Find a call-clobbered reg that needs restoring.
	     We can use it as a temporary if we defer restoring it.  */
	  if (maybe_tempreg == 0)
	    {
	      maybe_tempreg = gen_rtx (REG, Pmode, regno);
	      /* Don't use it if not valid for addressing.  */
	      if (! strict_memory_address_p (QImode, maybe_tempreg))
		maybe_tempreg = 0;
	    }
	}

      /* If any call-clobbered reg is dead, put it in TEMPREG.
	 It can be used as a temporary at no extra cost.  */
      if (tempreg == 0 && call_used_regs[regno] && ! fixed_regs[regno]
	  && ! offset[regno] >= 0
	  && HARD_REGNO_MODE_OK (regno, Pmode))
	{
	  tempreg = gen_rtx (REG, Pmode, regno);
	  /* Don't use it if not valid for addressing.  */
	  if (! strict_memory_address_p (QImode, tempreg))
	    tempreg = 0;
	}

      /* Any non-call-clobbered reg, put in CAN_PUSH_REG.
	 It can be used as a temporary if we push and pop it.  */
      if (can_push_reg == 0 && ! call_used_regs[regno]
	  && HARD_REGNO_MODE_OK (regno, Pmode))
	{
	  can_push_reg = gen_rtx (REG, Pmode, regno);
	  /* Don't use it if not valid for addressing.  */
	  if (! strict_memory_address_p (QImode, can_push_reg))
	    can_push_reg = 0;
	}
      /* Any reg we already restored can be a temporary
	 if we push and pop it.  */
      if (can_push_reg == 0 && already_restored[regno]
	  && HARD_REGNO_MODE_OK (regno, Pmode))
	{
	  can_push_reg = gen_rtx (REG, Pmode, regno);
	  /* Don't use it if not valid for addressing.  */
	  if (! strict_memory_address_p (QImode, can_push_reg))
	    can_push_reg = 0;
	}
    }

  /* If 2 or more regs need to be restored, use one as a temp reg
     for the rest (if we need a tempreg).  */
  if (tempreg == 0 && maybe_tempreg != 0 && restore_count > 1)
    tempreg = maybe_tempreg;

  /* Clear NEEDPUSH if we already found an empty reg.  */
  if (tempreg != 0)
    needpush = 0;

  /* If we need a temp reg and none is free, make one free.  */
  if (needpush)
    {
      tempreg = can_push_reg;

      /* Push it on the stack.  */
#ifdef STACK_GROWS_DOWNWARD
      decrement = UNITS_PER_WORD;
#else
      decrement = - UNITS_PER_WORD;
#endif

      emit_insn_before (gen_add2_insn (stack_pointer_rtx,
				       gen_rtx (CONST_INT, VOIDmode, -decrement)),
			insn);
      emit_insn_before (gen_move_insn (gen_rtx (MEM, Pmode, stack_pointer_rtx),
				       tempreg),
			insn);
    }

  /* Restore the regs we are supposed to restore, aside from TEMPREG.
     Use TEMPREG for address calculations when needed.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; ++regno)
    if (offset[regno] >= 0 && ! already_restored[regno]
	&& tempreg != 0 && REGNO (tempreg) != regno)
      {
	rtx reg = save_reg_rtx[regno];
	rtx addr1 = plus_constant (addr, offset[regno]);
	rtx temp;
	if (! memory_address_p (GET_MODE (reg), addr1))
	  {
	    if (GET_CODE (addr1) != PLUS)
	      abort ();
	    if (GET_CODE (XEXP (addr1, 1)) != CONST_INT
		|| GET_CODE (XEXP (addr1, 0)) != REG)
	      abort ();
	    emit_insn_before (gen_move_insn (tempreg, XEXP (addr1, 0)), insn);
	    emit_insn_before (gen_add2_insn (tempreg, XEXP (addr1, 1)), insn);
	    addr1 = tempreg;
	  }
	temp = gen_rtx (MEM, GET_MODE (reg), addr1);
	emit_insn_before (gen_move_insn (reg, temp), insn);
	already_restored[regno] = 1;
      }

  /* If we pushed TEMPREG to make it free, pop it.  */
  if (needpush)
    {
      emit_insn_before (gen_move_insn (tempreg,
				       gen_rtx (MEM, Pmode, stack_pointer_rtx)),
			insn);
      emit_insn_before (gen_add2_insn (stack_pointer_rtx,
				       gen_rtx (CONST_INT, VOIDmode, decrement)),
			insn);
    }

  /* If TEMPREG itself needs restoring, go back and restore it.
     We can find a reg already restored to push and use as a temporary.  */
  if (tempreg != 0
      && offset[REGNO (tempreg)] >= 0 && ! already_restored[REGNO (tempreg)])
    goto retry;
}

/* Return the address of a new block of size SIZE on the stack.
   The old save block is at ADDR; ADDR is 0 if no block exists yet.  */

static rtx
grow_save_block (addr, size)
     rtx addr;
     int size;
{
  rtx newaddr;

  /* Keep the size a multiple of the main allocation unit.  */
  size = (((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
	   / (BIGGEST_ALIGNMENT / BITS_PER_UNIT))
	  * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  /* If no save block exists yet, create one and return it.  */
  if (! addr)
    {
      save_block_size = size;
      return XEXP (assign_stack_local (BLKmode, size), 0);
    }

  /* Get a new block and coalesce it with the old one.  */
  newaddr = XEXP (assign_stack_local (BLKmode, size - save_block_size), 0);
  if (GET_CODE (newaddr) == PLUS
      && XEXP (newaddr, 0) == frame_pointer_rtx
      && GET_CODE (XEXP (newaddr, 1)) == CONST_INT
      && GET_CODE (addr) == PLUS
      && XEXP (addr, 0) == frame_pointer_rtx
      && GET_CODE (XEXP (addr, 1)) == CONST_INT
      && ((INTVAL (XEXP (newaddr, 1)) - INTVAL (XEXP (addr, 1))
	   == size - save_block_size)
	  || (INTVAL (XEXP (addr, 1)) - INTVAL (XEXP (newaddr, 1))
	      == size - save_block_size)))
    {
      save_block_size = size;
      if (INTVAL (XEXP (newaddr, 1)) < INTVAL (XEXP (addr, 1)))
	return newaddr;
      else
	return addr;
    }

  /* They didn't coalesce, find out why */
  abort ();			

  save_block_size = size;
  return XEXP (assign_stack_local (BLKmode, size), 0);
}

/* Return a machine mode that is legitimate for hard reg REGNO
   and large enough to save the whole register.  */

static enum machine_mode
choose_hard_reg_mode (regno)
     int regno;
{
  enum reg_class class = REGNO_REG_CLASS (regno);

  if (CLASS_MAX_NREGS (class, DImode) == 1
      && HARD_REGNO_MODE_OK (regno, DImode))
    return DImode;
  else if (CLASS_MAX_NREGS (class, DFmode) == 1
	   && HARD_REGNO_MODE_OK (regno, DFmode))
    return DFmode;
  else if (CLASS_MAX_NREGS (class, SImode) == 1
	   && HARD_REGNO_MODE_OK (regno, SImode))
    return SImode;
  else if (CLASS_MAX_NREGS (class, SFmode) == 1
	   && HARD_REGNO_MODE_OK (regno, SFmode))
    return SFmode;
  else if (CLASS_MAX_NREGS (class, HImode) == 1
	   && HARD_REGNO_MODE_OK (regno, HImode))
    return HImode;
  else
    abort ();
}
