
;;- Machine description for SPARC chip for GNU C compiler
;;   Copyright (C) 1988, 1989 Free Software Foundation, Inc.
;;   Contributed by Michael Tiemann (tiemann@mcc.com)

;; This file is part of GNU CC.

;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

;;- Operand classes for the register allocator:

;; Compare instructions.
;; This controls RTL generation and register allocation.

;; Put cmpsi first among compare insns so it matches two CONST_INT operands.

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "arith_operand" "r,rI")
		 (match_operand:SI 1 "arith_operand" "I,r")))]
  ""
  "*
{
  if (! REG_P (operands[0]))
    {
      cc_status.flags |= CC_REVERSED;
      return \"cmp %1,%0\";
    }
  return \"cmp %0,%1\";
}")

(define_expand "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "nonmemory_operand" "f,fG")
		 (match_operand:DF 1 "nonmemory_operand" "G,f")))]
  ""
  "emit_insn (gen_rtx (USE, VOIDmode, gen_rtx (REG, DFmode, 32)));")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:DF 0 "nonmemory_operand" "f,fG")
		 (match_operand:DF 1 "nonmemory_operand" "G,f")))]
  "GET_CODE (operands[0]) != CONST_INT && GET_CODE (operands[1]) != CONST_INT"
  "*
{
  if (GET_CODE (operands[0]) == CONST_DOUBLE
      || GET_CODE (operands[1]) == CONST_DOUBLE)
    make_f0_contain_0 (2);

  cc_status.flags |= CC_IN_FCCR;
  if (GET_CODE (operands[0]) == CONST_DOUBLE)
    return \"fcmped %%f0,%1\;nop\";
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    return \"fcmped %0,%%f0\;nop\";
  return \"fcmped %0,%1\;nop\";
}")

(define_expand "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "nonmemory_operand" "f,fG")
		 (match_operand:SF 1 "nonmemory_operand" "G,f")))]
  ""
  "emit_insn (gen_rtx (USE, VOIDmode, gen_rtx (REG, SFmode, 32)));")

(define_insn ""
  [(set (cc0)
	(compare (match_operand:SF 0 "nonmemory_operand" "f,fG")
		 (match_operand:SF 1 "nonmemory_operand" "G,f")))]
  "GET_CODE (operands[0]) != CONST_INT && GET_CODE (operands[1]) != CONST_INT"
  "*
{
  if (GET_CODE (operands[0]) == CONST_DOUBLE
      || GET_CODE (operands[1]) == CONST_DOUBLE)
    make_f0_contain_0 (1);

  cc_status.flags |= CC_IN_FCCR;
  if (GET_CODE (operands[0]) == CONST_DOUBLE)
    return \"fcmpes %%f0,%1\;nop\";
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    return \"fcmpes %0,%%f0\;nop\";
  return \"fcmpes %0,%1\;nop\";
}")

;; Put tstsi first among test insns so it matches a CONST_INT operand.

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "tst %0")

;; Need this to take a general operand because cse can make
;; a CONST which won't be in a register.
(define_insn ""
  [(set (cc0)
	(match_operand:SI 0 "immediate_operand" "i"))]
  ""
  "set %0,%%g1\;tst %%g1")

;; Optimize the case of following a reg-reg move with a test
;; of reg just moved.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "register_operand" "r"))
   (set (cc0) (match_operand:SI 2 "register_operand" "r"))]
  "operands[2] == operands[0]
   || operands[2] == operands[1]"
  "orcc %1,%%g0,%0 ! 2-insn combine")

;; Optimize 5(6) insn sequence to 3(4) insns.
;; These patterns could also optimize more complicated sets
;; before conditional branches.

;; Turned off because (1) this case is rarely encounted
;; (2) to be correct, more conditions must be checked
;; (3) the conditions must be checked with rtx_equal_p, not ==
;; (4) when branch scheduling is added to the compiler,
;;     this optimization will be performed by the branch scheduler
;; Bottom line: it is not worth the trouble of fixing or
;; maintaining it.

;(define_peephole
;  [(set (match_operand:SI 0 "register_operand" "=r")
;	(match_operand:SI 1 "general_operand" "g"))
;   (set (match_operand:SI 2 "register_operand" "=r")
;	(match_operand:SI 3 "reg_or_0_operand" "rJ"))
;   (set (cc0) (match_operand:SI 4 "register_operand" "r"))
;   (set (pc) (match_operand 5 "" ""))]
;  "GET_CODE (operands[5]) == IF_THEN_ELSE
;   && operands[0] != operands[3]
;   && ! reg_mentioned_p (operands[2], operands[1])
;   && (operands[4] == operands[0]
;       || operands[4] == operands[2]
;       || operands[4] == operands[3])"
;  "*
;{
;  rtx xoperands[2];
;  int parity;
;  xoperands[0] = XEXP (operands[5], 0);
;  if (GET_CODE (XEXP (operands[5], 1)) == PC)
;    {
;      parity = 1;
;      xoperands[1] = XEXP (XEXP (operands[5], 2), 0);
;    }
;  else
;    {
;      parity = 0;
;      xoperands[1] = XEXP (XEXP (operands[5], 1), 0);
;    }
;
;  if (operands[4] == operands[0])
;    {
;      /* Although the constraints for operands[1] permit a general
;	 operand (and hence possibly a const_int), we know that
;	 in this branch it cannot be a CONST_INT, since that would give
;	 us a fixed condition, and those should have been optimized away.  */
;      if (REG_P (operands[1]))
;	output_asm_insn (\"orcc %1,%%g0,%0 ! 3-insn reorder\", operands);
;      else if (GET_CODE (operands[1]) != MEM)
;	abort ();
;      else
;	{
;	  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
;	    output_asm_insn (\"sethi %%hi(%m1),%%g1\;ld [%%g1+%%lo(%m1)],%0\;tst %0 ! 4-insn reorder\", operands);
;	  else
;	    output_asm_insn (\"ld %1,%0\;tst %0 ! 3.5-insn reorder\", operands);
;	}
;      XVECEXP (PATTERN (insn), 0, 0) = XVECEXP (PATTERN (insn), 0, 2);
;      XVECEXP (PATTERN (insn), 0, 1) = XVECEXP (PATTERN (insn), 0, 3);
;    }
;  else
;    {
;      output_asm_insn (\"orcc %3,%%g0,%2 ! 3-insn reorder\", operands);
;    }
;  if (parity)
;    return output_delayed_branch (\"b%N0 %l1\", xoperands, insn);
;  else
;    return output_delayed_branch (\"b%C0 %l1\", xoperands, insn);
;}")

;; By default, operations don't set the condition codes.
;; These patterns allow cc's to be set, while doing some work

(define_insn ""
  [(set (cc0)
	(zero_extend:SI (subreg:QI (match_operand:SI 0 "register_operand" "r") 0)))]
  ""
  "andcc %0,0xff,%%g0")

(define_insn ""
  [(set (cc0)
	(plus:SI (match_operand:SI 0 "register_operand" "r%")
		 (match_operand:SI 1 "arith_operand" "rI")))]
  "ignore_overflow_conditional_p (NEXT_INSN (insn))"
  "*
{
  cc_status.flags |= CC_NO_OVERFLOW;
  return \"addcc %0,%1,%%g0\";
}")

(define_insn ""
  [(set (cc0)
	(plus:SI (match_operand:SI 0 "register_operand" "r%")
		 (match_operand:SI 1 "arith_operand" "rI")))
   (set (match_operand:SI 2 "register_operand" "=r")
	(plus:SI (match_dup 0) (match_dup 1)))]
  "ignore_overflow_conditional_p (NEXT_INSN (insn))"
  "*
{
  cc_status.flags |= CC_NO_OVERFLOW;
  return \"addcc %0,%1,%2\";
}")

(define_insn ""
  [(set (cc0)
	(minus:SI (match_operand:SI 0 "register_operand" "r")
		  (match_operand:SI 1 "arith_operand" "rI")))]
  "ignore_overflow_conditional_p (NEXT_INSN (insn))"
  "*
{
  cc_status.flags |= CC_NO_OVERFLOW;
  return \"subcc %0,%1,%%g0\";
}")

(define_insn ""
  [(set (cc0)
	(minus:SI (match_operand:SI 0 "register_operand" "r")
		  (match_operand:SI 1 "arith_operand" "rI")))
   (set (match_operand:SI 2 "register_operand" "=r")
	(minus:SI (match_dup 0) (match_dup 1)))]
  "ignore_overflow_conditional_p (NEXT_INSN (insn))"
  "*
{
  cc_status.flags |= CC_NO_OVERFLOW;
  return \"subcc %0,%1,%2\";
}")

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "register_operand" "r%")
		(match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "andcc %0,%1,%%g0")

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "register_operand" "r%")
		(match_operand:SI 1 "arith_operand" "rI")))
   (set (match_operand:SI 2 "register_operand" "=r")
	(and:SI (match_dup 0) (match_dup 1)))]
  ""
  "andcc %0,%1,%2")

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "register_operand" "r")
		(not:SI (match_operand:SI 1 "arith_operand" "rI"))))]
  ""
  "andncc %0,%1,%%g0")

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "register_operand" "r")
		(not:SI (match_operand:SI 1 "arith_operand" "rI"))))
   (set (match_operand:SI 2 "register_operand" "=r")
	(and:SI (match_dup 0) (not:SI (match_dup 1))))]
  ""
  "andncc %0,%1,%2")

(define_insn ""
  [(set (cc0)
	(ior:SI (match_operand:SI 0 "register_operand" "r%")
		(match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "orcc %0,%1,%%g0")

(define_insn ""
  [(set (cc0)
	(ior:SI (match_operand:SI 0 "register_operand" "r%")
		(match_operand:SI 1 "arith_operand" "rI")))
   (set (match_operand:SI 2 "register_operand" "=r")
	(ior:SI (match_dup 0) (match_dup 1)))]
  ""
  "orcc %0,%1,%2")

(define_insn ""
  [(set (cc0)
	(ior:SI (match_operand:SI 0 "register_operand" "r")
		(not:SI (match_operand:SI 1 "arith_operand" "rI"))))]
  ""
  "orncc %0,%1,%%g0")

(define_insn ""
  [(set (cc0)
	(ior:SI (match_operand:SI 0 "register_operand" "r")
		(not:SI (match_operand:SI 1 "arith_operand" "rI"))))
   (set (match_operand:SI 2 "register_operand" "=r")
	(ior:SI (match_dup 0) (not:SI (match_dup 1))))]
  ""
  "orncc %0,%1,%2")

(define_insn ""
  [(set (cc0)
	(xor:SI (match_operand:SI 0 "register_operand" "r%")
		(match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "xorcc %0,%1,%%g0")

(define_insn ""
  [(set (cc0)
	(xor:SI (match_operand:SI 0 "register_operand" "r%")
		(match_operand:SI 1 "arith_operand" "rI")))
   (set (match_operand:SI 2 "register_operand" "=r")
	(xor:SI (match_dup 0) (match_dup 1)))]
  ""
  "xorcc %0,%1,%2")

(define_insn ""
  [(set (cc0)
	(xor:SI (match_operand:SI 0 "register_operand" "r")
		(not:SI (match_operand:SI 1 "arith_operand" "rI"))))]
  ""
  "xnorcc %0,%1,%%g0")

(define_insn ""
  [(set (cc0)
	(xor:SI (match_operand:SI 0 "register_operand" "r")
		(not:SI (match_operand:SI 1 "arith_operand" "rI"))))
   (set (match_operand:SI 2 "register_operand" "=r")
	(xor:SI (match_dup 0) (not:SI (match_dup 1))))]
  ""
  "xnorcc %0,%1,%2")

(define_expand "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "register_operand" "f"))]
  ""
  "emit_insn (gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, DFmode, 32)));")

(define_insn ""
  [(set (cc0)
	(match_operand:DF 0 "register_operand" "f"))]
  ""
  "*
{
  make_f0_contain_0 (2);
  cc_status.flags |= CC_IN_FCCR;
  return \"fcmped %0,%%f0\;nop\";
}")

(define_expand "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "register_operand" "f"))]
  ""
  "emit_insn (gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SFmode, 32)));")

(define_insn ""
  [(set (cc0)
	(match_operand:SF 0 "register_operand" "f"))]
  ""
  "*
{
  make_f0_contain_0 (1);
  cc_status.flags |= CC_IN_FCCR;
  return \"fcmpes %0,%%f0\;nop\";
}")

;; There are no logical links for the condition codes.  This
;; would not normally be a problem, but on the SPARC (and possibly
;; other RISC machines), when argument passing, the insn which sets
;; the condition code and the insn which uses the set condition code
;; may not be performed adjacently (due to optimizations performed
;; in combine.c).  To make up for this, we emit insn patterns which
;; cannot possibly be rearranged on us.
(define_expand "seq"
  [(set (match_operand:SI 0 "general_operand" "=r")
	(eq (cc0) (const_int 0)))]
  ""
  "gen_scc_insn (EQ, VOIDmode, operands); DONE;")

(define_expand "sne"
  [(set (match_operand:SI 0 "general_operand" "=r")
	(ne (cc0) (const_int 0)))]
  ""
  "gen_scc_insn (NE, VOIDmode, operands); DONE;")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r,r")
	(match_operator 1 "eq_or_neq"
			[(compare (match_operand:SI 2 "general_operand" "r,rI")
				  (match_operand:SI 3 "general_operand" "I,r"))
			 (const_int 0)]))]
  ""
  "*
{
  CC_STATUS_INIT;
  cc_status.value1 = operands[0];
  if (! REG_P (operands[2]))
    {
      output_asm_insn (\"cmp %3,%2\", operands);
      cc_status.flags |= CC_REVERSED;
    }
  else
    output_asm_insn (\"cmp %2,%3\", operands);
  return output_scc_insn (GET_CODE (operands[1]), operands[0]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(match_operator 1 "eq_or_neq"
			[(match_operand:SI 2 "general_operand" "r")
			 (const_int 0)]))]
  ""
  "*
{
  CC_STATUS_INIT;
  cc_status.value1 = operands[0];
  output_asm_insn (\"tst %2\", operands);
  return output_scc_insn (GET_CODE (operands[1]), operands[0]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r,r")
	(match_operator 1 "eq_or_neq"
			[(compare (match_operand:DF 2 "general_operand" "f,fG")
				  (match_operand:DF 3 "general_operand" "G,f"))
			 (const_int 0)]))]
  ""
  "*
{
  CC_STATUS_INIT;
  cc_status.value1 = operands[0];
  cc_status.flags |= CC_IN_FCCR;

  if (GET_CODE (operands[2]) == CONST_DOUBLE
      || GET_CODE (operands[3]) == CONST_DOUBLE)
    make_f0_contain_0 (2);

  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    output_asm_insn (\"fcmped %%f0,%3\;nop\", operands);
  else if (GET_CODE (operands[3]) == CONST_DOUBLE)
    output_asm_insn (\"fcmped %2,%%f0\;nop\", operands);
  else output_asm_insn (\"fcmped %2,%3\;nop\", operands);
  return output_scc_insn (GET_CODE (operands[1]), operands[0]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(match_operator 1 "eq_or_neq"
			[(match_operand:DF 2 "general_operand" "f")
			 (const_int 0)]))]
  ""
  "*
{
  CC_STATUS_INIT;
  cc_status.value1 = operands[0];
  cc_status.flags |= CC_IN_FCCR;

  make_f0_contain_0 (2);
  output_asm_insn (\"fcmped %2,%%f0\;nop\", operands);
  return output_scc_insn (GET_CODE (operands[1]), operands[0]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r,r")
	(match_operator 1 "eq_or_neq"
			[(compare (match_operand:SF 2 "general_operand" "f,fG")
				  (match_operand:SF 3 "general_operand" "G,f"))
			 (const_int 0)]))]
  ""
  "*
{
  CC_STATUS_INIT;
  cc_status.value1 = operands[0];
  cc_status.flags |= CC_IN_FCCR;

  if (GET_CODE (operands[2]) == CONST_DOUBLE
      || GET_CODE (operands[3]) == CONST_DOUBLE)
    make_f0_contain_0 (1);

  if (GET_CODE (operands[2]) == CONST_DOUBLE)
    output_asm_insn (\"fcmpes %%f0,%3\;nop\", operands);
  else if (GET_CODE (operands[3]) == CONST_DOUBLE)
    output_asm_insn (\"fcmpes %2,%%f0\;nop\", operands);
  else output_asm_insn (\"fcmpes %2,%3\;nop\", operands);
  return output_scc_insn (GET_CODE (operands[1]), operands[0]);
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=r")
	(match_operator 1 "eq_or_neq"
			[(match_operand:SF 2 "general_operand" "f")
			 (const_int 0)]))]
  ""
  "*
{
  CC_STATUS_INIT;
  cc_status.value1 = operands[0];
  cc_status.flags |= CC_IN_FCCR;

  make_f0_contain_0 (1);
  output_asm_insn (\"fcmpes %2,%%f0\;nop\", operands);
  return output_scc_insn (GET_CODE (operands[1]), operands[0]);
}")

;; These control RTL generation for conditional jump insns
;; and match them for register allocation.

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"be %l0\;nop\", \"be %l0\;nop\", \"fbe %l0\;nop\");
}")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"bne %l0\;nop\", \"bne %l0\;nop\", \"fbne %l0\;nop\");
}")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"bg %l0\;nop\", 0, \"fbg %l0\;nop\");
}")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  if (cc_prev_status.flags & CC_IN_FCCR)
    abort ();
  return \"bgu %l0\;nop\";
}")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"bl %l0\;nop\", \"bneg %l0\;nop\", \"fbl %l0\;nop\");
}")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  if (cc_prev_status.flags & CC_IN_FCCR)
    abort ();
  return \"blu %l0\;nop\";
}")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"bge %l0\;nop\", \"bpos %l0\;nop\", \"fbge %l0\;nop\");
}")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  if (cc_prev_status.flags & CC_IN_FCCR)
    abort ();
  return \"bgeu %l0\;nop\";
}")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  OUTPUT_JUMP (\"ble %l0\;nop\", 0, \"fble %l0\;nop\");
}")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  if (cc_prev_status.flags & CC_IN_FCCR)
    abort ();
  return \"bleu %l0\;nop\";
}")

;; This matches inverted jump insns for register allocation.

(define_insn ""
  [(set (pc)
	(if_then_else (match_operator 0 "relop" [(cc0) (const_int 0)])
		      (pc)
		      (label_ref (match_operand 1 "" ""))))]
  ""
  "*
{
  if (cc_prev_status.flags & CC_NO_OVERFLOW)
    {
      if (GET_CODE (operands[0]) == GT || GET_CODE (operands[0]) == LE)
	/* These two conditions can't ignore overflow,
	   so reinsert the deleted test instruction.  */
	return 0;
      return \"b%U0 %l1\;nop\";
    }
  if (cc_prev_status.flags & CC_IN_FCCR)
    return \"fb%F0 %l1\;nop\";
  return \"b%N0 %l1\;nop\";
}")

;; Move instructions

(define_insn "swapsi"
  [(set (match_operand:SI 0 "general_operand" "r,rm")
	(match_operand:SI 1 "general_operand" "m,r"))
   (set (match_dup 1) (match_dup 0))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	output_asm_insn (\"set %a1,%%g1\", operands),
	operands[1] = gen_rtx (MEM, SImode, gen_rtx (REG, SImode, 1)),
	cc_status.flags &= ~CC_KNOW_HI_G1;
      output_asm_insn (\"swap %1,%0\", operands);
    }
  if (REG_P (operands[0]))
    {
      if (REGNO (operands[0]) == REGNO (operands[1]))
	return \"\";
      return \"xor %0,%1,%0\;xor %1,%0,%1\;xor %0,%1,%0\";
    }
  if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
    {
      output_asm_insn (\"set %a0,%%g1\", operands);
      operands[0] = gen_rtx (MEM, SImode, gen_rtx (REG, SImode, 1));
      cc_status.flags &= ~CC_KNOW_HI_G1;
    }
  return \"swap %0,%1\";
}")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=r,m")
	(match_operand:SI 1 "general_operand" "rmif,rJ"))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	return output_store (operands);
      return \"st %r1,%0\";
    }
  if (GET_CODE (operands[1]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	return output_load_fixed (operands);
      return \"ld %1,%0\";
    }
  if (FP_REG_P (operands[1]))
    return \"st %r1,[%%fp-4]\;ld [%%fp-4],%0\";
  if (REG_P (operands[1])
      || (GET_CODE (operands[1]) == CONST_INT
	  && SMALL_INT (operands[1])))
    return \"mov %1,%0\";
  if (GET_CODE (operands[1]) == CONST_INT
      && (INTVAL (operands[1]) & 0x3ff) == 0)
    return \"sethi %%hi(%1),%0\";
  return \"sethi %%hi(%1),%0\;or %%lo(%1),%0,%0\";
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=r,m")
	(match_operand:HI 1 "general_operand" "rmi,rJ"))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	return output_store (operands);
      return \"sth %r1,%0\";
    }
  if (GET_CODE (operands[1]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	return output_load_fixed (operands);
      return \"ldsh %1,%0\";
    }
  if (REG_P (operands[1])
      || (GET_CODE (operands[1]) == CONST_INT
	  && SMALL_INT (operands[1])))
    return \"mov %1,%0\";
  return \"sethi %%hi(%1),%0\;or %%lo(%1),%0,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=r,m")
	(match_operand:QI 1 "general_operand" "rmi,rJ"))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	return output_store (operands);
      return \"stb %r1,%0\";
    }
  if (GET_CODE (operands[1]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	return output_load_fixed (operands);
      return \"ldsb %1,%0\";
    }
  if (REG_P (operands[1])
      || (GET_CODE (operands[1]) == CONST_INT
	  && SMALL_INT (operands[1])))
    return \"mov %1,%0\";
  return \"sethi %%hi(%1),%0\;or %%lo(%1),%0,%0\";
}")

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it won't successfully combine with anything.
(define_expand "movstrsi"
  [(parallel [(set (mem:BLK (match_operand:BLK 0 "general_operand" ""))
		   (mem:BLK (match_operand:BLK 1 "general_operand" "")))
	      (use (match_operand:SI 2 "arith32_operand" ""))
	      (use (match_operand:SI 3 "immediate_operand" ""))
	      (clobber (match_dup 4))
	      (clobber (match_dup 0))
	      (clobber (match_dup 1))])]
  ""
  "
{
  operands[0] = copy_to_mode_reg (SImode, XEXP (operands[0], 0));
  operands[1] = copy_to_mode_reg (SImode, XEXP (operands[1], 0));
  operands[4] = gen_reg_rtx (SImode);
}")

(define_insn ""
  [(set (mem:BLK (match_operand:SI 0 "register_operand" "r"))
	(mem:BLK (match_operand:SI 1 "register_operand" "r")))
   (use (match_operand:SI 2 "arith32_operand" "rn"))
   (use (match_operand:SI 3 "immediate_operand" "i"))
   (clobber (match_operand:SI 4 "register_operand" "=r"))
   (clobber (match_operand:SI 5 "register_operand" "=0"))
   (clobber (match_operand:SI 6 "register_operand" "=1"))]
  ""
  "* return output_block_move (operands);")

;; Floating point move insns

;; This pattern forces (set (reg:DF ...) (const_double ...))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general movdf pattern.
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=r,f,o")
	(match_operand:DF 1 "" "mG,m,G"))]
  "GET_CODE (operands[1]) == CONST_DOUBLE"
  "*
{
  if (FP_REG_P (operands[0]))
    return output_fp_move_double (operands);
  if (operands[1] == dconst0_rtx && GET_CODE (operands[0]) == REG)
    {
      operands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
      return \"mov %%g0,%0\;mov %%g0,%1\";
    }
  if (operands[1] == dconst0_rtx && GET_CODE (operands[0]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && XEXP (operands[0], 0) == cc_prev_status.mdep))
	    {
	      cc_status.flags |= CC_KNOW_HI_G1;
	      cc_status.mdep = XEXP (operands[0], 0);
	      output_asm_insn (\"sethi %%hi(%m0),%%g1\", operands);
	    }
	  return \"st %%g0,[%%g1+%%lo(%%m0)]\;st %%g0,[%%g1+%%lo(%%m0)+4]\";
	}
      operands[1] = adj_offsettable_operand (operands[0], 4);
      return \"st %%g0,%0\;st %%g0,%1\";
    }
  return output_move_double (operands);
}")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=rm,&r,?f,?rm")
	(match_operand:DF 1 "general_operand" "r,m,rfm,f"))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM
      && CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
    return output_store (operands);
  if (GET_CODE (operands[1]) == MEM
      && CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    return output_load_floating (operands);

  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=rm,&r,?f,?rm")
	(match_operand:DI 1 "general_operand" "r,mi,rfm,f"))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM
      && CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
    return output_store (operands);
  if (GET_CODE (operands[1]) == MEM
      && CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    return output_load_fixed (operands);

  if (FP_REG_P (operands[0]) || FP_REG_P (operands[1]))
    return output_fp_move_double (operands);
  return output_move_double (operands);
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=rf,m")
	(match_operand:SF 1 "general_operand" "rfm,rf"))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM
      && CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
    return output_store (operands);
  if (GET_CODE (operands[1]) == MEM
      && CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    return output_load_floating (operands);
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	return \"fmovs %1,%0\";
      if (GET_CODE (operands[1]) == REG)
        return \"st %r1,[%%fp-4]\;ld [%%fp-4],%0\";
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	{
	  cc_status.flags |= CC_KNOW_HI_G1;
	  cc_status.mdep = XEXP (operands[1], 0);
	  return \"sethi %%hi(%m1),%%g1\;ld [%%g1+%%lo(%m1)],%0\";
	}
      return \"ld %1,%0\";
    }
  if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	return \"st %r1,[%%fp-4]\;ld [%%fp-4],%0\";
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && XEXP (operands[0], 0) == cc_prev_status.mdep))
	    {
	      cc_status.flags |= CC_KNOW_HI_G1;
	      cc_status.mdep = XEXP (operands[0], 0);
	      output_asm_insn (\"sethi %%hi(%m0),%%g1\", operands);
	    }
	  return \"st %r1,[%%g1+%%lo(%m0)]\";
	}
      return \"st %r1,%0\";
    }
  if (GET_CODE (operands[0]) == MEM)
    return \"st %r1,%0\";
  if (GET_CODE (operands[1]) == MEM)
    return \"ld %1,%0\";
  return \"mov %1,%0\";
}")

;;- truncation instructions
(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI
	 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM)
    if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
      {
	if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	       && XEXP (operands[0], 0) == cc_prev_status.mdep))
	  {
	    cc_status.flags |= CC_KNOW_HI_G1;
	    cc_status.mdep = XEXP (operands[0], 0);
	    output_asm_insn (\"sethi %%hi(%m0),%%g1\", operands);
	  }
	return \"stb %1,[%%g1+%%lo(%m0)]\";
      }
    else
      return \"stb %1,%0\";
  return \"mov %1,%0\";
}")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI
	 (match_operand:HI 1 "register_operand" "r")))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM)
    if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
      {
	if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	       && XEXP (operands[0], 0) == cc_prev_status.mdep))
	  {
	    cc_status.flags |= CC_KNOW_HI_G1;
	    cc_status.mdep = XEXP (operands[0], 0);
	    output_asm_insn (\"sethi %%hi(%m0),%%g1\", operands);
	  }
	return \"stb %1,[%%g1+%%lo(%m0)]\";
      }
    else
      return \"stb %1,%0\";
  return \"mov %1,%0\";
}")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(truncate:HI
	 (match_operand:SI 1 "register_operand" "r")))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == MEM)
    if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
      {
	if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	       && XEXP (operands[0], 0) == cc_prev_status.mdep))
	  {
	    cc_status.flags |= CC_KNOW_HI_G1;
	    cc_status.mdep = XEXP (operands[0], 0);
	    output_asm_insn (\"sethi %%hi(%m0),%%g1\", operands);
	  }
	return \"sth %1,[%%g1+%%lo(%m0)]\";
      }
    else
      return \"sth %1,%0\";
  return \"mov %1,%0\";
}")

;;- zero extension instructions

;; Note that the one starting from HImode comes before those for QImode
;; so that a constant operand will match HImode, not QImode.

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI
	 (match_operand:HI 1 "general_operand" "g")))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"sll %1,0x10,%0\;srl %0,0x10,%0\";
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     INTVAL (operands[1]) & 0xffff);
      output_asm_insn (\"set %1,%0\", operands);
      return \"\";
    }
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;lduh [%%g1+%%lo(%m1)],%0\";
    }
  else
    return \"lduh %1,%0\";
}")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI
	 (match_operand:QI 1 "general_operand" "g")))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"and %1,0xff,%0\";
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     INTVAL (operands[1]) & 0xff);
      output_asm_insn (\"set %1,%0\", operands);
      return \"\";
    }
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ldub [%%g1+%%lo(%m1)],%0\";
    }
  else
    return \"ldub %1,%0\";
}")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI
	 (match_operand:QI 1 "general_operand" "g")))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"and %1,0xff,%0\";
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     INTVAL (operands[1]) & 0xff);
      output_asm_insn (\"set %1,%0\", operands);
      return \"\";
    }
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ldub [%%g1+%%lo(%m1)],%0\";
    }
  else
    return \"ldub %1,%0\";
}")

;;- sign extension instructions
;; Note that the one starting from HImode comes before those for QImode
;; so that a constant operand will match HImode, not QImode.

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI
	 (match_operand:HI 1 "general_operand" "g")))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"sll %1,0x10,%0\;sra %0,0x10,%0\";
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = (short)INTVAL (operands[1]);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, i);
      output_asm_insn (\"set %1,%0\", operands);
      return \"\";
    }
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ldsh [%%g1+%%lo(%m1)],%0\";
    }
  else
    return \"ldsh %1,%0\";
}")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI
	 (match_operand:QI 1 "general_operand" "g")))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"sll %1,0x18,%0\;sra %0,0x18,%0\";
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = (char)INTVAL (operands[1]);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, i);
      output_asm_insn (\"set %1,%0\", operands);
      return \"\";
    }
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ldsb [%%g1+%%lo(%m1)],%0\";
    }
  else
    return \"ldsb %1,%0\";
}")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(sign_extend:SI
	 (match_operand:QI 1 "general_operand" "g")))]
  ""
  "*
{
  if (REG_P (operands[1]))
    return \"sll %1,0x18,%0\;sra %0,0x18,%0\";
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      int i = (char)INTVAL (operands[1]);
      operands[1] = gen_rtx (CONST_INT, VOIDmode, i);
      output_asm_insn (\"set %1,%0\", operands);
      return \"\";
    }
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ldsb [%%g1+%%lo(%m1)],%0\";
    }
  else
    return \"ldsb %1,%0\";
}")

;; Signed bitfield extractions come out looking like
;;	(shiftrt (shift (sign_extend <Y>) <C1>) <C2>)
;; which we expand poorly as four shift insns.
;; These patters yeild two shifts:
;;	(shiftrt (shift <Y> <C3>) <C4>)
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI
	 (sign_extend:SI
	  (match_operand:QI 1 "register_operand" "r"))
	 (match_operand:SI 2 "small_int" "n")))]
  ""
  "sll %1,0x18,%0\;sra %0,0x18+%2,%0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI
	 (sign_extend:SI
	  (subreg:QI (ashift:SI (match_operand:SI 1 "register_operand" "r")
				(match_operand:SI 2 "small_int" "n")) 0))
	 (match_operand:SI 3 "small_int" "n")))]
  ""
  "sll %1,0x18+%2,%0\;sra %0,0x18+%3,%0")

;; Special patterns for optimizing bit-field instructions.

;; First two patterns are for bitfields that came from memory
;; testing only the high bit.  They work with old combiner.
;; @@ Actually, the second pattern does not work if we
;; @@ need to set the N bit.
(define_insn ""
  [(set (cc0)
	(zero_extend:SI (subreg:QI (lshiftrt:SI (match_operand:SI 0 "register_operand" "r")
						(const_int 7)) 0)))]
  "0"
  "andcc %0,128,%%g0")

(define_insn ""
  [(set (cc0)
	(sign_extend:SI (subreg:QI (ashiftrt:SI (match_operand:SI 0 "register_operand" "r")
						(const_int 7)) 0)))]
  "0"
  "andcc %0,128,%%g0")

;; next two patterns are good for bitfields coming from memory
;; (via pseudo-register) or from a register, though this optimization
;; is only good for values contained wholly within the bottom 13 bits
(define_insn ""
  [(set (cc0)
	(and:SI (lshiftrt:SI (match_operand:SI 0 "register_operand" "r")
			     (match_operand:SI 1 "small_int" "n"))
		(match_operand:SI 2 "small_int" "n")))]
  "(unsigned)((INTVAL (operands[2]) << INTVAL (operands[1])) + 0x1000) < 0x2000"
  "andcc %0,%2<<%1,%%g0")

(define_insn ""
  [(set (cc0)
	(and:SI (ashiftrt:SI (match_operand:SI 0 "register_operand" "r")
			     (match_operand:SI 1 "small_int" "n"))
		(match_operand:SI 2 "small_int" "n")))]
  "(unsigned)((INTVAL (operands[2]) << INTVAL (operands[1])) + 0x1000) < 0x2000"
  "andcc %0,%2<<%1,%%g0")

;; Conversions between float and double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF
	 (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fstod %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "register_operand" "f")))]
  ""
  "fdtos %1,%0")

;; Conversion between fixed point and floating point.
;; Note that among the fix-to-float insns
;; the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.

;; This pattern forces (set (reg:SF ...) (float:SF (const_int ...)))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general floatsisf2 pattern.
(define_insn ""
  [(set (match_operand:SF 0 "general_operand" "=f")
	(float:SF (match_operand 1 "" "m")))]
  "GET_CODE (operands[1]) == CONST_INT"
  "*
{
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ld [%%g1+%%lo(%m1)],%0\;fitos %0,%0\";
    }
  return \"ld %1,%0\;fitos %0,%0\";
}")

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=f")
	(float:SF (match_operand:SI 1 "general_operand" "rfm")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == MEM)
    if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
      {
	cc_status.flags |= CC_KNOW_HI_G1;
	cc_status.mdep = XEXP (operands[1], 0);
	return \"sethi %%hi(%m1),%%g1\;ld [%%g1+%%lo(%m1)],%0\;fitos %0,%0\";
      }
    else
      return \"ld %1,%0\;fitos %0,%0\";
  else if (FP_REG_P (operands[1]))
    return \"fitos %1,%0\";
  return \"st %r1,[%%fp-4]\;ld [%%fp-4],%0\;fitos %0,%0\";
}")

;; This pattern forces (set (reg:DF ...) (float:DF (const_int ...)))
;; to be reloaded by putting the constant into memory.
;; It must come before the more general floatsidf2 pattern.
(define_insn ""
  [(set (match_operand:DF 0 "general_operand" "=f")
	(float:DF (match_operand 1 "" "m")))]
  "GET_CODE (operands[1]) == CONST_INT"
  "*
{
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ld [%%g1+%%lo(%m1)],%0\;fitod %0,%0\";
    }
  return \"ld %1,%0\;fitod %0,%0\";
}")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=f")
	(float:DF (match_operand:SI 1 "general_operand" "rfm")))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == MEM)
    if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
      {
	cc_status.flags |= CC_KNOW_HI_G1;
	cc_status.mdep = XEXP (operands[1], 0);
	return \"sethi %%hi(%m1),%%g1\;ld [%%g1+%%lo(%m1)],%0\;fitod %0,%0\";
      }
    else
      return \"ld %1,%0\;fitod %0,%0\";
  else if (FP_REG_P (operands[1]))
    return \"fitod %1,%0\";
  else
    return \"st %r1,[%%fp-4]\;ld [%%fp-4],%0\;fitod %0,%0\";
}")

;; Convert a float to an actual integer.
;; Truncation is performed as part of the conversion.
(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(fix:SI (fix:SF (match_operand:SF 1 "general_operand" "fm"))))]
  ""
  "*
{
  cc_status.flags &= ~(CC_F1_IS_0);
  if (FP_REG_P (operands[1]))
    output_asm_insn (\"fstoi %1,%%f1\", operands);
  else if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      output_asm_insn (\"sethi %%hi(%m1),%%g1\;ld [%%g1+%%lo(%m1)],%%f1\;fstoi %%f1,%%f1\", operands);
    }
  else
    output_asm_insn (\"ld %1,%%f1\;fstoi %%f1,%%f1\", operands);
  if (GET_CODE (operands[0]) == MEM)
    if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
      {
	if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	       && XEXP (operands[0], 0) == cc_prev_status.mdep))
	  {
	    cc_status.flags |= CC_KNOW_HI_G1;
	    cc_status.mdep = XEXP (operands[0], 0);
	    output_asm_insn (\"sethi %%hi(%m0),%%g1\", operands);
	  }
	return \"st %%f1,[%%g1+%%lo(%m0)]\";
      }
    else
      return \"st %%f1,%0\";
  else
    return \"st %%f1,[%%fp-4]\;ld [%%fp-4],%0\";
}")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=rm")
	(fix:SI (fix:DF (match_operand:DF 1 "general_operand" "fm"))))]
  ""
  "*
{
  cc_status.flags &= ~CC_F0_IS_0;
  if (FP_REG_P (operands[1]))
    output_asm_insn (\"fdtoi %1,%%f0\", operands);
  else
    {
      rtx xoperands[2];
      xoperands[0] = gen_rtx (REG, DFmode, 32);
      xoperands[1] = operands[1];
      output_asm_insn (output_fp_move_double (xoperands), xoperands);
      output_asm_insn (\"fdtoi %%f0,%%f0\", 0);
    }
  if (GET_CODE (operands[0]) == MEM)
    if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
      {
	if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	       && XEXP (operands[0], 0) == cc_prev_status.mdep))
	  {
	    cc_status.flags |= CC_KNOW_HI_G1;
	    cc_status.mdep = XEXP (operands[0], 0);
	    output_asm_insn (\"sethi %%hi(%m0),%%g1\", operands);
	  }
	return \"st %%f0,[%%g1+%%lo(%m0)]\";
      }
    else
      return \"st %%f0,%0\";
  else
    return \"st %%f0,[%%fp-4]\;ld [%%fp-4],%0\";
}")

;;- arithmetic instructions

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith32_operand" "%r")
		 (match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (REG_P (operands[2]))
    return \"add %1,%2,%0\";
  if (SMALL_INT (operands[2]))
    return \"add %1,%2,%0\";
  cc_status.flags &= ~CC_KNOW_HI_G1;
  return \"sethi %%hi(%2),%%g1\;or %%lo(%2),%%g1,%%g1\;add %1,%%g1,%0\";
}")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (REG_P (operands[2]))
    return \"sub %1,%2,%0\";
  if (SMALL_INT (operands[2]))
    return \"sub %1,%2,%0\";
  cc_status.flags &= ~CC_KNOW_HI_G1;
  return \"sethi %%hi(%2),%%g1\;or %%lo(%2),%%g1,%%g1\;sub %1,%%g1,%0\";
}")

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "r")
	(mult:SI (match_operand:SI 1 "general_operand" "")
		 (match_operand:SI 2 "general_operand" "")))]
  ""
  "
{
  rtx src;

  if (GET_CODE (operands[1]) == CONST_INT)
    if (GET_CODE (operands[2]) == CONST_INT)
      {
	emit_move_insn (operands[0],
			gen_rtx (CONST_INT, VOIDmode,
				 INTVAL (operands[1]) * INTVAL (operands[2])));
	DONE;
      }
    else
      src = gen_rtx (MULT, SImode,
		     copy_to_mode_reg (SImode, operands[2]),
		     operands[1]);
  else if (GET_CODE (operands[2]) == CONST_INT)
    src = gen_rtx (MULT, SImode,
		   copy_to_mode_reg (SImode, operands[1]),
		   operands[2]);
  else src = 0;

  if (src)
    emit_insn (gen_rtx (SET, VOIDmode, operands[0], src));
  else
    emit_insn (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (5,
	       gen_rtx (SET, VOIDmode, operands[0],
			gen_rtx (MULT, SImode, operands[1], operands[2])),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 8)),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 9)),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 12)),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 13)))));
  DONE;
}")

(define_expand "umulsi3"
  [(set (match_operand:SI 0 "register_operand" "r")
	(umult:SI (match_operand:SI 1 "general_operand" "")
		  (match_operand:SI 2 "general_operand" "")))]
  ""
  "
{
  rtx src;

  if (GET_CODE (operands[1]) == CONST_INT)
    if (GET_CODE (operands[2]) == CONST_INT)
      {
	emit_move_insn (operands[0],
			gen_rtx (CONST_INT, VOIDmode,
				 (unsigned)INTVAL (operands[1]) * (unsigned)INTVAL (operands[2])));
	DONE;
      }
    else
      src = gen_rtx (UMULT, SImode,
		     copy_to_mode_reg (SImode, operands[2]),
		     operands[1]);
  else if (GET_CODE (operands[2]) == CONST_INT)
    src = gen_rtx (UMULT, SImode,
		   copy_to_mode_reg (SImode, operands[1]),
		   operands[2]);
  else src = 0;

  if (src)
    emit_insn (gen_rtx (SET, VOIDmode, operands[0], src));
  else
    emit_insn (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (5,
	       gen_rtx (SET, VOIDmode, operands[0],
			gen_rtx (UMULT, SImode, operands[1], operands[2])),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 8)),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 9)),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 12)),
	       gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, SImode, 13)))));
  DONE;
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "immediate_operand" "n")))]
  ""
  "* return output_mul_by_constant (insn, operands, 0);")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(umult:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "immediate_operand" "n")))]
  ""
  "* return output_mul_by_constant (insn, operands, 1);")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "general_operand" "%r")
		 (match_operand:SI 2 "general_operand" "r")))
   (clobber (reg:SI 8))
   (clobber (reg:SI 9))
   (clobber (reg:SI 12))
   (clobber (reg:SI 13))]
  ""
  "* return output_mul_insn (operands, 0);")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(umult:SI (match_operand:SI 1 "general_operand" "%r")
		  (match_operand:SI 2 "general_operand" "r")))
   (clobber (reg:SI 8))
   (clobber (reg:SI 9))
   (clobber (reg:SI 12))
   (clobber (reg:SI 13))]
  ""
  "* return output_mul_insn (operands, 1);")

;; this pattern is needed because cse may eliminate the multiplication,
;; but leave the clobbers behind.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "general_operand" "g"))
   (clobber (reg:SI 8))
   (clobber (reg:SI 9))
   (clobber (reg:SI 12))
   (clobber (reg:SI 13))]
  ""
  "*
{
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (SMALL_INT (operands[1]))
	return \"mov %1,%0\";
      return \"sethi %%hi(%1),%0\;or %%lo(%1),%0,%0\";
    }
  if (GET_CODE (operands[1]) == MEM)
    return \"ld %1,%0\";
  return \"mov %1,%0\";
}")

;; In case constant factor turns out to be -1.
(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "general_operand" "rI")))
   (clobber (reg:SI 8))
   (clobber (reg:SI 9))
   (clobber (reg:SI 12))
   (clobber (reg:SI 13))]
  ""
  "sub %%g0,%1,%0")

;;- and instructions (with compliment also)			   
(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "arith32_operand" "%r")
		(match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (REG_P (operands[2]) || SMALL_INT (operands[2]))
    return \"and %1,%2,%0\";
  cc_status.flags &= ~CC_KNOW_HI_G1;
  return \"sethi %%hi(%2),%%g1\;or %%lo(%2),%%g1,%%g1\;and %1,%%g1,%0\";
}")

(define_insn "andcbsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "register_operand" "r")
		(not:SI (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "andn %1,%2,%0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "arith32_operand" "%r")
		(match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (REG_P (operands[2]) || SMALL_INT (operands[2]))
    return \"or %1,%2,%0\";
  cc_status.flags &= ~CC_KNOW_HI_G1;
  return \"sethi %%hi(%2),%%g1\;or %%lo(%2),%%g1,%%g1\;or %1,%%g1,%0\";
}")

(define_insn "iorcbsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "register_operand" "r")
		(not:SI (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "orn %1,%2,%0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "arith32_operand" "%r")
		(match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (REG_P (operands[2]) || SMALL_INT (operands[2]))
    return \"xor %1,%2,%0\";
  cc_status.flags &= ~CC_KNOW_HI_G1;
  return \"sethi %%hi(%2),%%g1\;or %%lo(%2),%%g1,%%g1\;xor %1,%%g1,%0\";
}")

(define_insn "xorcbsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "register_operand" "r")
		(not:SI (match_operand:SI 2 "register_operand" "r"))))]
  ""
  "xnor %1,%2,%0")

;; We cannot use the "neg" pseudo insn because the Sun assembler
;; does not know how to make it work for constants.
(define_insn "negsi2"
  [(set (match_operand:SI 0 "general_operand" "=r")
	(neg:SI (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "sub %%g0,%1,%0")

;; We cannot use the "not" pseudo insn because the Sun assembler
;; does not know how to make it work for constants.
(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "general_operand" "=r")
	(not:SI (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "xnor %%g0,%1,%0")

;; Floating point arithmetic instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  ""
  "faddd %1,%2,%0")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  ""
  "fadds %1,%2,%0")

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "register_operand" "f")
		  (match_operand:DF 2 "register_operand" "f")))]
  ""
  "fsubd %1,%2,%0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  ""
  "fsubs %1,%2,%0")

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  ""
  "fmuld %1,%2,%0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  ""
  "fmuls %1,%2,%0")

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "register_operand" "f")
		(match_operand:DF 2 "register_operand" "f")))]
  ""
  "fdivd %1,%2,%0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  ""
  "fdivs %1,%2,%0")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  output_asm_insn (\"fnegs %1,%0\", operands);
  if (REGNO (operands[0]) != REGNO (operands[1]))
    {
      operands[0] = gen_rtx (REG, VOIDmode, REGNO (operands[0]) + 1);
      operands[1] = gen_rtx (REG, VOIDmode, REGNO (operands[1]) + 1);
      output_asm_insn (\"fmovs %1,%0\", operands);
    }
  return \"\";
}")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fnegs %1,%0")

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
{
  output_asm_insn (\"fabss %1,%0\", operands);
  if (REGNO (operands[0]) != REGNO (operands[1]))
    {
      operands[0] = gen_rtx (REG, VOIDmode, REGNO (operands[0]) + 1);
      operands[1] = gen_rtx (REG, VOIDmode, REGNO (operands[1]) + 1);
      output_asm_insn (\"fmovs %1,%0\", operands);
    }
  return \"\";
}")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "fabss %1,%0")

;; Shift instructions

;; Optimized special case of shifting.
;; Must precede the general case.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 24)))]
  ""
  "*
{
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ldsb [%%g1+%%lo(%m1)],%0\";
    }
  return \"ldsb %1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "memory_operand" "m")
		     (const_int 24)))]
  ""
  "*
{
  if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
    {
      cc_status.flags |= CC_KNOW_HI_G1;
      cc_status.mdep = XEXP (operands[1], 0);
      return \"sethi %%hi(%m1),%%g1\;ldub [%%g1+%%lo(%m1)],%0\";
    }
  return \"ldub %1,%0\";
}")

;;- arithmetic shift instructions
(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >= 32)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 31);
  return \"sll %1,%2,%0\";
}")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >= 32)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 31);
  return \"sra %1,%2,%0\";
}")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith32_operand" "rn")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) >= 32)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[2]) & 31);
  return \"srl %1,%2,%0\";
}")

;; Unconditional and other jump instructions
;; Note that for the Sparc, by setting the annul bit on an unconditional
;; branch, the following insn is never executed.  This saves us a nop,
;; but requires a debugger which can handle annuled branches.
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  extern int optimize;
  extern int flag_no_peephole;

  if (optimize && !flag_no_peephole)
    return \"b,a %l0\";
  return \"b %l0\;nop\";
}")

;; Peephole optimizers recognize a few simple cases when delay insns are safe.
;; Complex ones are up front.  Simple ones after.

;; This pattern is just like the following one, but matches when there
;; is a jump insn after the "delay" insn.  Without this pattern, we
;; de-optimize that case.

(define_peephole
  [(set (pc) (match_operand 0 "" ""))
   (set (match_operand:SI 1 "" "")
	(match_operand:SI 2 "" ""))
   (set (pc) (label_ref (match_operand 3 "" "")))]
  "TARGET_EAGER && operands_satisfy_eager_branch_peephole (operands, 2)"
  "*
{
  rtx xoperands[2];
  rtx pat = gen_rtx (SET, VOIDmode, operands[1], operands[2]);
  rtx delay_insn = gen_rtx (INSN, VOIDmode, 0, 0, 0, pat, -1, 0, 0);
  rtx label, head;
  int parity;

  if (GET_CODE (XEXP (operands[0], 1)) == PC)
    {
      parity = 1;
      label = XEXP (XEXP (operands[0], 2), 0);
    }
  else
    {
      parity = 0;
      label = XEXP (XEXP (operands[0], 1), 0);
    }
  xoperands[0] = XEXP (operands[0], 0);
  xoperands[1] = label;

  head = next_real_insn_no_labels (label);

  /* If at the target of this label we set the condition codes,
     and the condition codes are already set for that value,
     advance, if we can, to the following insn.  */
  if (GET_CODE (PATTERN (head)) == SET
      && GET_CODE (SET_DEST (PATTERN (head))) == CC0
      && cc_status.value2 == SET_SRC (PATTERN (head)))
    {
      rtx nhead = next_real_insn_no_labels (head);
      if (nhead
	  && GET_CODE (nhead) == INSN
	  && GET_CODE (PATTERN (nhead)) == SET
	  && strict_single_insn_op_p (SET_SRC (PATTERN (nhead)),
				      GET_MODE (SET_DEST (PATTERN (nhead))))
	  && strict_single_insn_op_p (SET_DEST (PATTERN (nhead)), VOIDmode)
	  /* Moves between FP regs and CPU regs are two insns.  */
	  && !(GET_CODE (SET_SRC (PATTERN (nhead))) == REG
	       && GET_CODE (SET_DEST (PATTERN (nhead))) == REG
	       && (FP_REG_P (SET_SRC (PATTERN (nhead)))
		   != FP_REG_P (SET_DEST (PATTERN (nhead))))))
	{
	  head = nhead;
	}
    }

  /* Output the branch instruction first.  */
  if (cc_prev_status.flags & CC_IN_FCCR)
    {
      if (parity)
	output_asm_insn (\"fb%F0,a %l1 ! eager\", xoperands);
      else
	output_asm_insn (\"fb%C0,a %l1 ! eager\", xoperands);
    }
  else if (cc_prev_status.flags & CC_NO_OVERFLOW)
    {
      if (parity)
	output_asm_insn (\"b%U0,a %l1 ! eager\", xoperands);
      else
	output_asm_insn (\"b%I0,a %l1 ! eager\", xoperands);
    }
  else
    {
      if (parity)
	output_asm_insn (\"b%N0,a %l1 ! eager\", xoperands);
      else
	output_asm_insn (\"b%C0,a %l1 ! eager\", xoperands);
    }

  /* Now steal the first insn of the target.  */
  output_eager_then_insn (head, operands);

  XVECEXP (PATTERN (insn), 0, 0) = XVECEXP (PATTERN (insn), 0, 1);
  XVECEXP (PATTERN (insn), 0, 1) = XVECEXP (PATTERN (insn), 0, 2);

  return output_delayed_branch (\"b %l3 ! eager2\", operands, insn);
}")

;; Here is a peephole which recognizes where delay insns can be made safe:
;; (1) following a conditional branch, if the target of the conditional branch
;; has only one user (this insn), move the first insn into our delay slot
;; and emit an annulled branch.
;; (2) following a conditional branch, if we can execute the fall-through
;; insn without risking any evil effects, then do that instead of a nop.

(define_peephole
  [(set (pc) (match_operand 0 "" ""))
   (set (match_operand:SI 1 "" "")
	(match_operand:SI 2 "" ""))]
  "TARGET_EAGER && operands_satisfy_eager_branch_peephole (operands, 1)"
  "*
{
  rtx xoperands[2];
  rtx pat = gen_rtx (SET, VOIDmode, operands[1], operands[2]);
  rtx delay_insn = gen_rtx (INSN, VOIDmode, 0, 0, 0, pat, -1, 0, 0);
  rtx label, head, prev = (rtx)1;
  int parity;

  if (GET_CODE (XEXP (operands[0], 1)) == PC)
    {
      parity = 1;
      label = XEXP (XEXP (operands[0], 2), 0);
    }
  else
    {
      parity = 0;
      label = XEXP (XEXP (operands[0], 1), 0);
    }
  xoperands[0] = XEXP (operands[0], 0);
  xoperands[1] = label;

  if (LABEL_NUSES (label) == 1)
    {
      prev = PREV_INSN (label);
      while (prev
	     && (GET_CODE (prev) == NOTE
		 || (GET_CODE (prev) == INSN
		     && (GET_CODE (PATTERN (prev)) == CLOBBER
			 || GET_CODE (PATTERN (prev)) == USE))))
	prev = PREV_INSN (prev);
      if (prev == 0
	  || GET_CODE (prev) == BARRIER)
	{
	  prev = 0;
	  head = next_real_insn_no_labels (label);
	}
    }
  if (prev == 0
      && head != 0
      && ! INSN_DELETED_P (head)
      && GET_CODE (head) == INSN
      && GET_CODE (PATTERN (head)) == SET
      && strict_single_insn_op_p (SET_SRC (PATTERN (head)),
				  GET_MODE (SET_DEST (PATTERN (head))))
      && strict_single_insn_op_p (SET_DEST (PATTERN (head)), VOIDmode)
      /* Moves between FP regs and CPU regs are two insns.  */
      && !(GET_CODE (SET_SRC (PATTERN (head))) == REG
	   && GET_CODE (SET_DEST (PATTERN (head))) == REG
	   && (FP_REG_P (SET_SRC (PATTERN (head)))
	       != FP_REG_P (SET_DEST (PATTERN (head))))))
    {
      /* If at the target of this label we set the condition codes,
	 and the condition codes are already set for that value,
	 advance, if we can, to the following insn.  */
      if (GET_CODE (PATTERN (head)) == SET
	  && GET_CODE (SET_DEST (PATTERN (head))) == CC0
	  && cc_status.value2 == SET_SRC (PATTERN (head)))
	{
	  rtx nhead = next_real_insn_no_labels (head);
	  if (nhead
	      && GET_CODE (nhead) == INSN
	      && GET_CODE (PATTERN (nhead)) == SET
	      && strict_single_insn_op_p (SET_SRC (PATTERN (nhead)),
					  GET_MODE (SET_DEST (nhead)))
	      && strict_single_insn_op_p (SET_DEST (PATTERN (nhead)), VOIDmode)
	      /* Moves between FP regs and CPU regs are two insns.  */
	      && !(GET_CODE (SET_SRC (PATTERN (nhead))) == REG
		   && GET_CODE (SET_DEST (PATTERN (nhead))) == REG
		   && (FP_REG_P (SET_SRC (PATTERN (nhead)))
		       != FP_REG_P (SET_DEST (PATTERN (nhead))))))
	    head = nhead;
	}

      /* Output the branch instruction first.  */
      if (cc_prev_status.flags & CC_IN_FCCR)
	{
	  if (parity)
	    output_asm_insn (\"fb%F0,a %l1 ! eager\", xoperands);
	  else
	    output_asm_insn (\"fb%C0,a %l1 ! eager\", xoperands);
	}
      else if (cc_prev_status.flags & CC_NO_OVERFLOW)
	{
	  if (parity)
	    output_asm_insn (\"b%U0,a %l1 ! eager\", xoperands);
	  else
	    output_asm_insn (\"b%I0,a %l1 ! eager\", xoperands);
	}
      else
	{
	  if (parity)
	    output_asm_insn (\"b%N0,a %l1 ! eager\", xoperands);
	  else
	    output_asm_insn (\"b%C0,a %l1 ! eager\", xoperands);
	}

      /* Now steal the first insn of the target.  */
      output_eager_then_insn (head, operands);
    }
  else
    {
      /* Output the branch instruction first.  */
      if (cc_prev_status.flags & CC_IN_FCCR)
	{
	  if (parity)
	    output_asm_insn (\"fb%F0 %l1 ! eager\", xoperands);
	  else
	    output_asm_insn (\"fb%C0 %l1 ! eager\", xoperands);
	}
      else if (cc_prev_status.flags & CC_NO_OVERFLOW)
	{
	  if (parity)
	    output_asm_insn (\"b%U0,a %l1 ! eager\", xoperands);
	  else
	    output_asm_insn (\"b%I0,a %l1 ! eager\", xoperands);
	}
      else
	{
	  if (parity)
	    output_asm_insn (\"b%N0 %l1 ! eager\", xoperands);
	  else
	    output_asm_insn (\"b%C0 %l1 ! eager\", xoperands);
	}
    }
  return output_delay_insn (delay_insn);
}")

;; Here are two simple peepholes which fill the delay slot of
;; an unconditional branch.

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "single_insn_src_p" "p"))
   (set (pc) (label_ref (match_operand 2 "" "")))]
  "single_insn_extra_test (operands[0], operands[1])"
  "* return output_delayed_branch (\"b %l2\", operands, insn);")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "reg_or_0_operand" "rJ"))
   (set (pc) (label_ref (match_operand 2 "" "")))]
  ""
  "* return output_delayed_branch (\"b %l2\", operands, insn);")

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp %0\;nop")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "single_insn_src_p" "p"))
   (parallel [(set (pc) (match_operand:SI 2 "register_operand" "r"))
	      (use (label_ref (match_operand 3 "" "")))])]
  "REGNO (operands[0]) != REGNO (operands[2])
   && single_insn_extra_test (operands[0], operands[1])"
  "* return output_delayed_branch (\"jmp %2\", operands, insn);")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "reg_or_0_operand" "rJ"))
   (parallel [(set (pc) (match_operand:SI 2 "register_operand" "r"))
	      (use (label_ref (match_operand 3 "" "")))])]
  ""
  "* return output_delayed_branch (\"jmp %2\", operands, insn);")

;;- jump to subroutine
(define_expand "call"
  [(call (match_operand:SI 0 "memory_operand" "m")
	 (match_operand 1 "" "i"))]
  ;; operand[2] is next_arg_register
  ""
  "
{
  rtx fn_rtx, nregs_rtx;

  if (TARGET_SUN_ASM && GET_CODE (XEXP (operands[0], 0)) == REG)
    {
      rtx g1_rtx = gen_rtx (REG, SImode, 1);
      emit_move_insn (g1_rtx, XEXP (operands[0], 0));
      fn_rtx = gen_rtx (MEM, SImode, g1_rtx);
    }
  else
    fn_rtx = operands[0];

  /* Count the number of parameter registers being used by this call.
     if that argument is NULL, it means we are using them all, which
     means 6 on the sparc.  */
#if 0
  if (operands[2])
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, REGNO (operands[2]) - 8);
  else
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, 6);
#else
  nregs_rtx = const0_rtx;
#endif

  emit_call_insn (gen_rtx (PARALLEL, VOIDmode, gen_rtvec (2,
			   gen_rtx (CALL, VOIDmode, fn_rtx, nregs_rtx),
			   gen_rtx (USE, VOIDmode, gen_rtx (REG, SImode, 31)))));
  DONE;
}")

(define_insn ""
  [(call (match_operand:SI 0 "memory_operand" "m")
	 (match_operand 1 "" "i"))
   (use (reg:SI 31))]
  ;;- Don't use operand 1 for most machines.
  "CONSTANT_P (XEXP (operands[0], 0))
   || GET_CODE (XEXP (operands[0], 0)) == REG"
  "*
{
  /* strip the MEM.  */
  operands[0] = XEXP (operands[0], 0);
  CC_STATUS_INIT;
  if (TARGET_SUN_ASM && GET_CODE (operands[0]) == REG)
    return \"jmpl %a0,%%o7\;nop\";
  return \"call %a0,%1\;nop\";
}")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "single_insn_src_p" "p"))
   (parallel [(call (match_operand:SI 2 "memory_operand" "m")
		    (match_operand 3 "" "i"))
	      (use (reg:SI 31))])]
  ;;- Don't use operand 1 for most machines.
  "! reg_mentioned_p (operands[0], operands[2])
   && single_insn_extra_test (operands[0], operands[1])"
  "*
{
  /* strip the MEM.  */
  operands[2] = XEXP (operands[2], 0);
  if (TARGET_SUN_ASM && GET_CODE (operands[2]) == REG)
    return output_delayed_branch (\"jmpl %a2,%%o7\", operands, insn);
  return output_delayed_branch (\"call %a2,%3\", operands, insn);
}")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "reg_or_0_operand" "rJ"))
   (parallel [(call (match_operand:SI 2 "memory_operand" "m")
		    (match_operand 3 "" "i"))
	      (use (reg:SI 31))])]
  ;;- Don't use operand 1 for most machines.
  ""
  "*
{
  /* strip the MEM.  */
  operands[2] = XEXP (operands[2], 0);
  if (TARGET_SUN_ASM && GET_CODE (operands[2]) == REG)
    return output_delayed_branch (\"jmpl %a2,%%o7\", operands, insn);
  return output_delayed_branch (\"call %a2,%3\", operands, insn);
}")

(define_expand "call_value"
  [(set (match_operand 0 "register_operand" "=rf")
	(call (match_operand:SI 1 "memory_operand" "m")
	      (match_operand 2 "" "i")))]
  ;; operand 3 is next_arg_register
  ""
  "
{
  rtx fn_rtx, nregs_rtx;
  rtvec vec;

  if (TARGET_SUN_ASM && GET_CODE (XEXP (operands[1], 0)) == REG)
    {
      rtx g1_rtx = gen_rtx (REG, SImode, 1);
      emit_move_insn (g1_rtx, XEXP (operands[1], 0));
      fn_rtx = gen_rtx (MEM, SImode, g1_rtx);
    }
  else
    fn_rtx = operands[1];

#if 0
  if (operands[3])
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, REGNO (operands[3]) - 8);
  else
    nregs_rtx = gen_rtx (CONST_INT, VOIDmode, 6);
#else
  nregs_rtx = const0_rtx;
#endif

  vec = gen_rtvec (2,
		   gen_rtx (SET, VOIDmode, operands[0],
			    gen_rtx (CALL, VOIDmode, fn_rtx, nregs_rtx)),
		   gen_rtx (USE, VOIDmode, gen_rtx (REG, SImode, 31)));

  emit_call_insn (gen_rtx (PARALLEL, VOIDmode, vec));
  DONE;
}")

(define_insn ""
  [(set (match_operand 0 "" "=rf")
	(call (match_operand:SI 1 "memory_operand" "m")
	      (match_operand 2 "" "i")))
   (use (reg:SI 31))]
  ;;- Don't use operand 2 for most machines.
  "CONSTANT_P (XEXP (operands[1], 0))
   || GET_CODE (XEXP (operands[1], 0)) == REG"
  "*
{
  /* strip the MEM.  */
  operands[1] = XEXP (operands[1], 0);
  CC_STATUS_INIT;
  if (TARGET_SUN_ASM && GET_CODE (operands[1]) == REG)
    return \"jmpl %a1,%%o7\;nop\";
  return \"call %a1,%2\;nop\";
}")

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "single_insn_src_p" "p"))
   (parallel [(set (match_operand 2 "" "=rf")
		   (call (match_operand:SI 3 "memory_operand" "m")
			 (match_operand 4 "" "i")))
	      (use (reg:SI 31))])]
  ;;- Don't use operand 4 for most machines.
  "! reg_mentioned_p (operands[0], operands[3])
   && single_insn_extra_test (operands[0], operands[1])"
  "*
{
  /* strip the MEM.  */
  operands[3] = XEXP (operands[3], 0);
  if (TARGET_SUN_ASM && GET_CODE (operands[3]) == REG)
    return output_delayed_branch (\"jmpl %a3,%%o7\", operands, insn);
  return output_delayed_branch (\"call %a3,%4\", operands, insn);
}")

(define_peephole
  [(set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "reg_or_0_operand" "rJ"))
   (parallel [(set (match_operand 2 "" "=rf")
		   (call (match_operand:SI 3 "memory_operand" "m")
			 (match_operand 4 "" "i")))
	      (use (reg:SI 31))])]
  ;;- Don't use operand 4 for most machines.
  ""
  "*
{
  /* strip the MEM.  */
  operands[3] = XEXP (operands[3], 0);
  if (TARGET_SUN_ASM && GET_CODE (operands[3]) == REG)
    return output_delayed_branch (\"jmpl %a3,%%o7\", operands, insn);
  return output_delayed_branch (\"call %a3,%4\", operands, insn);
}")

(define_insn "return"
  [(return)]
  "! TARGET_EPILOGUE"
  "ret\;restore")

(define_peephole
  [(set (reg:SI 24)
	(match_operand:SI 0 "reg_or_0_operand" "rJ"))
   (return)]
  "! TARGET_EPILOGUE"
  "ret\;restore %r0,0x0,%%o0")

(define_peephole
  [(set (reg:SI 24)
	(plus:SI (match_operand:SI 0 "register_operand" "r%")
		 (match_operand:SI 1 "arith_operand" "rI")))
   (return)]
  "! TARGET_EPILOGUE"
  "ret\;restore %r0,%1,%%o0")

(define_peephole
  [(set (reg:SI 24)
	(minus:SI (match_operand:SI 0 "register_operand" "r")
		  (match_operand:SI 1 "small_int" "I")))
   (return)]
  "! TARGET_EPILOGUE"
  "ret\;restore %0,-(%1),%%o0")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:

