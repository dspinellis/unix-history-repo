;;  Mips.md        Naive version of Machine Description for MIPS
;;  Contributed by   A. Lichnewsky, lich@inria.inria.fr
;;  Changes by       Michael Meissner, meissner@osf.org
;;  Copyright (C) 1989, 1990 Free Software Foundation, Inc.

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

;;
;;------------------------------------------------------------------------
;;

;;
;;  ....................
;;
;;  Peephole Optimizations for
;;
;;          ARITHMETIC
;;
;;  ....................
;;
					;;- The following peepholes are
					;;- motivated by the fact that
					;;- stack movement result in some
					;;- cases in embarrassing sequences
					;;- of addiu SP,SP,int
					;;-    addiu SP,SP,other_int

					;;- --------------------
					;;- REMARK: this would be done better
					;;- by analysis of dependencies in
					;;- basic blocks, prior to REG ALLOC,
					;;- and simplification of trees:
					;;-      (+  (+ REG const) const)
					;;- ->   (+ REG newconst)
					;;- --------------------
					;;- Merged peephole code from
					;;- raeburn@ATHENA.MIT.EDU

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "=r")
        (match_operator 1 "additive_op"
                        [(match_operand:SI 2 "register_operand" "r")
                         (match_operand:SI 3 "small_int" "I")]))
   (set (match_operand:SI 4 "register_operand" "=r")
        (match_operator 5 "additive_op"
                        [(match_dup 0)
                         (match_operand:SI 6 "small_int" "I")]))]
  "(REGNO (operands[0]) == REGNO (operands[4])
    || dead_or_set_p (insn, operands[0]))"
  "*
{
  int addend;
  /* compute sum, with signs */
  addend = INTVAL (operands[3]) * (GET_CODE (operands[1]) == PLUS ? 1 : -1);
  addend += INTVAL (operands[6]) * (GET_CODE (operands[5]) == PLUS ? 1 : -1);
  if (addend != 0)
    {
      operands[0] = gen_rtx (CONST_INT, VOIDmode, addend);
      return \"addi%:\\t%4,%2,%0\";
    }
  /* value is zero; copy */
  if (REGNO (operands[4]) != REGNO (operands[2]))
    return \"add%:\\t%4,%2,$0\";
  /* copying to self; punt */
  return \" # null operation: additive operands cancel (%0,%2)\";
}")

;;
;;  ....................
;;
;;          ARITHMETIC
;;
;;  ....................
;;

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(plus:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  ""
  "add.d\\t%0,%1,%2")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(plus:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  ""
  "add.s\\t%0,%1,%2")

;; The following is generated when omiting the frame pointer
;; and for referencing large auto arrays during optimization.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] == stack_pointer_rtx || operands[1] == frame_pointer_rtx"
  "*
{
  int number;
  if (GET_CODE (operands[2]) != CONST_INT)
    return \"add%:\\t%0,%1,%2\";

  number = INTVAL (operands[2]);
  if (((unsigned) (number + 0x8000) > 0xffff))
    {
      operands[3] = gen_rtx (REG, SImode, 1);	/* assembler temp. */
      return \".set\\tnoat\\n\\tli\\t%3,%2\\n\\tadd%:\\t%0,%1,%3\\n\\t.set\\tat\";
    }

  return (number < 0) ? \"sub%:\\t%0,%1,%n2\" : \"add%:\\t%0,%1,%2\";
}")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(plus:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"sub%:\\t%0,%1,%n2\"
    : \"add%:\\t%0,%1,%2\";
}")


;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(minus:DF (match_operand:DF 1 "register_operand" "f")
		  (match_operand:DF 2 "register_operand" "f")))]
  ""
  "sub.d\\t%0,%1,%2")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(minus:SF (match_operand:SF 1 "register_operand" "f")
		  (match_operand:SF 2 "register_operand" "f")))]
  ""
  "sub.s\\t%0,%1,%2")

;; The following is generated when omiting the frame pointer
;; and for referencing large auto arrays during optimization.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "%r")
		  (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] == stack_pointer_rtx || operands[1] == frame_pointer_rtx"
  "*
{
  int number;
  if (GET_CODE (operands[2]) != CONST_INT)
    return \"sub%:\\t%0,%1,%2\";

  number = INTVAL (operands[2]);
  if (((unsigned) (number + 0x8000) > 0xffff))
    {
      operands[3] = gen_rtx (REG, SImode, 1);	/* assembler temp. */
      return \".set\\tnoat\\n\\tli\\t%3,%2\\n\\tsub%:\\t%0,%1,%3\\n\\t.set\\tat\";
    }

  return (number < 0) ? \"add%:\\t%0,%1,%n2\" : \"sub%:\\t%0,%1,%2\";
}")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    ? \"add%:\\t%0,%1,%n2\"
    : \"sub%:\\t%0,%1,%2\";
}")


;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(mult:DF (match_operand:DF 1 "register_operand" "f")
		 (match_operand:DF 2 "register_operand" "f")))]
  ""
  "mul.d\\t%0,%1,%2")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(mult:SF (match_operand:SF 1 "register_operand" "f")
		 (match_operand:SF 2 "register_operand" "f")))]
  ""
  "mul.s\\t%0,%1,%2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI (match_operand:SI 1 "arith_operand" "%r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "mul\\t%0,%1,%2")


;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(div:DF (match_operand:DF 1 "register_operand" "f")
		(match_operand:DF 2 "register_operand" "f")))]
  ""
  "div.d\\t%0,%1,%2")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(div:SF (match_operand:SF 1 "register_operand" "f")
		(match_operand:SF 2 "register_operand" "f")))]
  ""
  "div.s\\t%0,%1,%2")

(define_insn "divmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))
   (set (match_operand:SI 3 "register_operand" "=r")
	(mod:SI (match_dup 1)
		(match_dup 2)))]
  ""
  "div\\t$0,%1,%2\\n\\tmflo\\t%0\\t\\t#quotient\\n\\tmfhi\\t%3\\t\\t#remainder")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(div:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "div\\t%0,%1,%2")

(define_insn "udivmodsi4"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "arith_operand" "rI")))
   (set (match_operand:SI 3 "register_operand" "=r")
	(umod:SI (match_dup 1)
		 (match_dup 2)))]
  ""
  "divu\\t$0,%1,%2\\n\\tmflo\\t%0\\t\\t#quotient\\n\\tmfhi\\t%3\\t\\t#remainder")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(udiv:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "divu\\t%0,%1,%2")


;; Remainder instructions


(define_insn "modsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mod:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "rem\\t%0,%1,%2")

(define_insn "umodsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(umod:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "remu\\t%0,%1,%2")


;; Absoluate value instructions -- Don't use the integer abs,
;; since that signals an exception on -2147483648 (sigh).

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(abs:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "abs.s\\t%0,%1")

(define_insn "absdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(abs:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "abs.d\\t%0,%1")

;;
;;  ....................
;;
;;          LOGICAL
;;
;;  ....................
;;

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(and:DI (match_operand:DI 1 "register_operand" "r")
                (match_operand:DI 2 "register_operand" "r")))]
  ""
  "and\\t%0,%1,%2\;and\\t%D0,%D1,%D2")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(and:SI (match_operand:SI 1 "uns_arith_operand" "%r")
		(match_operand:SI 2 "uns_arith_operand" "rK")))]
  ""
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT)
		? \"andi\\t%0,%1,%x2\"
		: \"and\\t%0,%1,%2\";
}")


;; Simple hack to recognize the "nor" instruction on the MIPS
;; [rms: I don't think the following is actually required.]
;; This must appear before the normal or patterns, so that the
;; combiner will correctly fold things.

(define_insn ""
  [(set (match_operand:DI 0 "register_operand" "=r")
	(not:DI (ior:DI (match_operand:DI 1 "register_operand" "r")
			(match_operand:DI 2 "register_operand" "r"))))]
  ""
  "nor\\t%0,%1,%2\;nor\\t%D0,%D1,%D2")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (ior:SI (match_operand:SI 1 "register_operand" "r")
			(match_operand:SI 2 "register_operand" "r"))))]
  ""
  "nor\\t%0,%1,%2")

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(ior:DI (match_operand:DI 1 "register_operand" "r")
                (match_operand:DI 2 "register_operand" "r")))]
  ""
  "or\\t%0,%1,%2\;or\\t%D0,%D1,%D2")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ior:SI (match_operand:SI 1 "uns_arith_operand" "%r")
		(match_operand:SI 2 "uns_arith_operand" "rJ")))]
  ""
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT)
		? \"ori\\t%0,%1,%x2\"
		: \"or\\t%0,%1,%2\";
}")

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=&r")
	(xor:DI (match_operand:DI 1 "register_operand" "r")
                (match_operand:DI 2 "register_operand" "r")))]
  ""
  "xor\\t%0,%1,%2\;xor\\t%D0,%D1,%D2")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(xor:SI (match_operand:SI 1 "uns_arith_operand" "%r")
		(match_operand:SI 2 "uns_arith_operand" "rK")))]
  ""
  "*
{
  return (GET_CODE (operands[2]) == CONST_INT)
		? \"xori\\t%0,%1,%x2\"
		: \"xor\\t%0,%1,%2\";
}")

;;
;;  ....................
;;
;;          TRUNCATION
;;
;;  ....................

;; Extension and truncation insns.
;; Those for integer source operand
;; are ordered widest source type first.


(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "andi\\t%0,%1,0xff\\t#truncsiqi2\\t %1 -> %0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(truncate:HI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "*
    output_asm_insn (\"sll\\t%0,%1,0x10\\t#truncsihi2\\t %1 -> %0\",
                    operands);
    return \"sra\\t%0,%0,0x10\\t#truncsihi2\\t %1 -> %0\";
")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI (match_operand:HI 1 "register_operand" "r")))]
  ""
  "andi\\t%0,%1,0xff\\t#trunchiqi2\\t %1 -> %0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "cvt.s.d\\t%0,%1\\t#truncdfsf2\\t %1 -> %0")

;;
;;  ....................
;;
;;          ZERO EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand
;; are ordered widest source type first.



(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "r,m")))]
  ""
  "*
{
  if (which_alternative == 0)
    {
      output_asm_insn (\"sll\\t%0,%1,0x10\\t#zero_extendhisi2\\t %1 -> %0\",
		       operands);
      return \"srl\\t%0,%0,0x10\\t#zero_extendhisi2\\t %1 -> %0\";
    }
  else
    return \"lhu\\t%0,%1\\t#zero extendhisi2 %1 -> %0\";
}")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "*
    output_asm_insn (\"sll\\t%0,%1,0x18\\t#zero_extendqihi2\\t %1 -> %0\",
                    operands);
    return \"srl\\t%0,%0,0x18\\t#zero_extendqihi2\\t %1 -> %0\";
  ")


(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "general_operand" "r,m")))]
  ""
  "*
{
  if (which_alternative == 0)
    {
      return \"andi\\t%0,%1,0xff\\t#zero_extendqisi2\\t %1 -> %0\";
    }
  else
    return \"lbu\\t%0,%1\\t#zero extendqisi2 %1 -> %0\";
}")


;;
;;  ....................
;;
;;          SIGN EXTENSION
;;
;;  ....................

;; Extension insns.
;; Those for integer source operand
;; are ordered widest source type first.



(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "r,m")))]
  ""
  "*
{
  if (which_alternative == 0)
    {
      output_asm_insn (\"sll\\t%0,%1,0x10\\t#sign extendhisi2\\t %1 -> %0\",
		       operands);
      return \"sra\\t%0,%0,0x10\\t#sign extendhisi2\\t %1 -> %0\";
    }
  else
    return \"lh\\t%0,%1\\t#sign extendhisi2 %1 -> %0\";
}")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "*
    output_asm_insn (\"sll\\t%0,%1,0x18\\t#sign extendqihi2\\t %1 -> %0\",
                    operands);
    return \"sra\\t%0,%0,0x18\\t#sign extendqihi2\\t %1 -> %0\";
  ")


(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "r,m")))]
  ""
  "*
{
  if (which_alternative == 0)
    {
      output_asm_insn (\"sll\\t%0,%1,0x18\\t#sign extendqisi2\\t %1 -> %0\",
		       operands);
      return \"sra\\t%0,%0,0x18\\t#sign extendqisi2\\t %1 -> %0\";
    }
  else
    return \"lb\\t%0,%1\\t#sign extendqisi2 %1 -> %0\";
}")


(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "cvt.d.s\\t%0,%1\\t#extendsfdf2\\t %1 -> %0")


;;
;;  ....................
;;
;;          CONVERSIONS
;;
;;  ....................


(define_insn "fix_truncdfsi2_internal"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(fix:DF (match_operand:DF 1 "register_operand" "f")))
   (clobber (match_operand:SI 2 "register_operand" "r"))]
  ""
  "trunc.w.d %0,%1,%2")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "f"))))]
  ""
  "mfc1\\t%0,%1")

(define_expand "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "f"))))]
  ""
  "
{
  rtx reg1 = gen_reg_rtx (DFmode);	/* fp reg that gets trunc result */
  rtx reg2 = gen_reg_rtx (SImode);	/* gp reg that saves FP status bits */
  emit_insn (gen_fix_truncdfsi2_internal (reg1, operands[1], reg2));
  operands[1] = reg1;
  /* Fall through and generate default code */
}")

(define_insn "fix_truncsfsi2_internal"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(fix:SF (match_operand:SF 1 "register_operand" "f")))
   (clobber (match_operand:SI 2 "register_operand" "r"))]
  ""
  "trunc.w.s %0,%1,%2")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  ""
  "mfc1\\t%0,%1")

(define_expand "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "f"))))]
  ""
  "
{
  rtx reg1 = gen_reg_rtx (SFmode);	/* fp reg that gets trunc result */
  rtx reg2 = gen_reg_rtx (SImode);	/* gp reg that saves FP status bits */
  emit_insn (gen_fix_truncsfsi2_internal (reg1, operands[1], reg2));
  operands[1] = reg1;
  /* Fall through and generate default code */
}")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(float:DF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "mtc1\\t%1,%0\\t\\t#floatsidf2\\t%1 -> %0\;cvt.d.w\\t%0,%0")


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=f")
	(float:SF (match_operand:SI 1 "register_operand" "r")))]
  ""
  "mtc1\\t%1,%0\\t\\t#floatsisf2\\t%1 -> %0\;cvt.s.w\\t%0,%0")

(define_expand "fixuns_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:DF 1 "register_operand" "")))]
  ""
  "
{
  rtx reg1 = gen_reg_rtx (DFmode);
  rtx reg2 = gen_reg_rtx (DFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset = REAL_VALUE_LDEXP (1.0, 31);

  if (reg1)			/* turn off complaints about unreached code */
    {
      emit_move_insn (reg1, immed_real_const_1 (offset, DFmode));
      do_pending_stack_adjust ();
      emit_insn (gen_rtx (SET, VOIDmode, cc0_rtx,
			  gen_rtx (COMPARE, DFmode, operands[1], reg1)));

      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_truncdfsi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			       gen_rtx (LABEL_REF, VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx (MINUS, DFmode, operands[1], reg1));
      emit_move_insn (reg3, gen_rtx (CONST_INT, VOIDmode, 0x80000000));

      emit_insn (gen_fix_truncdfsi2 (operands[0], reg2));
      emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx (USE, VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")

(define_expand "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "")))]
  ""
  "
{
  rtx reg1 = gen_reg_rtx (SFmode);
  rtx reg2 = gen_reg_rtx (SFmode);
  rtx reg3 = gen_reg_rtx (SImode);
  rtx label1 = gen_label_rtx ();
  rtx label2 = gen_label_rtx ();
  REAL_VALUE_TYPE offset = REAL_VALUE_LDEXP (1.0, 31);

  if (reg1)			/* turn off complaints about unreached code */
    {
      emit_move_insn (reg1, immed_real_const_1 (offset, SFmode));
      do_pending_stack_adjust ();
      emit_insn (gen_rtx (SET, VOIDmode, cc0_rtx,
			  gen_rtx (COMPARE, SFmode, operands[1], reg1)));

      emit_jump_insn (gen_bge (label1));

      emit_insn (gen_fix_truncsfsi2 (operands[0], operands[1]));
      emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			       gen_rtx (LABEL_REF, VOIDmode, label2)));
      emit_barrier ();

      emit_label (label1);
      emit_move_insn (reg2, gen_rtx (MINUS, SFmode, operands[1], reg1));
      emit_move_insn (reg3, gen_rtx (CONST_INT, VOIDmode, 0x80000000));

      emit_insn (gen_fix_truncsfsi2 (operands[0], reg2));
      emit_insn (gen_iorsi3 (operands[0], operands[0], reg3));

      emit_label (label2);

      /* allow REG_NOTES to be set on last insn (labels don't have enough
	 fields, and can't be used for REG_NOTES anyway).  */
      emit_insn (gen_rtx (USE, VOIDmode, stack_pointer_rtx));
      DONE;
    }
}")

					;;- Wild things used when
					;;- unions make double and int
					;;- overlap.
					;;-
					;;- This must be supported
					;;- since corresponding code
					;;- gets generated

(define_insn ""
  [(set (subreg:DF (match_operand:DI 0 "register_operand" "=ry") 0)
	(match_operand:DF 1  "register_operand" "rf"))
   (clobber (match_operand  2  "register_operand" "rf"))]
  ""
  "mfc1\\t%0,%L1\;mfc1\\t%D0,%M1")

(define_insn ""
  [(set (subreg:DF (match_operand:DI 0 "register_operand" "=ry") 0)
	(match_operand:DF 1  "register_operand" "rf"))]
  ""
  "mfc1\\t%0,%L1\;mfc1\\t%D0,%M1")

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=rf")
        (subreg:DF (match_operand:DI 1 "register_operand" "ry") 0))
   (clobber (match_operand  2  "register_operand" "rf"))]
  ""
  "mfc1\\t%0,%L1\;mfc1\\t%D0,%M1")

(define_insn ""
  [(set (match_operand:DF 0 "register_operand" "=rf")
        (subreg:DF (match_operand:DI 1 "register_operand" "ry") 0))]
  ""
  "mfc1\\t%0,%L1\;mfc1\\t%D0,%M1")

;;
;;  ....................
;;
;;          MOVES
;;
;;          and
;;
;;          LOADS AND STORES
;;
;;  ....................

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=r,*r,*m")
	(match_operand:DI 1 "general_operand" "r,*miF,*r"))]
  ""
  "*
{
  extern rtx adj_offsettable_operand ();
  extern int offsettable_address_p ();

  if (which_alternative == 0)
    {
      /* Move REGISTER <- REGISTER */
      if (REGNO (operands[0]) != (REGNO (operands[1])+1))
	return \"move\\t%0,%1\\n\\tmove\\t%D0,%D1\";
      else
	return \"move\\t%D0,%D1\\n\\tmove\\t%0,%1\";
    }

  else if (which_alternative == 1)
    {
      if (GET_CODE (operands[1]) == MEM)
	{
	  /* REGISTER <- MEMORY */
	  if (offsettable_address_p (1, DImode, XEXP (operands[1], 0)))
	    {
	      operands[2] = adj_offsettable_operand (operands[1], 4);
	      return \"lw\\t%0,%1\;lw\\t%D0,%2\";
	    }

	  else
	    {
	      operands[2] = gen_rtx (REG, Pmode, 1);
	      return \".set\\tnoat\;la\\t%2,%1\;lw\\t%0,0(%2)\;lw\\t%D0,4(%2)\;set\\tat\";
	    }
	}

      /* REGISTER <- small integer constant */
      else if (CONSTANT_P (operands[1]))
	{
	  operands[2] = gen_rtx (CONST_INT, VOIDmode, INTVAL (operands[1]) >= 0 ? 0 : -1);
	  return \"li\\t%M0,%2\;li\\t%L0,%1\";
	}

      /* Register <- large integer constant */
      else if (GET_CODE (operands[1]) == CONST_DOUBLE)
	{
	  operands[2] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_LOW (operands[1]));
	  operands[3] = gen_rtx (CONST_INT, VOIDmode, CONST_DOUBLE_HIGH (operands[1]));
	  return \"li\\t%M0,%3\;li\\t%L0,%2\";
	}
    }

  else if (which_alternative == 2 && GET_CODE (operands[0]) == MEM)
    {
      /* Memory <- Register */
      if (offsettable_address_p (1, DImode, XEXP (operands[0], 0)))
	{
	  operands[2] = adj_offsettable_operand (operands[0], 4);
	  return \"sw\\t%1,%0\;sw\\t%D1,%2\";
	}

      else
	{
	  operands[2] = gen_rtx (REG, Pmode, 1);
	  return \".set\\tnoat\;la\\t%2,%0\;sw\\t%1,0(%2)\;sw\\t%D1,4(%2)\;set\\tat\";
	}
    }

  abort_with_insn (insn, \"impossible case in movdi\");
  return \"\";
}")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=r,r,m,r,r,m,*r")
	(match_operand:SI 1 "general_operand" "r,m,r,i,J,J,*p"))]
  ""
  "*
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);

  if (code0 == REG && code1 == REG)
    return \"move\\t%0,%1\";

  else if (code0 == REG && code1 == MEM)
    return \"lw\\t%0,%1\";

  else if (code0 == MEM && code1 == REG)
    return \"sw\\t%1,%0\";

  else if (code0 == REG && code1 == CONST_INT)
    return \"li\\t%0,%1\";

  else if (code0 == MEM && code1 == CONST_INT && INTVAL (operands[1]) == 0)
    return \"sw\\t$0,%0\";

  else if (code0 == REG && CONSTANT_P (operands[1]))
    return \"la\\t%0,%a1\";

  else if (code0 == REG && code1 == PLUS
	   && GET_CODE (XEXP (operands[1], 0)) == REG
	   && GET_CODE (XEXP (operands[1], 1)) == CONST_INT)
    {
      operands[2] = XEXP (operands[1], 0);
      operands[3] = XEXP (operands[1], 1);
      return \"add%:\\t%0,%2,%3\";
    }

  abort_with_insn (insn, \"Bad movsi\");
  return 0;
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=r,r,m,r,r,m")
	(match_operand:HI 1 "general_operand" "r,m,r,i,J,J"))]
  ""
  "*
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);

  if (code0 == REG && code1 == REG)
    return \"move\\t%0,%1\";

  else if (code0 == REG && code1 == MEM)
    return \"lh\\t%0,%1\";

  else if (code0 == MEM && code1 == REG)
    return \"sh\\t%1,%0\";

  else if (code0 == REG && code1 == CONST_INT)
    return \"li\\t%0,%1\";

  else if (code0 == MEM && code1 == CONST_INT && INTVAL (operands[1]) == 0)
    return \"sh\\t$0,%0\";

  else if (code0 == REG && CONSTANT_P (operands[1]))
    return \"la\\t%0,%a1\";

  else if (code0 == REG && code1 == PLUS
	   && GET_CODE (XEXP (operands[1], 0)) == REG
	   && GET_CODE (XEXP (operands[1], 1)) == CONST_INT)
    {
      operands[2] = XEXP (operands[1], 0);
      operands[3] = XEXP (operands[1], 1);
      return \"add%:\\t%0,%2,%3\";
    }

  abort_with_insn (insn, \"Bad movhi\");
  return 0;
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=r,r,m,r,r,m")
	(match_operand:QI 1 "general_operand" "r,m,r,i,J,J"))]
  ""
  "*
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);

  if (code0 == REG && code1 == REG)
    return \"move\\t%0,%1\";

  else if (code0 == REG && code1 == MEM)
    return \"lb\\t%0,%1\";

  else if (code0 == MEM && code1 == REG)
    return \"sb\\t%1,%0\";

  else if (code0 == REG && code1 == CONST_INT)
    return \"li\\t%0,%1\";

  else if (code0 == MEM && code1 == CONST_INT && INTVAL (operands[1]) == 0)
    return \"sb\\t$0,%0\";

  else if (code0 == REG && CONSTANT_P (operands[1]))
    return \"la\\t%0,%a1\";

  else if (code0 == REG && code1 == PLUS
	   && GET_CODE (XEXP (operands[1], 0)) == REG
	   && GET_CODE (XEXP (operands[1], 1)) == CONST_INT)
    {
      operands[2] = XEXP (operands[1], 0);
      operands[3] = XEXP (operands[1], 1);
      return \"add%:\\t%0,%2,%3\";
    }

  abort_with_insn (insn, \"Bad movqi\");
  return 0;
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=f,f,m,fy,*f,*y,*y,*m")
	(match_operand:SF 1 "general_operand" "f,m,f,F,*y,*f,*m,*y"))]
  ""
  "*
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);

  if (code0 == REG)
    {
      if (code1 == REG)
	{
	  if (FP_REG_P (REGNO (operands[0])))
	    {
	      if (FP_REG_P (REGNO (operands[1])))
		return \"mov.s\\t%0,%1\";
	      else
		return \"mtc1\\t%1,%0\\t\\t# Calling sequence trick\";
	    }

	  else if (FP_REG_P (REGNO (operands[1])))
	    return \"mfc1\\t%0,%1\\t\\t# Calling sequence trick\";

	  else
	    return \"move\\t%0,%1\";
	}

      else if (code1 == CONST_DOUBLE)
	return \"li.s\\t%0,%1\";

      else if (code1 == MEM)
	return (GP_REG_P (REGNO (operands[0]))) ? \"lw\\t%0,%1\" : \"l.s\\t%0,%1\";
    }

  else if (code0 == MEM && code1 == REG)
    return (GP_REG_P (REGNO (operands[1]))) ? \"sw\\t%1,%0\" : \"s.s\\t%1,%0\";

  abort_with_insn (insn, \"Bad movsf\");
  return \"\";
}")


(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=f,f,m,fy,*f,*y,&*y,*m")
	(match_operand:DF 1 "general_operand" "f,m,f,F,*y,*f,*m,*y"))]
  ""
  "*
{
  extern rtx adj_offsettable_operand ();
  extern int offsettable_address_p ();

  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);

  if (code0 == REG)
    {
      if (code1 == REG)
	{
	  if (FP_REG_P (REGNO (operands[0])))
	    {
	      if (FP_REG_P (REGNO (operands[1])))
		return \"mov.d\\t%0,%1\";
	      else
		return \"mtc1\\t%L1,%0\\t\\t# Calling sequence trick\;mtc1\\t%M1,%D0\";
	    }

	  else if (FP_REG_P (REGNO (operands[1])))
	    return \"mfc1\\t%L0,%1\\t\\t# Calling sequence trick\;mfc1\\t%M0,%D1\";

	  else if (REGNO (operands[0]) != (REGNO (operands[1])+1))
	    return \"move\\t%0,%1\\n\\tmove\\t%D0,%D1\";

	  else
	    return \"move\\t%D0,%D1\\n\\tmove\\t%0,%1\";
	}

      else if (code1 == CONST_DOUBLE)
	return \"li.d\\t%0,%1\";

      else if (code1 == MEM)
	{
	  if (FP_REG_P (REGNO (operands[0])))
	    return \"l.d\\t%0,%1\";

	  else if (offsettable_address_p (1, DFmode, XEXP (operands[1], 0)))
	    {
	      operands[2] = adj_offsettable_operand (operands[1], 4);
	      if (reg_mentioned_p (operands[0], operands[1]))
		return \"lw\\t%D0,%2\;lw\\t%0,%1\";
	      else
		return \"lw\\t%0,%1\;lw\\t%D0,%2\";
	    }

	  else
	    {
	      operands[2] = gen_rtx (REG, Pmode, 1);
	      return \".set\\tnoat\;la\\t%2,%1\;lw\\t%0,0(%2)\;lw\\t%D0,4(%2)\;set\\tat\";
	    }
	}
    }

  else if (code0 == MEM && code1 == REG)
    {
      if (FP_REG_P (REGNO (operands[1])))
	return \"s.d\\t%1,%0\";

      else if (offsettable_address_p (1, DFmode, XEXP (operands[0], 0)))
	{
	  operands[2] = adj_offsettable_operand (operands[0], 4);
	  return \"sw\\t%1,%0\;sw\\t%D1,%2\";
	}

      else
	{
	  operands[2] = gen_rtx (REG, Pmode, 1);
	  return \".set\\tnoat\;la\\t%2,%0\;sw\\t%1,0(%2)\;sw\\t%D1,4(%2)\;set\\tat\";
	}
    }

  abort_with_insn (insn, \"Bad movdf\");
  return \"\";
}")


;;
;;  ....................
;;
;;          OTHER ARITHMETIC AND SHIFT
;;
;;  ....................

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "r")
		   (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);

  return \"sll\\t%0,%1,%2\";
}")

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);

  return \"sra\\t%0,%1,%2\";
}")

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand" "r")
		     (match_operand:SI 2 "arith_operand" "rI")))]
  ""
  "*
{
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, (XINT (operands[2], 0))& 0x1f);

  return \"srl\\t%0,%1,%2\";
}")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "sub%:\\t%0,$0,%1")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=f")
	(neg:DF (match_operand:DF 1 "register_operand" "f")))]
  ""
  "neg.d\\t%0,%1")

(define_insn "negsf2"

  [(set (match_operand:SF 0 "register_operand" "=f")
	(neg:SF (match_operand:SF 1 "register_operand" "f")))]
  ""
  "neg.s\\t%0,%1")


(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "nor\\t%0,$0,%1")

;;
;;  ....................
;;
;;          COMPARISONS
;;
;;  ....................

					;;- Order is significant here
					;;- because there are untyped
					;;- comparisons generated by
					;;- the optimizer
                                        ;;- (set (cc0)
                                        ;;-      (compare (const_int 2)
                                        ;;-           (const_int 1)))

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "register_operand" "r")
		 (match_operand:SI 1 "arith_operand" "rI")))]
  ""
  "*
    compare_collect (SImode, operands[0], operands[1]);
    return \"\\t\\t\\t\\t# cmpsi\\t%0,%1\";
")


(define_insn ""
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "*
    compare_collect (SImode, operands[0], gen_rtx (REG, SImode, 0));
    return \"\\t\\t\\t\\t# (set (cc0)\\t%0)\";
")

;; These patterns are hopelessly invalid, because
;; comparing subword values properly requires extending them.

;; (define_insn "cmphi"
;;   [(set (cc0)
;; 	(compare (match_operand:HI 0 "register_operand" "r")
;; 		 (match_operand:HI 1 "register_operand" "r")))]
;;   ""
;;   "*
;;     compare_collect (HImode, operands[0], operands[1]);
;;     return      \" #\\tcmphi\\t%0,%1\";
;;   ")
;; 
;; (define_insn "cmpqi"
;;   [(set (cc0)
;; 	(compare (match_operand:QI 0 "register_operand" "r")
;; 		 (match_operand:QI 1 "register_operand" "r")))]
;;   ""
;;   "*
;;     compare_collect (QImode, operands[0], operands[1]);
;;     return      \" #\\tcmpqi\\t%0,%1\";
;;   ")
;; 
;; (define_insn ""
;;   [(set (cc0)
;; 	(match_operand:QI 0 "register_operand" "r"))]
;;   ""
;;   "*
;;     compare_collect (QImode, operands[0], gen_rtx (REG, QImode, 0));
;;     return \" #\\t (set (cc0)\\t%0)\";
;; ")
;; 
;; (define_insn ""
;;   [(set (cc0)
;; 	(match_operand:HI 0 "register_operand" "r"))]
;;   ""
;;   "*
;;     compare_collect (HImode, operands[0], gen_rtx (REG, HImode, 0));
;;     return \" #\\t (set (cc0)\\t%0)\";
;; ")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "f")
		 (match_operand:DF 1 "register_operand" "f")))]
  ""
  "*
    compare_collect (DFmode, operands[0], operands[1]);
    return \" #\\t\\t\\t\\tcmpdf\\t%0,%1\" ;
")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "register_operand" "f")
		 (match_operand:SF 1 "register_operand" "f")))]
  ""
  "*
    compare_collect (SFmode, operands[0], operands[1]);
    return \"\\t\\t\\t\\t# cmpsf\\t%0,%1\" ;
")

;;
;;  ....................
;;
;;          BRANCHES
;;
;;  ....................

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "*
{
  if (GET_CODE (operands[0]) == REG)
    return \"j\\t%0\";
  else
    return \"j\\t%l0\";
}")


(define_insn "tablejump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "j\\t%0")


(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.eq.d\\t%0,%1\\t\\t# beq\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# beq\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.eq.s\\t%0,%1\\t\\t# beq\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# beq\", br_ops);
    }
  else
    {
      output_asm_insn (\"beq\\t%0,%1,%2\\t\\t# beq\", br_ops);
    }
  return \"\";
}
   ")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.eq.d\\t%0,%1\\t\\t# bne\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bne\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.eq.s\\t%0,%1\\t\\t# bne\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bne\", br_ops);
    }
  else
    {
      output_asm_insn (\"bne\\t%0,%1,%2\\t\\t# bne\", br_ops);
    }
  return \"\";
}

")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# bgt branch %0 > %1\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgt\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# bgt branch %0 > %1\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgt\", br_ops);
    }
  else
    {
      output_asm_insn (\"bgt\\t%0,%1,%2\\t\\t# bgt\", br_ops);
    }
  return \"\";
}

")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# blt\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# blt\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# blt\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# blt\", br_ops);
    }
  else
    {
      output_asm_insn (\"blt\\t%0,%1,%2\\t\\t# blt\", br_ops);
    }
  return \" #\\tblt\\t%l0\\t\\t# blt\";
}
")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# bgtu\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgtu\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# bgtu\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgtu\", br_ops);
    }
  else
    {
      output_asm_insn (\"bgtu\\t%0,%1,%2\\t\\t# bgtu\", br_ops);
    }
  return \" #\\tbgtu\\t%l0\\t\\t# bgtu\";
}
")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# bltu\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# bltu\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# bltu\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# bltu\", br_ops);
    }
  else
    {
      output_asm_insn (\"bltu\\t%0,%1,%2\\t\\t# bltu\", br_ops);
    }
  return \"\";
}
")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# bge\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bge\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# bge\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bge\", br_ops);
    }
  else
    {
      output_asm_insn (\"bge\\t%0,%1,%2\\t\\t# bge\", br_ops);
    }
  return \"\";
}
")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# bgeu\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgeu\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# bgeu\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgeu\", br_ops);
    }
  else
    {
      output_asm_insn (\"bgeu\\t%0,%1,%2\\t\\t# bgeu\", br_ops);
    }
  return \"\";
}
")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# ble\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# ble\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# ble\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# ble\", br_ops);
    }
  else
    {
      output_asm_insn (\"ble\\t%0,%1,%2\\t\\t# ble\", br_ops);
    }
  return \"\";
}
")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# ble\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# ble\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# ble\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# ble\", br_ops);
    }
  else
    {
      output_asm_insn (\"bleu\\t%0,%1,%2\\t\\t# bleu\", br_ops);
    }
  return \" #\\tbleu\\t%l0\\t\\t# bleu\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.eq.d\\t%0,%1\\t\\t# beq\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# beq\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.eq.s\\t%0,%1\\t\\t# beq\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# beq\", br_ops);
    }
  else
    {
      output_asm_insn (\"beq\\t%0,%1,%2\\t\\t# beq Inv.\", br_ops);
    }
  return \"\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.eq.d\\t%0,%1\\t\\t# bne\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bne\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.eq.s\\t%0,%1\\t\\t# bne\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# beq\", br_ops);
    }
  else
    {
      output_asm_insn (\"bne\\t%0,%1,%2\\t\\t# bne Inv.\", br_ops);
    }
  return \"\";
}

")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# bgt\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# beq\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# bgt\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# beq\", br_ops);
    }
  else
    {
      output_asm_insn (\"bgt\\t%0,%1,%2\\t\\t# bgt Inv.\", br_ops);
    }
  return \"\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# bgt\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# beq\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# bgt\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# beq\", br_ops);
    }
  else
    {
      output_asm_insn (\"bgtu\\t%0,%1,%2\\t\\t# bgtu Inv.\", br_ops);
    }
  return \" #\\tbgtu\\t%l0\\t\\t# bgtu\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# blt\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# beq\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# blt\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# beq\", br_ops);
    }
  else
    {
      output_asm_insn (\"blt\\t%0,%1,%2\\t\\t# blt Inv.\", br_ops);
    }
  return \"\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# bltu\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# bltu\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# bltu\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# bltu\", br_ops);
    }
  else
    {
      output_asm_insn (\"bltu\\t%0,%1,%2\\t\\t# bltu Inv.\", br_ops);
    }
  return \" #\\tbltu\\t%l0\\t\\t# bltu\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# bge\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bge (DF) Inv.\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# bge\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bge (SF) Inv.\", br_ops);
    }
  else
    {
      output_asm_insn (\"bge\\t%0,%1,%2\\t\\t# bge Inv.\", br_ops);
    }
  return \"\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.lt.d\\t%0,%1\\t\\t# bge\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgeu (DF)  Inv.\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.lt.s\\t%0,%1\\t\\t# bge\", br_ops);
      output_asm_insn (\"bc1f\\t%2\\t\\t# bgeu (SF )Inv.\", br_ops);
    }
  else
    {
      output_asm_insn (\"bgeu\\t%0,%1,%2\\t\\t# bgeu Inv.\", br_ops);
    }
  return \" #\\tbgeu\\t%l0\\t\\t# bgeu\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# ble\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# ble\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# ble\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# ble\", br_ops);
    }
  else
    {
      output_asm_insn (\"ble\\t%0,%1,%2\\t\\t# ble Inv.\", br_ops);
    }
  return \"\";
}
")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "*
{
  rtx br_ops[3];
  enum machine_mode mode;
  compare_restore (br_ops,  &mode, insn);
  br_ops[2] = operands[0];
  if (mode == DFmode)
    {
      output_asm_insn (\"c.le.d\\t%0,%1\\t\\t# bleu\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# bleu\", br_ops);
    }
  else if  (mode == SFmode)
    {
      output_asm_insn (\"c.le.s\\t%0,%1\\t\\t# bleu\", br_ops);
      output_asm_insn (\"bc1t\\t%2\\t\\t# bleu\", br_ops);
    }
  else
    {
      output_asm_insn (\"bleu\\t%0,%1,%2\\t\\t# bleu Inv.\", br_ops);
    }
  return \"\";
}
")

;;
;;  ....................
;;
;;          LINKAGE
;;
;;  ....................

(define_insn "call"
  [(call (match_operand 0 "memory_operand" "m")
	 (match_operand 1 "" "i"))
   (clobber (reg:SI 31))]
  ""
  "*
{
  register rtx target = XEXP (operands[0], 0);

  if (GET_CODE (target) == SYMBOL_REF)
    return \"jal\\t%0\";

  else
    {
      operands[0] = target;
      return \"jal\\t$31,%0\";
    }
}")


(define_insn "call_value"
  [(set (match_operand 0 "" "=rf")
        (call (match_operand 1 "memory_operand" "m")
              (match_operand 2 "" "i")))
   (clobber (reg:SI 31))]
  ""
  "*
{
  register rtx target = XEXP (operands[1], 0);

  if (GET_CODE (target) == SYMBOL_REF)
    return \"jal\\t%1\";

  else
    {
      operands[1] = target;
      return \"jal\\t$31,%1\";
    }
}")

(define_insn "nop"
  [(const_int 0)]
  ""
  ".set\\tnoreorder\;nop\;.set\\treorder")

(define_insn "probe"
  [(mem:SI (reg:SI 29))]
  ""
  "*
{
  operands[0] = gen_rtx (REG, SImode, 1);
  operands[1] = stack_pointer_rtx;
  return \".set\\tnoat\;lw\\t%0,0(%1)\\t\\t# stack probe\;.set\\tat\";
}")

;;
;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:
