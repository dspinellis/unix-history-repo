
;;- Machine description for GNU compiler
;;- Vax Version
;;   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

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


;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;-
;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

; tstsi is first test insn so that it is the one to match
; a constant argument.

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "general_operand" "g"))]
  ""
  "tstl %0")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "general_operand" "g"))]
  ""
  "tstw %0")

(define_insn "tstqi"
  [(set (cc0)
	(match_operand:QI 0 "general_operand" "g"))]
  ""
  "tstb %0")

(define_insn "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "general_operand" "gF"))]
  ""
  "tst%# %0")

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "general_operand" "gF"))]
  ""
  "tstf %0")

;; Put cmpsi first among compare insns so it matches two CONST_INT operands.

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "general_operand" "g")
		 (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cmpl %0,%1")

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "general_operand" "g")
		 (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cmpw %0,%1")

(define_insn "cmpqi"
  [(set (cc0)
	(compare (match_operand:QI 0 "general_operand" "g")
		 (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cmpb %0,%1")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "general_operand" "gF")
		 (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cmp%# %0,%1")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "general_operand" "gF")
		 (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cmpf %0,%1")

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "general_operand" "g")
		(match_operand:SI 1 "general_operand" "g")))]
  ""
  "bitl %0,%1")

(define_insn ""
  [(set (cc0)
	(and:HI (match_operand:HI 0 "general_operand" "g")
		(match_operand:HI 1 "general_operand" "g")))]
  ""
  "bitw %0,%1")

(define_insn ""
  [(set (cc0)
	(and:QI (match_operand:QI 0 "general_operand" "g")
		(match_operand:QI 1 "general_operand" "g")))]
  ""
  "bitb %0,%1")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(match_operand:DF 1 "general_operand" "gF"))]
  ""
  "*
{
  if (operands[1] == dconst0_rtx)
    return \"clr%# %0\";
  return \"mov%# %1,%0\";
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(match_operand:SF 1 "general_operand" "gF"))]
  ""
  "*
{
  if (operands[1] == fconst0_rtx)
    return \"clrf %0\";
  return \"movf %1,%0\";
}")

;; Some vaxes don't support this instruction.
;;(define_insn "movti"
;;  [(set (match_operand:TI 0 "general_operand" "=g")
;;	(match_operand:TI 1 "general_operand" "g"))]
;;  ""
;;  "movh %1,%0")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(match_operand:DI 1 "general_operand" "g"))]
  ""
  "movq %1,%0")

;; This handles constants which are not legitimate
;; for the sake of shared libraries on VMS.
(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "" "i"))]
  "CONSTANT_P (operands[1]) && ! LEGITIMATE_CONSTANT_P (operands[1])"
  "*
{
  operands[2] = XEXP (XEXP (operands[1], 0), 0);
  operands[1] = XEXP (XEXP (operands[1], 0), 1);
  return \"movl %2,%0\;addl2 %1,%0\";
}")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "supergeneral_operand" "g"))]
  ""
  "*
{
  rtx link;
  if (operands[1] == const1_rtx
      && (link = find_reg_note (insn, REG_WAS_0, 0))
      /* Make sure the insn that stored the 0 is still present.  */
      && ! XEXP (link, 0)->volatil
      && GET_CODE (XEXP (link, 0)) != NOTE
      /* Make sure cross jumping didn't happen here.  */
      && no_labels_between_p (XEXP (link, 0), insn))
    /* Fastest way to change a 0 to a 1.  */
    return \"incl %0\";
  if (GET_CODE (operands[1]) == SYMBOL_REF || GET_CODE (operands[1]) == CONST)
    {
      if (push_operand (operands[0], SImode))
	return \"pushab %a1\";
      return \"movab %a1,%0\";
    }
  /* this is slower than a movl, except when pushing an operand */
  if (operands[1] == const0_rtx)
    return \"clrl %0\";
  if (GET_CODE (operands[1]) == CONST_INT
      && (unsigned) INTVAL (operands[1]) >= 64)
    {
      int i = INTVAL (operands[1]);
      if ((unsigned)(~i) < 64)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, ~i);
	  return \"mcoml %1,%0\";
	}
      if ((unsigned)i < 127)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, 63);
	  operands[2] = gen_rtx (CONST_INT, VOIDmode, i-63);
	  return \"addl3 %2,%1,%0\";
	}
      /* trading speed for space */
      if ((unsigned)i < 0x100)
	return \"movzbl %1,%0\";
      if (i >= -0x80 && i < 0)
	return \"cvtbl %1,%0\";
      if ((unsigned)i < 0x10000)
	return \"movzwl %1,%0\";
      if (i >= -0x8000 && i < 0)
	return \"cvtwl %1,%0\";
    }
  if (push_operand (operands[0], SImode))
    return \"pushl %1\";
  return \"movl %1,%0\";
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(match_operand:HI 1 "general_operand" "g"))]
  ""
  "*
{
  rtx link;
  if (operands[1] == const1_rtx
      && (link = find_reg_note (insn, REG_WAS_0, 0))
      /* Make sure the insn that stored the 0 is still present.  */
      && ! XEXP (link, 0)->volatil
      && GET_CODE (XEXP (link, 0)) != NOTE
      /* Make sure cross jumping didn't happen here.  */
      && no_labels_between_p (XEXP (link, 0), insn))
    /* Fastest way to change a 0 to a 1.  */
    return \"incw %0\";
  if (operands[1] == const0_rtx)
    return \"clrw %0\";
  if (GET_CODE (operands[1]) == CONST_INT
      && (unsigned) INTVAL (operands[1]) >= 64)
    {
      int i = INTVAL (operands[1]);
      if ((unsigned)((~i) & 0xffff) < 64)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, (~i) & 0xffff);
	  return \"mcomw %1,%0\";
	}
      if ((unsigned)(i & 0xffff) < 127)
	{
	   operands[1] = gen_rtx (CONST_INT, VOIDmode, 63);
	   operands[2] = gen_rtx (CONST_INT, VOIDmode, (i-63) & 0xffff);
	   return \"addw3 %2,%1,%0\";
	}
      /* this is a lot slower, and only saves 1 measly byte! */
      /* if ((unsigned)i < 0x100)
	   return \"movzbw %1,%0\"; */
      /* if (i >= -0x80 && i < 0)
	   return \"cvtbw %1,%0\"; */
    }
  return \"movw %1,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
  if (operands[1] == const0_rtx)
    return \"clrb %0\";
  if (GET_CODE (operands[1]) == CONST_INT
      && (unsigned) INTVAL (operands[1]) >= 64)
    {
      int i = INTVAL (operands[1]);
      if ((unsigned)((~i) & 0xff) < 64)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, (~i) & 0xff);
	  return \"mcomb %1,%0\";
	}
#if 0
      /* ASCII alphabetics */
      if (((unsigned) INTVAL (operands[1]) &0xff) < 127)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, 63);
	  operands[2] = gen_rtx (CONST_INT, VOIDmode, i-63);
	  return \"addb3 %2,%1,%0\";
	}
#endif
    }
  return \"movb %1,%0\";
}")

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it won't successfully combine with anything.
(define_insn "movstrhi"
  [(set (match_operand:BLK 0 "general_operand" "=g")
	(match_operand:BLK 1 "general_operand" "g"))
   (use (match_operand:HI 2 "general_operand" "g"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SI 3))
   (clobber (reg:SI 4))
   (clobber (reg:SI 5))]
  ""
  "movc3 %2,%1,%0")

;; Extension and truncation insns.
;; Those for integer source operand
;; are ordered widest source type first.

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlb %1,%0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(truncate:HI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlw %1,%0")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwb %1,%0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwl %1,%0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtbw %1,%0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtbl %1,%0")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float_extend:DF (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cvtf%# %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float_truncate:SF (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cvt%#f %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "movzwl %1,%0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "movzbw %1,%0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extend:SI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "movzbl %1,%0")

;; Fix-to-float conversion insns.
;; Note that the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlf %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtl%# %1,%0")

(define_insn "floathisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwf %1,%0")

(define_insn "floathidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtw%# %1,%0")

(define_insn "floatqisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtbf %1,%0")

(define_insn "floatqidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtb%# %1,%0")

;; Float-to-fix conversion insns.

(define_insn "fix_truncsfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(fix:QI (fix:SF (match_operand:SF 1 "general_operand" "gF"))))]
  ""
  "cvtfb %1,%0")

(define_insn "fix_truncsfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(fix:HI (fix:SF (match_operand:SF 1 "general_operand" "gF"))))]
  ""
  "cvtfw %1,%0")

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (fix:SF (match_operand:SF 1 "general_operand" "gF"))))]
  ""
  "cvtfl %1,%0")

(define_insn "fix_truncdfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(fix:QI (fix:DF (match_operand:DF 1 "general_operand" "gF"))))]
  ""
  "cvt%#b %1,%0")

(define_insn "fix_truncdfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(fix:HI (fix:DF (match_operand:DF 1 "general_operand" "gF"))))]
  ""
  "cvt%#w %1,%0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (fix:DF (match_operand:DF 1 "general_operand" "gF"))))]
  ""
  "cvt%#l %1,%0")

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(plus:DF (match_operand:DF 1 "general_operand" "gF")
		 (match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"add%#2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"add%#2 %1,%0\";
  return \"add%#3 %1,%2,%0\";
}")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(plus:SF (match_operand:SF 1 "general_operand" "gF")
		 (match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"addf2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addf2 %1,%0\";
  return \"addf3 %1,%2,%0\";
}")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(plus:SI (match_operand:SI 1 "general_operand" "g")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incl %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && INTVAL (operands[2]) == -1)
	return \"decl %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subl2 $%n2,%0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) INTVAL (operands[2]) >= 64
	  && GET_CODE (operands[1]) == REG)
	return \"movab %c2(%1),%0\";
      return \"addl2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addl2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subl3 $%n2,%1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) INTVAL (operands[2]) >= 64
      && GET_CODE (operands[1]) == REG)
    {
      if (push_operand (operands[0], SImode))
	return \"pushab %c2(%1)\";
      return \"movab %c2(%1),%0\";
    }
  return \"addl3 %1,%2,%0\";
}")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(plus:HI (match_operand:HI 1 "general_operand" "g")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incw %0\";
      if (GET_CODE (operands[1]) == CONST_INT
	  && INTVAL (operands[1]) == -1)
	return \"decw %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subw2 $%n2,%0\";
      return \"addw2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addw2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subw3 $%n2,%1,%0\";
  return \"addw3 %1,%2,%0\";
}")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(plus:QI (match_operand:QI 1 "general_operand" "g")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incb %0\";
      if (GET_CODE (operands[1]) == CONST_INT
	  && INTVAL (operands[1]) == -1)
	return \"decb %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subb2 $%n2,%0\";
      return \"addb2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addb2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subb3 $%n2,%1,%0\";
  return \"addb3 %1,%2,%0\";
}")

;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(minus:DF (match_operand:DF 1 "general_operand" "gF")
		  (match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"sub%#2 %2,%0\";
  return \"sub%#3 %2,%1,%0\";
}")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(minus:SF (match_operand:SF 1 "general_operand" "gF")
		  (match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"subf2 %2,%0\";
  return \"subf3 %2,%1,%0\";
}")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(minus:SI (match_operand:SI 1 "general_operand" "g")
		  (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"decl %0\";
      return \"subl2 %2,%0\";
    }
  return \"subl3 %2,%1,%0\";
}")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(minus:HI (match_operand:HI 1 "general_operand" "g")
		  (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"decw %0\";
      return \"subw2 %2,%0\";
    }
  return \"subw3 %2,%1,%0\";
}")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(minus:QI (match_operand:QI 1 "general_operand" "g")
		  (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"decb %0\";
      return \"subb2 %2,%0\";
    }
  return \"subb3 %2,%1,%0\";
}")

;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(mult:DF (match_operand:DF 1 "general_operand" "gF")
		 (match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mul%#2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mul%#2 %1,%0\";
  return \"mul%#3 %1,%2,%0\";
}")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(mult:SF (match_operand:SF 1 "general_operand" "gF")
		 (match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mulf2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mulf2 %1,%0\";
  return \"mulf3 %1,%2,%0\";
}")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(mult:SI (match_operand:SI 1 "general_operand" "g")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mull2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mull2 %1,%0\";
  return \"mull3 %1,%2,%0\";
}")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(mult:HI (match_operand:HI 1 "general_operand" "g")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mulw2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mulw2 %1,%0\";
  return \"mulw3 %1,%2,%0\";
}")

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(mult:QI (match_operand:QI 1 "general_operand" "g")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mulb2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mulb2 %1,%0\";
  return \"mulb3 %1,%2,%0\";
}")

;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(div:DF (match_operand:DF 1 "general_operand" "gF")
		(match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"div%#2 %2,%0\";
  return \"div%#3 %2,%1,%0\";
}")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(div:SF (match_operand:SF 1 "general_operand" "gF")
		(match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divf2 %2,%0\";
  return \"divf3 %2,%1,%0\";
}")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(div:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divl2 %2,%0\";
  return \"divl3 %2,%1,%0\";
}")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(div:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divw2 %2,%0\";
  return \"divw3 %2,%1,%0\";
}")

(define_insn "divqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(div:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divb2 %2,%0\";
  return \"divb3 %2,%1,%0\";
}")

;This is left out because it is very slow;
;we are better off programming around the "lack" of this insn.
;(define_insn "divmoddisi4"
;  [(set (match_operand:SI 0 "general_operand" "=g")
;	(div:SI (match_operand:DI 1 "general_operand" "g")
;		(match_operand:SI 2 "general_operand" "g")))
;   (set (match_operand:SI 3 "general_operand" "=g")
;	(mod:SI (match_operand:DI 1 "general_operand" "g")
;		(match_operand:SI 2 "general_operand" "g")))]
;  ""
;  "ediv %2,%1,%0,%3")

;; Bit-and on the vax is done with a clear-bits insn.
(define_expand "andsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (match_operand:SI 1 "general_operand" "g")
		(not:SI (match_operand:SI 2 "general_operand" "g"))))]
  ""
  "
{
  extern rtx expand_unop ();
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
  else
    operands[2] = expand_unop (SImode, one_cmpl_optab, operands[2], 0, 1);
}")

(define_expand "andhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (match_operand:HI 1 "general_operand" "g")
		(not:HI (match_operand:HI 2 "general_operand" "g"))))]
  ""
  "
{
  extern rtx expand_unop ();
  rtx op = operands[2];
  if (GET_CODE (op) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode,
			   ((1 << 16) - 1) & ~INTVAL (op));
  else
    operands[2] = expand_unop (HImode, one_cmpl_optab, op, 0, 1);
}")

(define_expand "andqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (match_operand:QI 1 "general_operand" "g")
		(not:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "
{
  extern rtx expand_unop ();
  rtx op = operands[2];
  if (GET_CODE (op) == CONST_INT)
    operands[2] = gen_rtx (CONST_INT, VOIDmode,
			   ((1 << 8) - 1) & ~INTVAL (op));
  else
    operands[2] = expand_unop (QImode, one_cmpl_optab, op, 0, 1);
}")

(define_insn "andcbsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (match_operand:SI 1 "general_operand" "g")
		(not:SI (match_operand:SI 2 "general_operand" "g"))))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicl2 %2,%0\";
  return \"bicl3 %2,%1,%0\";
}")

(define_insn "andcbhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (match_operand:HI 1 "general_operand" "g")
		(not:HI (match_operand:HI 2 "general_operand" "g"))))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicw2 %2,%0\";
  return \"bicw3 %2,%1,%0\";
}")

(define_insn "andcbqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (match_operand:QI 1 "general_operand" "g")
		(not:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicb2 %2,%0\";
  return \"bicb3 %2,%1,%0\";
}")

;; The following are needed because constant propagation can
;; create them starting from the bic insn patterns above.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT"
  "*
{ operands[2] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
  if (rtx_equal_p (operands[1], operands[0]))
    return \"bicl2 %2,%0\";
  return \"bicl3 %2,%1,%0\";
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT"
  "*
{ operands[2] = gen_rtx (CONST_INT, VOIDmode, 0xffff & ~INTVAL (operands[2]));
  if (rtx_equal_p (operands[1], operands[0]))
    return \"bicw2 %2,%0\";
  return \"bicw3 %2,%1,%0\";
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT"
  "*
{ operands[2] = gen_rtx (CONST_INT, VOIDmode, 0xff & ~INTVAL (operands[2]));
  if (rtx_equal_p (operands[1], operands[0]))
    return \"bicb2 %2,%0\";
  return \"bicb3 %2,%1,%0\";
}")

;;- Bit set instructions.

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ior:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bisl2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"bisl2 %1,%0\";
  return \"bisl3 %2,%1,%0\";
}")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ior:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bisw2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"bisw2 %1,%0\";
  return \"bisw3 %2,%1,%0\";
}")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ior:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bisb2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"bisb2 %1,%0\";
  return \"bisb3 %2,%1,%0\";
}")

;;- xor instructions.

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(xor:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"xorl2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"xorl2 %1,%0\";
  return \"xorl3 %2,%1,%0\";
}")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(xor:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"xorw2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"xorw2 %1,%0\";
  return \"xorw3 %2,%1,%0\";
}")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(xor:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"xorb2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"xorb2 %1,%0\";
  return \"xorb3 %2,%1,%0\";
}")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(neg:DF (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "mneg%# %1,%0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(neg:SF (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "mnegf %1,%0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(neg:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "mnegl %1,%0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(neg:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "mnegw %1,%0")

(define_insn "negqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(neg:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "mnegb %1,%0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(not:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "mcoml %1,%0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(not:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "mcomw %1,%0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(not:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "mcomb %1,%0")

;; Arithmetic right shift on the vax works by negating the shift count.
(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashift:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  operands[2] = negate_rtx (QImode, operands[2]);
}")

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashift:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (operands[2] == const1_rtx && rtx_equal_p (operands[0], operands[1]))
    return \"addl2 %0,%0\";
  if (GET_CODE (operands[1]) == REG
      && GET_CODE (operands[2]) == CONST_INT)
    {
      int i = INTVAL (operands[2]);
      if (i == 1)
	return \"addl3 %1,%1,%0\";
      if (i == 2)
	return \"moval 0[%1],%0\";
      if (i == 3)
	return \"movad 0[%1],%0\";
    }
  return \"ashl %2,%1,%0\";
}")

;; Arithmetic right shift on the vax works by negating the shift count.
(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(ashift:DI (match_operand:DI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  operands[2] = negate_rtx (QImode, operands[2]);
}")

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(ashift:DI (match_operand:DI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "ashq %2,%1,%0")

;; Rotate right on the vax works by negating the shift count.
(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotate:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "
{
  operands[2] = negate_rtx (QImode, operands[2]);
}")

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotate:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "rotl %2,%1,%0")

;This insn is probably slower than a multiply and an add.
;(define_insn ""
;  [(set (match_operand:SI 0 "general_operand" "=g")
;	(mult:SI (plus:SI (match_operand:SI 1 "general_operand" "g")
;			  (match_operand:SI 2 "general_operand" "g"))
;		 (match_operand:SI 3 "general_operand" "g")))]
;  ""
;  "index %1,$0x80000000,$0x7fffffff,%3,%2,%0")

;; Special cases of bit-field insns which we should
;; recognize in preference to the general case.
;; These handle aligned 8-bit and 16-bit fields,
;; which can usually be done with move instructions.

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "general_operand" "+ro")
			 (match_operand:SI 1 "immediate_operand" "i")
			 (match_operand:SI 2 "immediate_operand" "i"))
	(match_operand:SI 3 "general_operand" "g"))]
   "GET_CODE (operands[1]) == CONST_INT
   && (INTVAL (operands[1]) == 8 || INTVAL (operands[1]) == 16)
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) % INTVAL (operands[1]) == 0
   && (GET_CODE (operands[0]) == REG
       || ! mode_dependent_address_p (XEXP (operands[0], 0)))"
  "*
{
  if (REG_P (operands[0]))
    {
      if (INTVAL (operands[2]) != 0)
	return \"insv %3,%2,%1,%0\";
    }
  else
    operands[0]
      = adj_offsettable_operand (operands[0], INTVAL (operands[2]) / 8);

  if (INTVAL (operands[1]) == 8)
    return \"movb %3,%0\";
  return \"movw %3,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=&g")
	(zero_extract:SI (match_operand:SI 1 "general_operand" "ro")
			 (match_operand:SI 2 "immediate_operand" "i")
			 (match_operand:SI 3 "immediate_operand" "i")))]
   "GET_CODE (operands[2]) == CONST_INT
   && (INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && GET_CODE (operands[3]) == CONST_INT
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (GET_CODE (operands[1]) == REG
       || ! mode_dependent_address_p (XEXP (operands[1], 0)))"
  "*
{
  if (REG_P (operands[1]))
    {
      if (INTVAL (operands[3]) != 0)
	return \"extzv %3,%2,%1,%0\";
    }
  else
    operands[1]
      = adj_offsettable_operand (operands[1], INTVAL (operands[3]) / 8);

  if (INTVAL (operands[2]) == 8)
    return \"movzbl %1,%0\";
  return \"movzwl %1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "general_operand" "ro")
			 (match_operand:SI 2 "immediate_operand" "i")
			 (match_operand:SI 3 "immediate_operand" "i")))]
   "GET_CODE (operands[2]) == CONST_INT
   && (INTVAL (operands[2]) == 8 || INTVAL (operands[2]) == 16)
   && GET_CODE (operands[3]) == CONST_INT
   && INTVAL (operands[3]) % INTVAL (operands[2]) == 0
   && (GET_CODE (operands[1]) == REG
       || ! mode_dependent_address_p (XEXP (operands[1], 0)))"
  "*
{
  if (REG_P (operands[1]))
    {
      if (INTVAL (operands[3]) != 0)
	return \"extv %3,%2,%1,%0\";
    }
  else
    operands[1]
      = adj_offsettable_operand (operands[1], INTVAL (operands[3]) / 8);

  if (INTVAL (operands[2]) == 8)
    return \"cvtbl %1,%0\";
  return \"cvtwl %1,%0\";
}")

;; Register-only SImode cases of bit-field insns.

(define_insn ""
  [(set (cc0)
	(compare
	 (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			  (match_operand:SI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpv %2,%1,%0,%3")

(define_insn ""
  [(set (cc0)
	(compare
	 (zero_extract:SI (match_operand:SI 0 "general_operand" "r")
			  (match_operand:SI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpzv %2,%1,%0,%3")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "general_operand" "r")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extv %3,%2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extract:SI (match_operand:SI 1 "general_operand" "r")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extzv %3,%2,%1,%0")

;; Non-register cases.
;; nonimmediate_operand is used to make sure that mode-ambiguous cases
;; don't match these (and therefore match the cases above instead).

(define_insn ""
  [(set (cc0)
	(compare
	 (sign_extract:SI (match_operand:QI 0 "nonimmediate_operand" "rm")
			  (match_operand:SI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpv %2,%1,%0,%3")

(define_insn ""
  [(set (cc0)
	(compare
	 (zero_extract:SI (match_operand:QI 0 "nonimmediate_operand" "rm")
			  (match_operand:SI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpzv %2,%1,%0,%3")

(define_insn "extv"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:QI 1 "nonimmediate_operand" "rm")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extv %3,%2,%1,%0")

(define_insn "extzv"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extract:SI (match_operand:QI 1 "nonimmediate_operand" "rm")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extzv %3,%2,%1,%0")

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:QI 0 "general_operand" "+g")
			 (match_operand:SI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g"))
	(match_operand:SI 3 "general_operand" "g"))]
  ""
  "insv %3,%2,%1,%0")

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
			 (match_operand:SI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g"))
	(match_operand:SI 3 "general_operand" "g"))]
  ""
  "insv %3,%2,%1,%0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jbr %l0")

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jeql %l0")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jneq %l0")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgtr %l0")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgtru %l0")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlss %l0")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlssu %l0")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgeq %l0")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgequ %l0")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jleq %l0")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlequ %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jneq %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jeql %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jleq %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlequ %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgeq %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgequ %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlss %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlssu %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgtr %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgtru %l0")

;; Recognize jlbs and jlbc insns.
;; These come before the jbc and jbs recognizers so these will be preferred.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbs %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbs %0,%l1")

;; These four entries allow a jlbc or jlbs to be made
;; by combination with a bic.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbs %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jlbs %0,%l1")

;; Recognize jbs and jbc instructions.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  "GET_CODE (operands[1]) == CONST_INT
   && exact_log2 (INTVAL (operands[1])) >= 0
   && (GET_CODE (operands[0]) != MEM
       || ! mode_dependent_address_p (XEXP (operands[0], 0)))"
  "*
{
  operands[1]
    = gen_rtx (CONST_INT, VOIDmode, exact_log2 (INTVAL (operands[1])));
  return \"jbs %1,%0,%l2\";
}")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  "GET_CODE (operands[1]) == CONST_INT
   && exact_log2 (INTVAL (operands[1])) >= 0
   && (GET_CODE (operands[0]) != MEM
       || ! mode_dependent_address_p (XEXP (operands[0], 0)))"
  "*
{
  operands[1]
    = gen_rtx (CONST_INT, VOIDmode, exact_log2 (INTVAL (operands[1])));
  return \"jbc %1,%0,%l2\";
}")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  "GET_CODE (operands[1]) == CONST_INT
   && exact_log2 (INTVAL (operands[1])) >= 0
   && (GET_CODE (operands[0]) != MEM
       || ! mode_dependent_address_p (XEXP (operands[0], 0)))"
  "*
{
  operands[1]
    = gen_rtx (CONST_INT, VOIDmode, exact_log2 (INTVAL (operands[1])));
  return \"jbc %1,%0,%l2\";
}")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  "GET_CODE (operands[1]) == CONST_INT
   && exact_log2 (INTVAL (operands[1])) >= 0
   && (GET_CODE (operands[0]) != MEM
       || ! mode_dependent_address_p (XEXP (operands[0], 0)))"
  "*
{
  operands[1]
    = gen_rtx (CONST_INT, VOIDmode, exact_log2 (INTVAL (operands[1])));
  return \"jbs %1,%0,%l2\";
}")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  "GET_CODE (operands[0]) != MEM
   || ! mode_dependent_address_p (XEXP (operands[0], 0))"
  "jbs %1,%0,%l2")

;; Subtract-and-jump and Add-and-jump insns.
;; These are not used when output is for the Unix assembler
;; because it does not know how to modify them to reach far.

;; Normal sob insns.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (gt (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "jsobgtr %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ge (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "jsobgeq %0,%l1")

;; Reversed sob insns.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (le (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "jsobgtr %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (lt (plus:SI (match_operand:SI 0 "general_operand" "+g")
		      (const_int -1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  "!TARGET_UNIX_ASM"
  "jsobgeq %0,%l1")

;; Normal aob insns.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (lt (compare (plus:SI (match_operand:SI 0 "general_operand" "+g")
			       (const_int 1))
		      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaoblss %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (le (compare (plus:SI (match_operand:SI 0 "general_operand" "+g")
			       (const_int 1))
		      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaobleq %1,%0,%l2")

;; Reverse aob insns.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (ge (compare (plus:SI (match_operand:SI 0 "general_operand" "+g")
			       (const_int 1))
		      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaoblss %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (gt (compare (plus:SI (match_operand:SI 0 "general_operand" "+g")
			       (const_int 1))
		      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaobleq %1,%0,%l2")

;; Something like a sob insn, but compares against -1.
;; This finds `while (foo--)' which was changed to `while (--foo != -1)'.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (compare (plus:SI (match_operand:SI 0 "general_operand" "g")
			       (const_int -1))
		      (const_int -1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int -1)))]
  ""
  "decl %0\;jgequ %l1")

;; Note that operand 1 is total size of args, in bytes,
;; and what the call insn wants is the number of words.
(define_insn "call"
  [(call (match_operand:QI 0 "general_operand" "g")
	 (match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
  if (INTVAL (operands[1]) > 255 * 4)
    /* Vax `calls' really uses only one byte of #args, so pop explicitly.  */
    return \"calls $0,%0\;addl2 %1,sp\";
  operands[1] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[1]) + 3)/ 4);
  return \"calls %1,%0\";
")

(define_insn "call_value"
  [(set (match_operand 0 "" "=g")
	(call (match_operand:QI 1 "general_operand" "g")
	      (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
  if (INTVAL (operands[2]) > 255 * 4)
    /* Vax `calls' really uses only one byte of #args, so pop explicitly.  */
    return \"calls $0,%1\;addl2 %2,sp\";
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[2]) + 3)/ 4);
  return \"calls %2,%1\";
")

(define_insn "return"
  [(return)]
  ""
  "ret")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_insn "casesi"
  [(set (pc)
	(if_then_else (le (minus:SI (match_operand:SI 0 "general_operand" "g")
				    (match_operand:SI 1 "general_operand" "g"))
			  (match_operand:SI 2 "general_operand" "g"))
		      (plus:SI (sign_extend:SI
				(mem:HI (plus:SI (pc)
						 (minus:SI (match_dup 0)
							   (match_dup 1)))))
			       (label_ref:SI (match_operand 3 "" "")))
		      (pc)))]
  ""
  "casel %0,%1,%2")

;; This used to arise from the preceding by simplification
;; if operand 1 is zero.  Perhaps it is no longer necessary.
(define_insn ""
  [(set (pc)
	(if_then_else (le (match_operand:SI 0 "general_operand" "g")
			  (match_operand:SI 1 "general_operand" "g"))
		      (plus:SI (sign_extend:SI
				(mem:HI (plus:SI (pc)
						 (minus:SI (match_dup 0)
							   (const_int 0)))))
			       (label_ref:SI (match_operand 3 "" "")))
		      (pc)))]
  ""
  "casel %0,$0,%1")

;; This arises from the preceding by simplification if operand 1 is zero.
(define_insn ""
  [(set (pc)
	(if_then_else (le (match_operand:SI 0 "general_operand" "g")
			  (match_operand:SI 1 "general_operand" "g"))
		      (plus:SI (sign_extend:SI
				(mem:HI (plus:SI (pc)
						 (match_dup 0))))
			       (label_ref:SI (match_operand 3 "" "")))
		      (pc)))]
  ""
  "casel %0,$0,%1")

;; This arises from casesi if operand 0 is a constant, in range.
(define_insn ""
  [(set (pc)
	(plus:SI (sign_extend:SI
		  (mem:HI (plus:SI (pc)
				   (match_operand:SI 0 "general_operand" "g"))))
		 (label_ref:SI (match_operand 3 "" ""))))]
  ""
  "casel %0,$0,%0")

;; This arises from the above if both operands are the same.
(define_insn ""
  [(set (pc)
        (plus:SI (sign_extend:SI (mem:HI (pc)))
		 (label_ref:SI (match_operand 3 "" ""))))]
  ""
  "casel $0,$0,$0")

;;- load or push effective address 
;; These come after the move and add/sub patterns
;; because we don't want pushl $1 turned into pushad 1.
;; or addl3 r1,r2,r3 turned into movab 0(r1)[r2],r3.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:QI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushab %a1\";
  return \"movab %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:HI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushaw %a1\";
  return \"movaw %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushal %a1\";
  return \"moval %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SF 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushaf %a1\";
  return \"movaf %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:DF 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushad %a1\";
  return \"movad %a1,%0\";
}")

;; Optimize   extzv ...,z;    andl2 ...,z
;; with other operands constant.
(define_peephole
  [(set (match_operand:SI 0 "general_operand" "g")
	(zero_extract:SI (match_operand:SI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))
   (set (match_operand:SI 4 "general_operand" "g")
	(and:SI (match_dup 0)
		(match_operand:SI 5 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT
   && GET_CODE (operands[3]) == CONST_INT
   && (INTVAL (operands[2]) + INTVAL (operands[3])) == 32
   && GET_CODE (operands[5]) == CONST_INT
   && dead_or_set_p (insn, operands[0])"
  "*
{
  unsigned long mask = INTVAL (operands[5]);
  operands[3] = gen_rtx (CONST_INT, VOIDmode, -INTVAL (operands[3]));

  if ((floor_log2 (mask) + 1) >= INTVAL (operands[2]))
    mask &= ((1 << INTVAL (operands[2])) - 1);

  operands[5] = gen_rtx (CONST_INT, VOIDmode, ~mask);
  if (push_operand (operands[4], SImode))
    {
      output_asm_insn (\"rotl %3,%1,%0\", operands);
      return \"bicl3 %5,%0,%4\";
    }
  else
    {
      output_asm_insn (\"rotl %3,%1,%4\", operands);
      return \"bicl2 %5,%4\";
    }
}")

;; Optimize   andl3 x,y,z; extzv z,....,z

(define_peephole
  [(set (match_operand:SI 0 "general_operand" "g")
	(and:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))
   (set (match_operand 3 "general_operand" "g")
	(zero_extract:SI (match_dup 0)
			 (match_operand:SI 4 "general_operand" "g")
			 (match_operand:SI 5 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT
   && GET_CODE (operands[4]) == CONST_INT
   && GET_CODE (operands[5]) == CONST_INT
   && (INTVAL (operands[4]) + INTVAL (operands[5])) == 32
   && dead_or_set_p (insn, operands[0])"
  "*
{
  unsigned long mask = INTVAL (operands[2]);

  mask &= ~((1 << INTVAL (operands[5])) - 1);
  operands[2] = gen_rtx (CONST_INT, VOIDmode, ~mask);

  operands[5] = gen_rtx (CONST_INT, VOIDmode, -INTVAL (operands[5]));

  if (rtx_equal_p (operands[0], operands[1]))
    output_asm_insn (\"bicl2 %2,%0\", operands);
  else
    output_asm_insn (\"bicl3 %2,%1,%0\", operands);
  return \"rotl %5,%0,%3\";
}")

;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:
