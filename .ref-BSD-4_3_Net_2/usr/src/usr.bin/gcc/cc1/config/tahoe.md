;;- Machine description for GNU compiler
;;- Tahoe version
;;   Copyright (C) 1989 Free Software Foundation, Inc.

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


; File: tahoe.md
;
; This port made at the University of Buffalo by Devon Bowen,
; Dale Wiles and Kevin Zachmann.
;
; Mail bugs reports or fixes to:	gcc@cs.buffalo.edu


; movdi must call the output_move_double routine to move it around since
; the tahoe doesn't efficiently support 8 bit moves.

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(match_operand:DI 1 "general_operand" "g"))]
  ""
  "*
{
  CC_STATUS_INIT;
  return output_move_double (operands);
}")


; The trick in the movsi is accessing the contents of the sp register.  The
; tahoe doesn't allow you to access it directly so you have to access the
; address of the top of the stack instead.

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "general_operand" "g"))]
  ""
  "*
{
  rtx link;
  if (operands[1] == const1_rtx
      && (link = find_reg_note (insn, REG_WAS_0, 0))
      && ! XEXP (link, 0)->volatil
      && GET_CODE (XEXP (link, 0)) != NOTE
      && no_labels_between_p (XEXP (link, 0), insn))
    return \"incl %0\";
  if (GET_CODE (operands[1]) == SYMBOL_REF || GET_CODE (operands[1]) == CONST)
    {
      if (push_operand (operands[0], SImode))
	return \"pushab %a1\";
      return \"movab %a1,%0\";
    }
  if (operands[1] == const0_rtx)
    return \"clrl %0\";
  if (push_operand (operands[0], SImode))
    return \"pushl %1\";
  if (GET_CODE (operands[1]) == REG && REGNO (operands[1]) == 14)
    return \"moval (sp),%0\";
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
      && ! XEXP (link, 0)->volatil
      && GET_CODE (XEXP (link, 0)) != NOTE
      && no_labels_between_p (XEXP (link, 0), insn))
    return \"incw %0\";
  if (operands[1] == const0_rtx)
    return \"clrw %0\";
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
  return \"movb %1,%0\";
}")


; movsf has three cases since they can move from one place to another
; or to/from the fpp and since different instructions are needed for
; each case.  The fpp related instructions don't set the flags properly.

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=g,=a,=g")
	(match_operand:SF 1 "general_operand" "g,g,a"))]
  ""
  "*
{
  CC_STATUS_INIT;
  switch (which_alternative)
    {
    case 0:
      return \"movl %1,%0\";
    case 1:
      return \"ldf %1\";
    case 2:
      return \"stf %0\";
    }
}")


; movdf has a number of different cases.  If it's going to or from
; the fpp, use the special instructions to do it.  If not, use the
; output_move_double function.

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=a,=g,?=g")
	(match_operand:DF 1 "general_operand" "g,a,g"))]
  ""
  "*
{
  CC_STATUS_INIT;
  switch (which_alternative)
    {
    case 0:
      return \"ldd %1\";
    case 1:
      if (push_operand (operands[0], DFmode))
	return \"pushd\";
      else
	return \"std %0\";
    case 2:
      return output_move_double (operands);
    }
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
      return \"addl2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addl2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && GET_CODE (operands[1]) == REG)
    {
      if (push_operand (operands[0], SImode))
        return \"pushab %c2(%1)\";
      return \"movab %c2(%1),%0\";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subl3 $%n2,%1,%0\";
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


; addsf3 can only add into the fpp register since the fpp is treated
; as a separate unit in the machine.  It also doesn't set the flags at
; all.

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=a")
	(plus:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "general_operand" "g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"addf %2\";
}")


; adddf3 can only add into the fpp reg since the fpp is treated as a
; separate entity.  Doubles can only be read from a register or memory
; since a double is not an immediate mode.  Flags are not set by this
; instruction.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(plus:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "general_operand" "rm")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"addd %2\";
}")


; Subtraction from the sp (needed by the built in alloc funtion) needs
; to be different since the sp cannot be directly read on the tahoe.
; If it's a simple constant, you just use displacment.  Otherwise, you
; push the sp, and then do the subtraction off the stack.

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
      if (GET_CODE (operands[0]) == REG && REGNO (operands[0]) == 14)
	if (GET_CODE (operands[2]) == CONST_INT)
	  return \"movab %n2(sp),sp\";
	else
	  return \"pushab (sp)\;subl3 %2,(sp),sp\";
      return \"subl2 %2,%0\";
    }
  if (rtx_equal_p (operands[1], operands[2]))
    return \"clrl %0\";
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
  if (rtx_equal_p (operands[1], operands[2]))
    return \"clrw %0\";
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
  if (rtx_equal_p (operands[1], operands[2]))
    return \"clrb %0\";
  return \"subb3 %2,%1,%0\";
}")


; subsf3 can only subtract into the fpp accumulator due to the way
; the fpp reg is limited by the instruction set.  This also doesn't
; bother setting up flags.

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=a")
	(minus:SF (match_operand:SF 1 "register_operand" "0")
		  (match_operand:SF 2 "general_operand" "g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"subf %2\";
}")


; subdf3 is set up to subtract into the fpp reg due to limitations
; of the fpp instruction set.  Doubles can not be immediate.  This
; instruction does not set the flags.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(minus:DF (match_operand:DF 1 "register_operand" "0")
		  (match_operand:DF 2 "general_operand" "rm")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"subd %2\";
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


; mulsf3 can only multiply into the fpp accumulator due to limitations
; of the fpp.  It also does not set the condition codes properly.

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=a")
	(mult:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "general_operand" "g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"mulf %2\";
}")


; muldf3 can only multiply into the fpp reg since the fpp is limited
; from the rest.  Doubles may not be immediate mode.  This does not set
; the flags like GCC would expect.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(mult:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "general_operand" "rm")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"muld %2\";
}")


(define_insn "divsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(div:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[1], operands[2]))
    return \"movl $1,%0\";
  if (operands[1] == const0_rtx)
    return \"clrl %0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) == -1)
    return \"mnegl %1,%0\";
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divl2 %2,%0\";
  return \"divl3 %2,%1,%0\";
}")


; divsf3 must divide into the fpp accumulator.  Flags are not set by
; this instruction, so they are cleared.

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=a")
	(div:SF (match_operand:SF 1 "register_operand" "0")
		(match_operand:SF 2 "general_operand" "g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"divf %2\";
}")


; divdf3 also must divide into the fpp reg so optimization isn't
; possible.  Note that doubles cannot be immediate.  The flags here
; are not set correctly so they must be ignored.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(div:DF (match_operand:DF 1 "register_operand" "0")
		(match_operand:DF 2 "general_operand" "rm")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"divd %2\";
}")


(define_insn "andsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "andl3 %2,%1,%0")


(define_insn "andhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "andw3 %1,%2,%0")


(define_insn "andqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "andb3 %1,%2,%0")


(define_insn "iorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ior:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "orl3 %2,%1,%0")


(define_insn "iorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ior:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "orw3 %2,%1,%0")


(define_insn "iorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ior:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "orb3 %2,%1,%0")


(define_insn "xorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(xor:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "xorl3 %1,%2,%0")


(define_insn "xorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(xor:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "xorw3 %1,%2,%0")


(define_insn "xorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(xor:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "xorb3 %1,%2,%0")


; Shifts on the tahoe are expensive, so try to do an add instead.

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashift:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (operands[2] == const1_rtx && rtx_equal_p (operands[0], operands[1]))
    {
      CC_STATUS_INIT;
      return \"addl2 %0,%0\";
    }
  return \"shal %2,%1,%0\";
}")


(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashiftrt:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "shar %2,%1,%0")


; Shifts are very expensive, so try to do an add if possible.

(define_insn "lshlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(lshift:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (operands[2] == const1_rtx && rtx_equal_p (operands[0], operands[1]))
    {
      CC_STATUS_INIT;
      return \"addl2 %0,%0\";
    }
  return \"shll %2,%1,%0\";
}")


(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(lshiftrt:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "shrl %2,%1,%0")


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


; negsf2 can only negate the value already in the fpp accumulator.
; The value remains in the fpp accumulator.  No flags are set.

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=a,=a")
	(neg:SF (match_operand:SF 1 "register_operand" "a,g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  switch (which_alternative)
    {
    case 0:
      return \"negf\";
    case 1:
      return \"lnf %1\";
    }
}")


; negdf2 can only negate the value already in the fpp accumulator.
; The value remains in the fpp accumulator.  No flags are set.

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=a,=a")
	(neg:DF (match_operand:DF 1 "register_operand" "a,g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  switch (which_alternative)
    {
    case 0:
      return \"negd\";
    case 1:
      return \"lnd %1\";
    }
}")


; sqrtsf2 tahoe can calculate the square root of a float in the
; fpp accumulator.  The answer remains in the fpp accumulator.  No
; flags are set by this function.

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=a")
	(sqrt:SF (match_operand:SF 1 "register_operand" "0")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"sqrtf\";
}")


; ffssi2 tahoe instruction gives one less than GCC desired result for
; any given input.  So the increment is necessary here.

(define_insn "ffssi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ffs:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "ffs %1,%0\;incl %0")


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


; cmpsi works fine, but due to microcode problems, the tahoe doesn't
; properly compare hi's and qi's.  Leaving them out seems to be acceptable
; to the compiler, so they were left out.  Compares of the stack are
; possible, though.

;; Put cmpsi first among compare insns so it matches two CONST_INT operands.

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "general_operand" "g")
	         (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cmpl %0,%1")


; cmpsf similar to vax, but first operand is expected to be in the
; fpp accumulator.

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "register_operand" "a")
	         (match_operand:SF 1 "general_operand" "g")))]
  ""
  "cmpf %1")


; cmpdf similar to vax, but first operand is expected to be in the
; fpp accumulator.  Immediate doubles not allowed.

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "a")
	         (match_operand:DF 1 "general_operand" "rm")))]
  ""
  "cmpd %1")


;; Put tstsi first among test insns so it matches a CONST_INT operand.

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "general_operand" "g"))]
  ""
  "tstl %0")


; Small tests from memory are normal, but testing from registers don't
; expand the data properly.  So test in this case does a convert and tests
; the new register data from the stack.

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "general_operand" "m,?r"))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"tstw %0\";
    case 1:
      return \"pushl %0\;cvtwl 2(sp),(sp)\;tstl (sp)+\";
    }
}")


; Small tests from memory are normal, but testing from registers don't
; expand the data properly.  So test in this case does a convert and tests
; the new register data from the stack.

(define_insn "tstqi"
  [(set (cc0)
	(match_operand:QI 0 "general_operand" "m,?r"))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"tstb %0\";
    case 1:
      return \"pushl %0\;cvtbl 3(sp),(sp)\;tstl (sp)+\";
    }
}")


; tstsf compares a given value to a value already in the fpp accumulator.
; No flags are set by this so ignore them.

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "register_operand" "a"))]
  ""
  "tstf")


; tstdf compares a given value to a value already in the fpp accumulator.
; immediate doubles not allowed.  Flags are ignored after this.

(define_insn "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "register_operand" "a"))]
  ""
  "tstd")


; movstrhi tahoe instruction does not load registers by itself like
; the vax counterpart does.  registers 0-2 must be primed by hand.
; we have loaded the registers in the order: dst, src, count.

(define_insn "movstrhi"
  [(set (match_operand:BLK 0 "general_operand" "p")
	 (match_operand:BLK 1 "general_operand" "p"))
   (use (match_operand:HI 2 "general_operand" "g"))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))]
  ""
  "movab %0,r1\;movab %1,r0\;movl %2,r2\;movblk")


; floatsisf2 on tahoe converts the long from reg/mem into the fpp
; accumulator.  There are no hi and qi counterparts.  Flags are not
; set correctly here.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=a")
	(float:SF (match_operand:SI 1 "general_operand" "g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"cvlf %1\";
}")


; floatsidf2 on tahoe converts the long from reg/mem into the fpp
; accumulator.  There are no hi and qi counterparts.  Flags are not
; set correctly here.

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(float:DF (match_operand:SI 1 "general_operand" "g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"cvld %1\";
}")


; fix_truncsfsi2 to convert a float to long, tahoe must have the float
; in the fpp accumulator.  Flags are not set here.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "a"))))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"cvfl %0\";
}")


; fix_truncsfsi2 to convert a double to long, tahoe must have the double
; in the fpp accumulator.  Flags are not set here.

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "a"))))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"cvdl %0\";
}")


(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(truncate:HI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlw %1,%0")


(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlb %1,%0")


(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwb %1,%0")


; The fpp related instructions don't set flags, so ignore them
; after this instruction.

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=a")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "0")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"cvdf\";
}")


; This monster is to cover for the Tahoe's nasty habit of not extending
; a number if the source is in a register.  (It just moves it!)  Case 0 is
; a normal extend from memory.  Case 1 does the extension from the top of
; the stack.  Extension from the stack doesn't set the flags right since
; the moval changes them.

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g,?=g")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "m,r")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"cvtwl %1,%0\";
    case 1:
      if (push_operand(operands[0], SImode))
	return \"pushl %1\;cvtwl 2(sp),(sp)\";
      else
	{
	  CC_STATUS_INIT;
	  return \"pushl %1\;cvtwl 2(sp),%0\;moval 4(sp),sp\";
	}
    }
}")


; This monster is to cover for the Tahoe's nasty habit of not extending
; a number if the source is in a register.  (It just moves it!)  Case 0 is
; a normal extend from memory.  Case 1 does the extension from the top of
; the stack.  Extension from the stack doesn't set the flags right since
; the moval changes them.

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g,?=g")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"cvtbl %1,%0\";
    case 1:
      if (push_operand(operands[0], SImode))
	return \"pushl %1\;cvtbl 3(sp),(sp)\";
      else
	{
	  CC_STATUS_INIT;
	  return \"pushl %1\;cvtbl 3(sp),%0\;moval 4(sp),sp\";
	}
    }
}")


; This monster is to cover for the Tahoe's nasty habit of not extending
; a number if the source is in a register.  (It just moves it!)  Case 0 is
; a normal extend from memory.  Case 1 does the extension from the top of
; the stack.  Extension from the stack doesn't set the flags right since
; the moval changes them.

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g,?=g")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"cvtbw %1,%0\";
    case 1:
      if (push_operand(operands[0], SImode))
	return \"pushl %1\;cvtbw 3(sp),2(sp)\";
      else {
	CC_STATUS_INIT;
	return \"pushl %1\;cvtbw 3(sp),%0\;moval 4(sp),sp\";
      }
    }
}")


; extendsfdf2 tahoe uses the fpp accumulator to do the extension.
; It takes a float and loads it up directly as a double.

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=a")
	(float_extend:DF (match_operand:SF 1 "general_operand" "g")))]
  ""
  "*
{
  CC_STATUS_INIT;
  return \"ldfd %1\";
}")


; movz works fine from memory but not from register for the same reasons
; the cvt instructions don't work right.  So we use the normal instruction
; from memory and we use an and to simulate it from register.  This is faster
; than pulling it off the stack.

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g,?=g")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "m,r")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"movzwl %1,%0\";
    case 1:
      return \"andl3 $0xffff,%1,%0\";
    }
}")


; movz works fine from memory but not from register for the same reasons
; the cvt instructions don't work right.  So we use the normal instruction
; from memory and we use an and to simulate it from register.  This is faster
; than pulling it off the stack.

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g,?=g")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"movzbw %1,%0\";
    case 1:
      return \"andw3 $0xff,%1,%0\";
    }
}")


; movz works fine from memory but not from register for the same reasons
; the cvt instructions don't work right.  So we use the normal instruction
; from memory and we use an and to simulate it from register.  This is faster
; than pulling it off the stack.

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g,?=g")
	(zero_extend:SI (match_operand:QI 1 "general_operand" "m,r")))]
  ""
  "*
{
  switch (which_alternative)
    {
    case 0:
      return \"movzbl %1,%0\";
    case 1:
      return \"andl3 $0xff,%1,%0\";
    }
}")


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


; GCC does not account for register mask/argc longword.  Thus the number
; for the call = number bytes for args + 4

(define_insn "call"
  [(call (match_operand:QI 0 "general_operand" "g")
	 (match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
  operands[1] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[1]) + 4));
  return \"calls %1,%0\";
}")


; GCC does not account for register mask/argc longword.  Thus the number
; for the call = number bytes for args + 4

(define_insn "call_value"
  [(set (match_operand 0 "" "=g")
	(call (match_operand:QI 1 "general_operand" "g")
	      (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[2]) + 4));
  return \"calls %2,%1\";
}")


(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")


(define_insn "return"
  [(return)]
  ""
  "ret")


; casesi, extracted from the vax code.  The instructions are
; very similar.  Tahoe requires that the table be word aligned.  GCC
; places the table immediately after, thus the alignment directive.

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
  "casel %0,%1,%2\;.align %@")


(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jbr %l0")


;; This is the list of all the non-standard insn patterns


; This is used to access the address of a byte.  This is similar to
; movqi, but the second operand had to be "address_operand" type, so
; it had to be an unnamed one.

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

; This is used to access the address of a word.  This is similar to
; movhi, but the second operand had to be "address_operand" type, so
; it had to be an unnamed one.

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

; This is used to access the address of a long.  This is similar to
; movsi, but the second operand had to be "address_operand" type, so
; it had to be an unnamed one.

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

; The tahoe doesn't have an 8 byte indexed move address command
; and GCC needs it.  To work around it, double the index (%2) and
; then use the 4 byte indexed move address command.
;
;(define_insn ""
;  [(set (match_operand:SI 0 "general_operand" "=g")
;	(plus:SI (match_operand:SI 1 "general_operand" "g")
;		 (mult:SI (match_operand:SI 2 "register_operand" "r")
;			  (const_int 8))))]
;  ""
;  "*
;{
;  if (GET_CODE (operands[0]) == REG &&
;      REGNO (operands[0]) == REGNO (operands[2])) {
;	return \"shll $3,%2,%2\;addl3 %1,%2,%0\";
;  } else {
;	return \"shll $3,%2,%2\;addl3 %1,%2,%0\;shrl $3,%2,%2\"; }
;}")


; Bit test longword instruction, same as vax.

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "general_operand" "g")
		(match_operand:SI 1 "general_operand" "g")))]
  ""
  "bitl %0,%1")


; Bit test word instructions, same as vax.

(define_insn ""
  [(set (cc0)
	(and:HI (match_operand:HI 0 "general_operand" "g")
		(match_operand:HI 1 "general_operand" "g")))]
  ""
  "bitw %0,%1")


; Bit test instructions, same as vax.

(define_insn ""
  [(set (cc0)
	(and:QI (match_operand:QI 0 "general_operand" "g")
		(match_operand:QI 1 "general_operand" "g")))]
  ""
  "bitb %0,%1")


; bne counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jneq %l0")


; beq counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jeql %l0")


; ble counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jleq %l0")


; bleu counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlequ %l0")


; bge counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgeq %l0")


; bgeu counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgequ %l0")


; blt counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlss %l0")


; bltu counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlssu %l0")


; bgt counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgtr %l0")


; bgtu counterpart.  In case GCC reverses the conditional.

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgtru %l0")


; casesi alternate form as found in vax code.  This form is to
; compensate for the table's offset being no distance (0 displacement)

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
  "casel %0,$0,%1\;.align %@")


; casesi alternate form as found in vax code.  Another form to
; compensate for the table's offset being no distance (0 displacement)

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
  "casel %0,$0,%1 \;.align %@")
