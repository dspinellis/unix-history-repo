;;- Machine description for GNU compiler
;;- Convex Version
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


;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;-
;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

;; Put tstsi first among test insns so it matches a CONST_INT operand.

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  "* return set_cmp (operands[0], const0_rtx, 'w');")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "register_operand" "r"))]
  ""
  "* return set_cmp (operands[0], const0_rtx, 'h');")

(define_expand "tstqi"
  [(set (match_dup 1)
	(sign_extend:SI (match_operand:QI 0 "register_operand" "r")))
   (set (cc0)
	(match_dup 1))]
  ""
  "operands[1] = gen_reg_rtx (SImode);")

(define_insn "tstdi"
  [(set (cc0)
	(match_operand:DI 0 "register_operand" "d"))
   (clobber (reg:DI 1))]
  ""
  "*
{
  output_asm_insn (\"ld.l #0,s1\");
  return set_cmp (operands[0], gen_rtx (REG, DImode, 1), 'l');
}")

(define_expand "tstdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "d")
		 (match_dup 1)))]
  ""
  "operands[1] = force_reg (DFmode, dconst0_rtx);")

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "register_operand" "d"))]
  ""
  "* return set_cmp (operands[0], fconst0_rtx, 's');")

;; Put cmpsi first among compare insns so it matches two CONST_INT operands.

(define_insn "cmpsi"
  [(set (cc0)
	(compare (match_operand:SI 0 "nonmemory_operand" "d,a,i,r")
		 (match_operand:SI 1 "nonmemory_operand" "d,a,r,i")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'w');")

(define_insn "cmphi"
  [(set (cc0)
	(compare (match_operand:HI 0 "nonmemory_operand" "d,a,r,i")
		 (match_operand:HI 1 "nonmemory_operand" "d,a,i,r")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'h');")

(define_insn ""
  [(set (cc0)
	(compare (sign_extend:SI (match_operand:QI 0 "register_operand" "d"))
		 (sign_extend:SI (match_operand:QI 1 "register_operand" "d"))))]
  ""
  "* return set_cmp (operands[0], operands[1], 'b');")

(define_insn "cmpdi"
  [(set (cc0)
	(compare (match_operand:DI 0 "register_operand" "d")
		 (match_operand:DI 1 "register_operand" "d")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'l');")

(define_insn "cmpdf"
  [(set (cc0)
	(compare (match_operand:DF 0 "register_operand" "d")
		 (match_operand:DF 1 "register_operand" "d")))]
  ""
  "* return set_cmp (operands[0], operands[1], 'd');")

(define_insn "cmpsf"
  [(set (cc0)
	(compare (match_operand:SF 0 "nonmemory_operand" "dF,d")
		 (match_operand:SF 1 "nonmemory_operand" "d,F")))]
  ""
  "* return set_cmp (operands[0], operands[1], 's');")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=g,d")
	(match_operand:DF 1 "general_operand" "d,gG"))]
  ""
  "*
{
  if (push_operand (operands[0], DFmode))
    return \"psh.l %1\";
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.l %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    return \"mov %1,%0\";
  else if (GET_CODE (operands[1]) == CONST_DOUBLE && LD_D_P (operands[1]))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     const_double_high_int (operands[1]));
      return \"ld.d %1,%0\";
    }
  else
    return \"ld.l %1,%0\";
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=g,d")
	(match_operand:SF 1 "general_operand" "d,gF"))]
  ""
  "*
{
  if (push_operand (operands[0], SFmode))
    return \"psh.w %1\";
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.s %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    return \"mov.s %1,%0\";
  else
    return \"ld.s %1,%0\";
}")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=g,d")
	(match_operand:DI 1 "general_operand" "d,gG"))]
  ""
  "*
{
  if (push_operand (operands[0], DImode))
    return \"psh.l %1\";
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.l %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    return \"mov %1,%0\";
  else if (GET_CODE (operands[1]) == CONST_DOUBLE && LD_D_P (operands[1]))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode,
			     const_double_high_int (operands[1]));
      return \"ld.d %1,%0\";
    }
  else
    return \"ld.l %1,%0\";
}")

;; Special case of movsi, needed to express A-reg preference.

(define_insn ""
  [(set (match_operand:SI 0 "push_operand" "=<")
	(plus:SI (match_operand:SI 1 "register_operand" "a")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] != stack_pointer_rtx"
  "pshea %a2(%1)")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=g,r,<")
	(match_operand:SI 1 "general_operand" "r,g,io"))]
  ""
  "*
{ 
  if (push_operand (operands[0], SImode))
    {
      if (GET_CODE (operands[1]) == REG)
	return \"psh.w %1\";
      else
        return \"pshea %a1\";
    }
  if (GET_CODE (operands[0]) == MEM)
    return \"st.w %1,%0\";
  if (GET_CODE (operands[1]) != REG)
    return \"ld.w %1,%0\";
  if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
    return \"mov.w %1,%0\";
  return \"mov %1,%0\";
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=g,r")
	(match_operand:HI 1 "general_operand" "r,g"))]
  ""
  "*
{
  if (push_operand (operands[0], HImode))
    abort ();
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.h %1,%0\";
  else if (GET_CODE (operands[1]) == REG) 
    {
      if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
	return \"mov.w %1,%0\";
      else
        return \"mov %1,%0\";
    }
  else if (GET_CODE (operands[1]) == CONST_INT)
    return \"ld.w %1,%0\";
  else
    return \"ld.h %1,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=g,r")
	(match_operand:QI 1 "general_operand" "r,g"))]
  ""
  "*
{
  if (push_operand (operands[0], QImode))
    abort ();
  else if (GET_CODE (operands[0]) == MEM)
    return \"st.b %1,%0\";
  else if (GET_CODE (operands[1]) == REG)
    {
      if (S_REG_P (operands[0]) && S_REG_P (operands[1]))
	return \"mov.w %1,%0\";
      else
        return \"mov %1,%0\";
    }
  else if (GET_CODE (operands[1]) == CONST_INT)
    return \"ld.w %1,%0\";
  else
    return \"ld.b %1,%0\";
}")

;; Extension and truncation insns.
;; Those for integer source operand
;; are ordered widest source type first.

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(truncate:QI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "cvtw.b %1,%0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(truncate:HI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "cvtw.h %1,%0")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(truncate:QI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "")

(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(truncate:SI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.w %1,%0")

(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(sign_extend:DI (match_operand:SI 1 "register_operand" "d")))]
  ""
  "cvtw.l %1,%0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(sign_extend:SI (match_operand:HI 1 "register_operand" "d,a")))]
  ""
  "cvth.w %1,%0")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(sign_extend:HI (match_operand:QI 1 "register_operand" "d,a")))]
  ""
  "cvtb.w %1,%0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(sign_extend:SI (match_operand:QI 1 "register_operand" "d,a")))]
  ""
  "cvtb.w %1,%0")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float_extend:DF (match_operand:SF 1 "register_operand" "d")))]
  ""
  "cvts.d %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float_truncate:SF (match_operand:DF 1 "register_operand" "d")))]
  ""
  "cvtd.s %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "register_operand" "0")))]
  ""
  "and #0xffff,%0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r")
	(zero_extend:HI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "and #0xff,%0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "register_operand" "0")))]
  ""
  "and #0xff,%0")

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(zero_extend:DI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "ld.u #0,%0")

;; Fix-to-float conversion insns.
;; Note that the ones that start with SImode come first.
;; That is so that an operand that is a CONST_INT
;; (and therefore lacks a specific machine mode).
;; will be recognized as SImode (which is always valid)
;; rather than as QImode or HImode.

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float:SF (match_operand:SI 1 "register_operand" "d")))]
  ""
  "cvtw.s %1,%0")

(define_insn "floatdisf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(float:SF (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.s %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float:DF (match_operand:SI 1 "register_operand" "d")))]
  "TARGET_C2"
  "cvtw.d %1,%0")

(define_insn "floatdidf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(float:DF (match_operand:DI 1 "register_operand" "d")))]
  ""
  "cvtl.d %1,%0")

;; Float-to-fix conversion insns.

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (fix:SF (match_operand:SF 1 "register_operand" "d"))))]
  ""
  "cvts.w %1,%0")

(define_insn "fix_truncsfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (fix:SF (match_operand:SF 1 "register_operand" "d"))))]
  ""
  "cvts.l %1,%0")

(define_insn "fix_truncdfsi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(fix:SI (fix:DF (match_operand:DF 1 "register_operand" "d"))))]
  ""
  "*
{
  if (TARGET_C2)
    return \"cvtd.w %1,%0\";
  return \"cvtd.l %1,%0\";
}")

(define_insn "fix_truncdfdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(fix:DI (fix:DF (match_operand:DF 1 "register_operand" "d"))))]
  ""
  "cvtd.l %1,%0")

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(plus:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "d")))]
  ""
  "add.d %2,%0")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(plus:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "add.s %2,%0")

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(plus:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "add.l %2,%0")

;; special case of addsi3, needed to specify an A reg for the destination 
;; when the source is a sum involving FP or AP.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=a")
	(plus:SI (match_operand:SI 1 "register_operand" "%a")
		 (match_operand:SI 2 "immediate_operand" "i")))]
  "operands[1] == frame_pointer_rtx || operands[1] == arg_pointer_rtx"
  "ldea %a2(%1),%0")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a,a")
	(plus:SI (match_operand:SI 1 "nonmemory_operand" "%0,0,a")
		 (match_operand:SI 2 "nonmemory_operand" "di,ri,i")))]
  ""
  "* switch (which_alternative) 
{
 case 0:
 case 1: 
   return \"add.w %2,%0\";
 case 2:
   if ((TARGET_C2 || A_REG_P (operands[0]))
       && operands[1] != stack_pointer_rtx)
     return \"ldea %a2(%1),%0\";
   else
     return \"mov %1,%0\;add.w %2,%0\";
}")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(plus:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "add.h %2,%0")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(plus:QI (match_operand:QI 1 "register_operand" "%0")
		 (match_operand:QI 2 "register_operand" "d")))]
  ""
  "add.b %2,%0")

;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(minus:DF (match_operand:DF 1 "register_operand" "0")
		  (match_operand:DF 2 "register_operand" "d")))]
  ""
  "sub.d %2,%0")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(minus:SF (match_operand:SF 1 "register_operand" "0")
		  (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "sub.s %2,%0")

(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(minus:DI (match_operand:DI 1 "register_operand" "0")
		  (match_operand:DI 2 "register_operand" "d")))]
  ""
  "sub.l %2,%0")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(minus:SI (match_operand:SI 1 "register_operand" "0,0")
		  (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "sub.w %2,%0")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(minus:HI (match_operand:HI 1 "register_operand" "0,0")
		  (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "sub.h %2,%0")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(minus:QI (match_operand:QI 1 "register_operand" "0")
		  (match_operand:QI 2 "register_operand" "d")))]
  ""
  "sub.b %2,%0")

;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(mult:DF (match_operand:DF 1 "register_operand" "%0")
		 (match_operand:DF 2 "register_operand" "d")))]
  ""
  "mul.d %2,%0")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(mult:SF (match_operand:SF 1 "register_operand" "%0")
		 (match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "mul.s %2,%0")

(define_insn "muldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(mult:DI (match_operand:DI 1 "register_operand" "%0")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "mul.l %2,%0")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(mult:SI (match_operand:SI 1 "register_operand" "%0,0")
		 (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "mul.w %2,%0")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(mult:HI (match_operand:HI 1 "register_operand" "%0,0")
		 (match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "mul.h %2,%0")

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(mult:QI (match_operand:QI 1 "register_operand" "%0")
		 (match_operand:QI 2 "register_operand" "d")))]
  ""
  "mul.b %2,%0")

;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(div:DF (match_operand:DF 1 "register_operand" "0")
		(match_operand:DF 2 "register_operand" "d")))]
  ""
  "div.d %2,%0")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(div:SF (match_operand:SF 1 "register_operand" "0")
		(match_operand:SF 2 "nonmemory_operand" "dF")))]
  ""
  "div.s %2,%0")

(define_insn "divdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(div:DI (match_operand:DI 1 "register_operand" "0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "div.l %2,%0")

(define_insn "udivdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(udiv:DI (match_operand:DI 1 "register_operand" "d")
		 (match_operand:DI 2 "register_operand" "d")))]
  ""
  "psh.l %2\;psh.l %1\;callq udiv64\;pop.l %0\;add.w #8,sp")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(div:SI (match_operand:SI 1 "register_operand" "0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "div.w %2,%0")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(div:HI (match_operand:HI 1 "register_operand" "0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "div.h %2,%0")

(define_insn "divqi3"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(div:QI (match_operand:QI 1 "register_operand" "0")
		(match_operand:QI 2 "register_operand" "d")))]
  ""
  "div.b %2,%0")

;; - and, or, xor

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(and:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "and %2,%0")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(and:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "and %2,%0")

(define_insn "andhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(and:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "and %2,%0")

(define_insn "andqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(and:QI (match_operand:QI 1 "register_operand" "%0,0")
		(match_operand:QI 2 "nonmemory_operand" "di,ai")))]
  ""
  "and %2,%0")

;;- Bit set instructions.

(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(ior:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "or %2,%0")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(ior:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "or %2,%0")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(ior:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "or %2,%0")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(ior:QI (match_operand:QI 1 "register_operand" "%0,0")
		(match_operand:QI 2 "nonmemory_operand" "di,ai")))]
  ""
  "or %2,%0")

;;- xor instructions.

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(xor:DI (match_operand:DI 1 "register_operand" "%0")
		(match_operand:DI 2 "register_operand" "d")))]
  ""
  "xor %2,%0")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(xor:SI (match_operand:SI 1 "register_operand" "%0,0")
		(match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "xor %2,%0")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(xor:HI (match_operand:HI 1 "register_operand" "%0,0")
		(match_operand:HI 2 "nonmemory_operand" "di,ai")))]
  ""
  "xor %2,%0")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(xor:QI (match_operand:QI 1 "register_operand" "%0,0")
		(match_operand:QI 2 "nonmemory_operand" "di,ai")))]
  ""
  "xor %2,%0")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(neg:DF (match_operand:DF 1 "register_operand" "d")))]
  ""
  "neg.d %1,%0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(neg:SF (match_operand:SF 1 "register_operand" "d")))]
  ""
  "neg.s %1,%0")

(define_insn "negdi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(neg:DI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "neg.l %1,%0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(neg:SI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "neg.w %1,%0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(neg:HI (match_operand:HI 1 "register_operand" "d,a")))]
  ""
  "neg.h %1,%0")

(define_insn "negqi2"
  [(set (match_operand:QI 0 "register_operand" "=d")
	(neg:QI (match_operand:QI 1 "register_operand" "d")))]
  ""
  "neg.b %1,%0")

(define_insn "one_cmpldi2"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(not:DI (match_operand:DI 1 "register_operand" "d")))]
  ""
  "not %1,%0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(not:SI (match_operand:SI 1 "register_operand" "d,a")))]
  ""
  "not %1,%0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "register_operand" "=d,a")
	(not:HI (match_operand:HI 1 "register_operand" "d,a")))]
  ""
  "not %1,%0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=d,a")
	(not:QI (match_operand:QI 1 "register_operand" "d,a")))]
  ""
  "not %1,%0")

;;- shifts
;;
;; Convex shift instructions are unsigned.
;; To make signed right shifts:
;; for SImode, sign extend to DImode and shift, works for 0..32
;; for DImode, shift and then extend the sign, works for 0..63
;;
;; It is very sad that DImode right shift 64 fails, but I don't see
;; any reasonable way to handle it.  ANSI only requires up to 63.

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "immediate_operand" "i")))]
  "INTVAL (operands[2]) >= 0"
  "*
{
  if (operands[2] == const1_rtx)
    return \"add.w %0,%0\";
  else if (TARGET_C2 && S_REG_P (operands[0]))
    return \"shf.w %2,%0\";
  else
    return \"shf %2,%0\";
}")

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "*
{
  if (operands[2] == const1_rtx)
    return \"add.w %0,%0\";
  else if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 0)
    return TARGET_C2 ? \"shf.w %2,%0\" : \"shf %2,%0\";
  else
    return \"cvtw.l %0,%0\;shf %2,%0\";
}")

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(ashift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

(define_insn "lshlsi3"
  [(set (match_operand:SI 0 "register_operand" "=d,a")
	(lshift:SI (match_operand:SI 1 "register_operand" "0,0")
		   (match_operand:SI 2 "nonmemory_operand" "di,ai")))]
  ""
  "*
{
  if (operands[2] == const1_rtx)
    return \"add.w %0,%0\";
  if (S_REG_P (operands[0]))
    {
      if (TARGET_C2)
	return \"shf.w %2,%0\";
      else if (GET_CODE (operands[2]) == CONST_INT
	       && INTVAL (operands[2]) >= 0)
        return \"shf %2,%0\";
      else
        return \"ld.u #0,%0\;shf %2,%0\";
    }
  return \"shf %2,%0\";
}")

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(lshift:SI (match_operand:SI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

;; signed  a >> b  is
;;     ((a >> b) ^ signbit) - signbit
;; where signbit is (1 << 63) >> b

(define_expand "ashldi3"
  [(match_operand:DI 0 "register_operand" "=d")
   (match_operand:DI 1 "register_operand" "0")
   (match_operand:SI 2 "nonmemory_operand" "di")
   (match_dup 3)]
  ""
  "
{
  if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) >= 0)
    emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			gen_rtx (LSHIFT, DImode, operands[1], operands[2])));
  else if (GET_CODE (operands[2]) == CONST_INT && INTVAL (operands[2]) < 0)
    {
      int rshift = - INTVAL (operands[2]);
      operands[3] = force_reg
	(DImode,
	 immed_double_const (1 << (63 - rshift), 1 << (31 - rshift), DImode));
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (LSHIFT, DImode, operands[1], operands[2])));
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (XOR, DImode, operands[0], operands[3])));
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (MINUS, DImode, operands[0], operands[3])));
    }
  else
    {
      operands[3] =
	force_reg (DImode, immed_double_const (0, 1 << 31, DImode));
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (LSHIFT, DImode, operands[1], operands[2])));
      emit_insn (gen_rtx (SET, VOIDmode, operands[3],
			  gen_rtx (LSHIFT, DImode, operands[3], operands[2])));
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (XOR, DImode, operands[0], operands[3])));
      emit_insn (gen_rtx (SET, VOIDmode, operands[0],
			  gen_rtx (MINUS, DImode, operands[0], operands[3])));
  }
  DONE;
}")

(define_expand "ashrdi3"
  [(match_operand:DI 0 "register_operand" "=d")
   (match_operand:DI 1 "register_operand" "0")
   (match_operand:SI 2 "nonmemory_operand" "di")]
  ""
  "
{
  emit_insn (gen_ashldi3 (operands[0], operands[1],
			  negate_rtx (SImode, operands[2])));
  DONE;	    
}")

(define_insn "lshldi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshift:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "shf %2,%0")

(define_expand "lshrdi3"
  [(set (match_operand:DI 0 "register_operand" "=d")
	(lshift:DI (match_operand:DI 1 "register_operand" "0")
		   (match_operand:SI 2 "nonmemory_operand" "di")))]
  ""
  "operands[2] = negate_rtx (SImode, operands[2]);")

;; __builtin instructions

(define_insn "sqrtdf2"
  [(set (match_operand:DF 0 "register_operand" "=d")
	(sqrt:DF (match_operand:DF 1 "register_operand" "0")))]
  "TARGET_C2"
  "sqrt.d %0")

(define_insn "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(sqrt:SF (match_operand:SF 1 "register_operand" "0")))]
  "TARGET_C2"
  "sqrt.s %0")

(define_insn ""
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (ffs:SI (match_operand:SI 1 "register_operand" "d"))
		  (const_int 1)))]
  ""
  "tzc %1,%0\;le.w #32,%0\;jbrs.f .+6\;ld.w #-1,%0")

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "register_operand" "=d")
	(minus:SI (ffs:SI (match_operand:SI 1 "register_operand" "d"))
		  (const_int 1)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  ""
  "")

(define_insn "abssf2"
  [(set (match_operand:SF 0 "register_operand" "=d")
	(abs:SF (match_operand:SF 1 "register_operand" "0")))]
  ""
  "and #0x7fffffff,%0")

(define_expand "absdf2"
  [(set (subreg:DI (match_operand:DF 0 "register_operand" "=d") 0)
	(and:DI (subreg:DI (match_operand:DF 1 "register_operand" "d") 0)
		(match_dup 2)))]
  ""
  "operands[2] = force_reg (DImode,
			    immed_double_const (-1, 0x7fffffff, DImode));")

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
  "* return gen_cmp (operands[0], \"eq\", 't'); ")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"eq\", 'f'); ")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"le\", 'f'); ")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 'f'); ")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 't'); ")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 't'); ")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 'f'); ")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 'f'); ")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"le\", 't'); ")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"eq\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"eq\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"le\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"lt\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"ltu\", 't'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"le\", 'f'); ")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "* return gen_cmp (operands[0], \"leu\", 'f'); ")

;;  - Calls
;;
;; arg count word may be omitted to save a push and let gcc try to
;; combine the arg list pop.  RETURN_POPS_ARGS from tm.h decides this.

(define_insn "call"
  [(call (match_operand:QI 0 "general_operand" "g")
	 (match_operand:SI 1 "general_operand" "g"))]
  ""
  "*
{
  if (! RETURN_POPS_ARGS (ignoreme)) 
    {
      if (operands[1] == const0_rtx)
	return \"calls %0\";
      if (! reg_mentioned_p (arg_pointer_rtx, operands[0]))
	return \"mov sp,ap\;calls %0\;ld.w 12(fp),ap\";
      operands[0] = XEXP (operands[0], 0);
      return \"ld.w %0,a1\;mov sp,ap\;calls (a1)\;ld.w 12(fp),ap\";
    }
  operands[1] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[1]) + 3)/ 4);
  if (! reg_mentioned_p (arg_pointer_rtx, operands[0]))
    return \"mov sp,ap\;pshea %a1\;calls %0\;ld.w 12(fp),ap\;add.w #4*%a1+4,sp\";
  operands[0] = XEXP (operands[0], 0);
  return \"ld.w %0,a1\;mov sp,ap\;pshea %a1\;calls (a1)\;ld.w 12(fp),ap\;add.w #4*%a1+4,sp\";
}")

(define_insn "call_value"
  [(set (match_operand 0 "" "=g")
	(call (match_operand:QI 1 "general_operand" "g")
	      (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (! RETURN_POPS_ARGS (ignoreme)) 
    {
      if (operands[2] == const0_rtx)
	return \"calls %1\";
      if (! reg_mentioned_p (arg_pointer_rtx, operands[1]))
	return \"mov sp,ap\;calls %1\;ld.w 12(fp),ap\";
      operands[1] = XEXP (operands[1], 0);
      return \"ld.w %1,a1\;mov sp,ap\;calls (a1)\;ld.w 12(fp),ap\";
    }
  operands[2] = gen_rtx (CONST_INT, VOIDmode, (INTVAL (operands[2]) + 3) / 4);
  if (! reg_mentioned_p (arg_pointer_rtx, operands[1]))
    return \"mov sp,ap\;pshea %a2\;calls %1\;ld.w 12(fp),ap\;add.w #4*%a2+4,sp\";
  operands[1] = XEXP (operands[1], 0);
  return \"ld.w %1,a1\;mov sp,ap\;pshea %a2\;calls (a1)\;ld.w 12(fp),ap\;add.w #4*%a2+4,sp\";
}")

(define_insn "return"
  [(return)]
  ""
  "rtn")

(define_insn "nop"
  [(const_int 0)]
  ""
  "nop")

(define_insn "tablejump"
  [(set (pc) (match_operand:SI 0 "address_operand" "p"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "jmp %a0")

;; - fix up the code generated for bit field tests

;; cc0 = (x >> k1) & k2  -->  cc0 = x & (k2 << k1)
;; cc0 = (x << k1) & k2  -->  cc0 = x & (k2 >> k1)  
;; provided k2 and (k2 << k1) don't include the sign bit

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "r")
	(lshift:SI (match_dup 0)
		   (match_operand 1 "immediate_operand" "i")))
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand 2 "immediate_operand" "i")))
   (set (cc0) (match_dup 0))]
  "dead_or_set_p (insn, operands[0])
   && GET_CODE (operands[1]) == CONST_INT
   && GET_CODE (operands[2]) == CONST_INT
   && INTVAL (operands[2]) >= 0
   && (INTVAL (operands[2]) << INTVAL (operands[1])) >= 0"
  "*
{
  operands[2] = gen_rtx (CONST_INT, VOIDmode, 
			 INTVAL (operands[2]) >> INTVAL (operands[1]));
  output_asm_insn (\"and %2,%0\", operands);
  return set_cmp (operands[0], const0_rtx, 'w');
}")

;; same as above where x is (y & 0xff...) caused by a zero extend

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "r")
	(zero_extend:SI (match_operand 1 "register_operand" "0")))
   (set (match_dup 0)
	(lshift:SI (match_dup 0)
		   (match_operand 2 "immediate_operand" "i")))
   (set (match_dup 0)
	(and:SI (match_dup 0)
		(match_operand 3 "immediate_operand" "i")))
   (set (cc0) (match_dup 0))]
  "dead_or_set_p (insn, operands[0])
   && REGNO (operands[0]) == REGNO (operands[1])
   && GET_CODE (operands[2]) == CONST_INT
   && GET_CODE (operands[3]) == CONST_INT
   && (INTVAL (operands[3]) << INTVAL (operands[2])) >= 0"
  "*
{
  operands[3] = gen_rtx (CONST_INT, VOIDmode, 
			 (INTVAL (operands[3]) >> INTVAL (operands[2])) &
			 ~((-1) << GET_MODE_BITSIZE (GET_MODE (operands[1]))));
  output_asm_insn (\"and %3,%0\", operands);
  return set_cmp (operands[0], const0_rtx, 'w');
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

