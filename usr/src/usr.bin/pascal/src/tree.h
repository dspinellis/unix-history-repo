/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)tree.h	8.1 (Berkeley) 6/6/93
 */

#define T_MINUS 1
#define T_MOD 2
#define T_DIV 3
#define T_DIVD 4
#define T_MULT 5
#define T_ADD 6
#define T_SUB 7
#define T_EQ 8
#define T_NE 9
#define T_LT 10
#define T_GT 11
#define T_LE 12
#define T_GE 13
#define T_NOT 14
#define T_AND 15
#define T_OR 16
#define T_ASGN 17
#define T_PLUS 18
#define T_IN 19
#define T_LISTPP 20
#define T_PDEC 21
#define T_FDEC 22
#define T_PVAL 23
#define T_PVAR 24
#define T_PFUNC 25
#define T_PPROC 26
#define T_NIL 27
#define T_STRNG 28
#define T_CSTRNG 29
#define T_PLUSC 30
#define T_MINUSC 31
#define T_ID 32
#define T_INT 33
#define T_FINT 34
#define T_CINT 35
#define T_CFINT 36
#define T_TYPTR 37
#define T_TYPACK 38
#define T_TYSCAL 39
#define T_TYRANG 40
#define T_TYARY 41
#define T_TYFILE 42
#define T_TYSET 43
#define T_TYREC 44
#define T_TYFIELD 45
#define T_TYVARPT 46
#define T_TYVARNT 47
#define T_CSTAT 48
#define T_BLOCK 49
#define T_BSTL 50
#define T_LABEL 51
#define T_PCALL 52
#define T_FCALL 53
#define T_CASE 54
#define T_WITH 55
#define T_WHILE 56
#define T_REPEAT 57
#define T_FORU 58
#define T_FORD 59
#define T_GOTO 60
#define T_IF 61
#define T_CSET 63
#define T_RANG 64
#define T_VAR 65
#define T_ARGL 66
#define T_ARY 67
#define T_FIELD 68
#define T_PTR 69
#define T_WEXP 70
#define T_PROG 71
#define T_BINT 72
#define T_CBINT 73
#define T_IFEL 74
#define T_IFX 75
#define T_TYID 76
#define T_COPSTR 77
#define T_BOTTLE 78
#define T_RFIELD 79
#define T_FLDLST 80
#define T_LAST 81
#define T_TYCRANG 82
#define T_TYCARY 83
