/* Copyright (c) 1979 Regents of the University of California */
#include <stdio.h>
#include "as.h"

#define OP(name,opcode,nargs,arg1,arg2,arg3,arg4,arg5,arg6) \
	{ \
		name,(nargs==0 ? INST0:INSTn), opcode,nargs, \
		arg1,arg2,arg3,arg4,arg5,arg6 \
	}
#define PSEUDO(name, type, tag) \
	{ \
		name, tag, type \
	}

readonly struct instab instab[] = {
PSEUDO(".space",	0,	ISPACE),
PSEUDO(".byte",		0,	IBYTE),
PSEUDO(".word",		0,	IWORD),
PSEUDO(".long",		0,	ILONG),
PSEUDO(".int",		0,	IINT),
PSEUDO(".data",		0,	IDATA),
PSEUDO(".globl",	0,	IGLOBAL),
PSEUDO(".set",		0,	ISET),
PSEUDO(".text",		0,	ITEXT),
PSEUDO(".comm",		0,	ICOMM),
PSEUDO(".lcomm",	0,	ILCOMM),
PSEUDO(".lsym",		0,	ILSYM),
PSEUDO(".align",	0,	IALIGN),
PSEUDO(".float",	0,	IFLOAT),
PSEUDO(".double",	0,	IDOUBLE),
PSEUDO(".org",		0,	IORG),
PSEUDO(".stab",		0,	ISTAB),
PSEUDO(".stabs",	0,	ISTABSTR),
PSEUDO(".stabn",	0,	ISTABNONE),
PSEUDO(".stabd",	0,	ISTABDOT),
PSEUDO(".ascii",	0,	IASCII),
PSEUDO(".asciz",	0,	IASCIZ),
PSEUDO(".file",		0,	IFILE),
PSEUDO(".line",		0,	ILINENO),
PSEUDO(".ABORT",	0,	IABORT),

PSEUDO("r0",		0,	REG),
PSEUDO("r1",		1,	REG),
PSEUDO("r2",		2,	REG),
PSEUDO("r3",		3,	REG),
PSEUDO("r4",		4,	REG),
PSEUDO("r5",		5,	REG),
PSEUDO("r6",		6,	REG),
PSEUDO("r7",		7,	REG),
PSEUDO("r8",		8,	REG),
PSEUDO("r9",		9,	REG),
PSEUDO("r10",		10,	REG),
PSEUDO("r11",		11,	REG),
PSEUDO("r12",		12,	REG),
PSEUDO("r13",		13,	REG),
PSEUDO("r14",		14,	REG),
PSEUDO("r15",		15,	REG),
PSEUDO("ap",		12,	REG),
PSEUDO("fp",		13,	REG),
PSEUDO("sp",		14,	REG),
PSEUDO("pc",		15,	REG),

PSEUDO("jcc",		0x1e,	IJXXX),
PSEUDO("jcs",		0x1f,	IJXXX),
PSEUDO("jeql",		0x13,	IJXXX),
PSEUDO("jeqlu",		0x13,	IJXXX),
PSEUDO("jgeq",		0x18,	IJXXX),
PSEUDO("jgequ",		0x1e,	IJXXX),
PSEUDO("jgtr",		0x14,	IJXXX),
PSEUDO("jgtru",		0x1a,	IJXXX),
PSEUDO("jleq",		0x15,	IJXXX),
PSEUDO("jlequ",		0x1b,	IJXXX),
PSEUDO("jlss",		0x19,	IJXXX),
PSEUDO("jlssu",		0x1f,	IJXXX),
PSEUDO("jneq",		0x12,	IJXXX),
PSEUDO("jnequ",		0x12,	IJXXX),
PSEUDO("jvc",		0x1c,	IJXXX),
PSEUDO("jvs",		0x1d,	IJXXX),
PSEUDO("jbr",		0x11,	IJXXX),
PSEUDO("jbc",		0xe1,	IJXXX),
PSEUDO("jbs",		0xe0,	IJXXX),
PSEUDO("jbcc",		0xe5,	IJXXX),
PSEUDO("jbsc",		0xe4,	IJXXX),
PSEUDO("jbcs",		0xe3,	IJXXX),
PSEUDO("jbss",		0xe2,	IJXXX),
PSEUDO("jlbc",		0xe9,	IJXXX),
PSEUDO("jlbs",		0xe8,	IJXXX),

#include "instrs"

0
};
