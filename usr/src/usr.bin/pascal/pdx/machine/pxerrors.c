/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)pxerrors.c 1.2 %G%";

/*
 * px error messages
 */

char *pxerrmsg[] ={
	"not an error!",
	"argument to chr out of range",
	"div (integer divide) by zero",
	"real divide by zero",
	"call to procedure halt",
	"reference through a nil pointer",
	"tried to read past end-of-file",
	"negative parameter to sqrt",
	"pi/px error: stack not empty",
	"subscript out of range",
	"reference to an inactive file",
	"pi/px error: write failed",
	"pi/px error: create failed",
	"non-positive argument to ln",
	"pi/px error: bad op",
	"bad data on integer read",
	"pi/px error: active frame not found in goto",
	"label not found in case",
	"pi/px error: seek failed",
	"pi/px error: bad parameter to alloc",
	"out of memory",
	"constructed set parameter exceeds set bounds",
	"too many digits in number",
	"mod (integer remainder) by 0",
	"bad data on real read",
	"pi/px error: remove failed",
	"pi/px error: close failed",
	"pi/px error: open failed",
	"parameter to argv out of range",
	"bad i to pack(a, i, z)",
	"bad i to unpack(z, a, i)",
	"value out of range",
	"assertion failed",
	"tried to read, but open for writing",
	"tried to write, but open for reading",
	"integer number too large",
	"statement limit exceeded",
	"runtime stack overflow",
	"interrupt",
	"overflow, underflow, or divide by zero in arithmetic operation",
};
