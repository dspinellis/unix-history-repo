/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b.h,v 2.2 85/08/22 15:59:55 timo Exp $ */

/*
 * B editor -- Basics copied from B interpreter's run-time system.
 */

#include <stdio.h>

#define Visible
#define Hidden static
#define Procedure

typedef int bool;
typedef short intlet;
typedef char *string;

#define No 0
#define Yes 1

#define Maxintlet ((1<<15)-1) /* MACHINE DEPENDENT */

typedef struct {
	char	type;
	char	_unused;
	intlet	refcnt;
	intlet	len;
	string	*cts;
} *value;

/* See also definitions in node.h and queu.h which must match the first
   four fields of 'value'! */

#define Refcnt(v) ((v)->refcnt)
#define Type(v) ((v)->type)
#define Length(v) ((v)->len)
#define Str(v) ((char*)(&(v)->cts))

#define Vnil ((value) NULL)

/* Types: */
#define Num '0'
#define Tex '"'
#define Com ','
#define Nod 'N'
#define Pat 'P'

/*
 * C library standard functions
 */

string malloc();
string realloc();

string sprintf();

string strcpy();
string strncpy();
string index();
string rindex();

string getenv();

#define Strequ(s, t) !strcmp(s, t)
#define Strnequ(s, t, n) !strncmp(s, t, n)
