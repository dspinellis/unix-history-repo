/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)symtab.h	5.2 (Berkeley) 1/3/88
 */

/*
 * Public definitions for symbol table.
 */

SYMTAB *symtab;

SYMTAB *st_creat();		/* create a symbol table */
int st_destroy();		/* destroy a symbol table, i.e. free storage */
SYM *st_insert();		/* insert a symbol */
SYM *st_lookup();		/* lookup a symbol */
int dumpvars();			/* dump the symbols of a function */
int print_alias();		/* print out currently active aliases */
int enter_alias();		/* create a new name for a command */
SYM *findtype();		/* search symbol table for a type name */
