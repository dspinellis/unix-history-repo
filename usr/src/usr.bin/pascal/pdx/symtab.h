/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)symtab.h 1.1 %G%";

/*
 * Public definitions for symbol table.
 */

SYMTAB *symtab;

SYMTAB *st_creat();		/* create a symbol table */
st_destroy();			/* destroy a symbol table, i.e. free storage */
SYM *st_insert();		/* insert a symbol */
SYM *st_lookup();		/* lookup a symbol */
dumpvars();			/* dump the symbols of a function */
print_alias();			/* print out currently active aliases */
enter_alias();			/* create a new name for a command */
SYM *findtype();		/* search symbol table for a type name */
