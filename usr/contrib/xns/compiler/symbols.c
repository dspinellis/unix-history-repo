/*
 * Copyright (C) 1984 by Eric C. Cooper.
 * All rights reserved.
 */
#ifndef lint
static char RCSid[] = "$Header: symbols.c,v 2.0 85/11/21 07:21:46 jqj Exp $";
#endif

/* $Log:	symbols.c,v $
 * Revision 2.0  85/11/21  07:21:46  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.4  85/03/26  06:10:36  jqj
 * *** empty log message ***
 * 
 * Revision 1.4  85/03/26  06:10:36  jqj
 * Revised public alpha-test version, released 26 March 1985
 * 
 * Revision 1.3  85/03/11  16:40:14  jqj
 * Public alpha-test version, released 11 March 1985
 * 
 * Revision 1.2  85/02/21  11:06:00  jqj
 * alpha test version
 * 
 * Revision 1.1  85/02/15  13:55:40  jqj
 * Initial revision
 * 
 */

/*
 * Symbol table management routines
 */

#include "compiler.h"

struct object *SymbolTables;
struct object EnumSymbols;
/*

 * Symbol table management.  Find a symbol given a known symbol table.
 * The routines lookup() and addsymbol() are the ONLY routines that
 * understand the format of the symbol table, and should be the only
 * routines that use ocar() and ocdr().
 */
static struct object *
lookup(name, symlist)
	char *name;
	struct object *symlist;
{
	struct object *op;

	for (op = symlist; op != ONIL; op = ocdr(op)) {
		if (streq(op->o_name, name))
			return (op);
	}
	return (ONIL);
}

static struct object *
addsymbol(name,osymp)
	char *name;
	struct object **osymp;
{
	struct object *o;

	o = New(struct object);
	o->o_class = O_UNKNOWN;
	/* (jqj)? use binary-tree symboltable here */
	ocdr(o) = *osymp;
	*osymp = o;
	o->o_name = copy(name);
	return(o);
}

struct object *
make_symbol(name,symboltablename)
	char *name;
	char *symboltablename;
{
	struct object *osym, *rsym;

	if (symboltablename == NULL) {
		rsym = addsymbol(name, &(EnumSymbols.o_symboltable));
		rsym->o_module = "";	/* not NULL */
		rsym->o_modnumber = 0;
		rsym->o_modversion = 0;
		return(rsym);
	}
	else if ((osym = lookup(symboltablename, SymbolTables))) {
		rsym = addsymbol(name, &(osym->o_symboltable));
		rsym->o_module = osym->o_module;
		rsym->o_modnumber = osym->o_modnumber;
		rsym->o_modversion = osym->o_modversion;
		return(rsym);
	}
	else {
		error(ERROR,"Internal error:  Symbol table %s undefined.",
			symboltablename);
		return(ONIL);
	}
}

struct object *
make_module(name,number,version)
	char *name;		/* Courier module name */
	char *number;		/* Courier program number */
	char *version;
{
	struct object *symbol;

	symbol = addsymbol(name, &SymbolTables);
	symbol->o_class = O_SYMBOLTABLE;
	symbol->o_name = name;
	symbol->o_module = name;
	symbol->o_modnumber = stringtocard(number);
	symbol->o_modversion = stringtocard(version);
	return(symbol);
}

/*
 * Check that a program name, number, version group is already defined.
 * Returns TRUE if name, number, and version all match
 */
 check_module_def(name,number,version)
	char *name, *number, *version;
{
	struct object *module;

	if ((module = lookup(name, SymbolTables)) == (struct object *) NULL)
		return(0);
	if (stringtocard(version) != module->o_modversion ||
	    stringtocard(number) != module->o_modnumber)
		return(0);
	return(1);	/* for now */
}

/*
 * Check that a program name was listed in the DEPENDS UPON list.
 * returns a pointer to the symbol if defined, else 0.
 */
check_dependency(name)
	char *name;
{
	return((int) lookup(name, SymbolTables));
}

/* (jqj) */
struct object *
check_def(symbol,symboltablename)
	char *symbol;		/* name of the symbol */
	char *symboltablename;	/* symbol table to examine */
{
	struct object *osym;

	if (symboltablename == NULL) {
		osym = &EnumSymbols;
	}
	else if (! (osym = lookup(symboltablename, SymbolTables))) {
		error(ERROR,"Internal error:  Symbol table %s undefined.",symboltablename);
		return(ONIL);
	}
	return(lookup(symbol, osym->o_symboltable));
}


static char gensymstr[MAXSTR];

char *
gensym(leader)
	char *leader;
{
	static int n;
	char buf[MAXSTR];

	(void) sprintf(buf, "%s%s_%d", leader, gensymstr, ++n);
	return (copy(buf));
}

setgensym(number,version)
	char *number, *version;
{
	(void) sprintf(gensymstr,"%s",number);
}

define_enumeration_symbol(sym, value)
	struct object *sym;
	char *value;
{
	class_of(sym) = O_ENUMTAG;
	sym->o_enum = New(struct enumtag);
	sym->o_enum->en_name = name_of(sym);
	sym->o_enum->en_value = stringtocard(value);
}
