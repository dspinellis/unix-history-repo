/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)keywords.c 1.6 %G%";
/*
 * Keyword and alias management.
 */

#include "defs.h"
#include "keywords.h"
#include "scanner.h"
#include "names.h"
#include "symbols.h"
#include "tree.h"
#include "y.tab.h"

#ifndef public
#include "scanner.h"
#endif

private String reserved[] ={
    "alias", "and", "assign", "at", "call", "catch", "cont",
    "debug", "delete", "div", "dump", "edit", "file", "func",
    "gripe", "help", "if", "ignore", "in",
    "list", "mod", "next", "nexti", "nil", "not", "or",
    "print", "psym", "quit", "run",
    "sh", "skip", "source", "status", "step", "stepi",
    "stop", "stopi", "trace", "tracei",
    "use", "whatis", "when", "where", "whereis", "which",
    "INT", "REAL", "NAME", "STRING",
    "LFORMER", "RFORMER", "#^", "->"
};

/*
 * The keyword table is a traditional hash table with collisions
 * resolved by chaining.
 */
typedef struct Keyword {
    Name name;
    Token toknum : 16;
    struct Keyword *chain;
} *Keyword;

typedef unsigned int Hashvalue;

#define KEYWORDHASH	101
private Keyword hashtab[KEYWORDHASH];
#define keyhash(n) ((((unsigned) n) >> 2) mod KEYWORDHASH)

/*
 * The alias table is virtually the same, just
 * replace the token id with a string to which
 * the alias expands.
 */
typedef struct Alias {
     Name name;
     Name expansion;
     struct Alias *chain;
} *Alias;

#define	ALIASHASH	503
private	Alias aliashashtab[ALIASHASH];
#define aliashash(n) ((((unsigned) n) >> 2) mod ALIASHASH)

/*
 * Enter all the reserved words into the keyword table.
 */
public enterkeywords()
{
    register Integer i;

}

/*
 * Deallocate the keyword and alias tables.
 */
public keywords_free()
{
    register Integer i;
    register Keyword k, nextk;
    register Alias a, nexta;

    for (i = 0; i < KEYWORDHASH; i++) {
	for (k = hashtab[i]; k != nil; k = nextk) {
	    nextk = k->chain;
	    dispose(k);
	}
	hashtab[i] = nil;
    }
    for (i = 0; i < ALIASHASH; i++) {
	for (a = aliashashtab[i]; a != nil; a = nexta) {
	    nexta = a->chain;
	    dispose(a);
	}
	aliashashtab[i] = nil;
    }
}

/*
 * Enter a keyword into the name table. 
 * It is assumed to not be there already.
 * The string is assumed to be statically allocated.
 */
private keyword(s, t)
String s;
Token t;
{
    register Keyword k;
    Hashvalue h;
    Name n;

    n = identname(s, true);
    h = keyhash(n);
    k = new(Keyword);
    k->name = n;
    k->toknum = t;
    k->chain = hashtab[h];
    hashtab[h] = k;
}

/*
 * Return the string associated with a token corresponding to a keyword.
 */
public String keywdstring(t)
Token t;
{
    return reserved[ord(t) - ord(ALIAS)];
}

/*
 * Find a keyword in the keyword table.
 * We assume that tokens cannot legitimately be nil (0).
 */

public Token findkeyword(n)
Name n;
{
    register Hashvalue h;
    register Keyword k;
    Token t;

    for (a = aliashashtab[aliashash(n)]; a != nil && a->name != n; a = a->chain)
	;
    return (a == nil ? nil : ident(a->expansion));
}

/*
 * Create an alias.
 */
public enter_alias(cmd, p)
Name cmd;
Node p;
{
    Token t;

    }
    if (p->op == O_SCON)
	n = identname(p->value.scon, true);
    else
	n = identname(ident(p->value.name), true);
    alias(cmd, n);
}

private alias(cmd, n)
Name cmd, n;
{
    register Alias a;
    Hashvalue h;

    h = aliashash(cmd);
    for (a = aliashashtab[h]; a != nil && a->name != cmd; a = a->chain)
	;
    if (a != nil) {
	/* interpret ``alias x x'' as ``unalias x'' */
	if (streq(ident(cmd), ident(n)))
	    unalias(h, a);
	else
	    a->expansion = n;
	return;
    }
    if (!streq(ident(cmd), ident(n))) {		/* as above */
	a = new(Alias);
	a->name = cmd;
	a->expansion = n;
	a->chain = aliashashtab[h];
	aliashashtab[h] = a;
    }
}

private unalias(h, a)
Alias a;
Hashvalue h;
{
    register Alias *ap;

    for (ap = &aliashashtab[h]; *ap != nil && *ap != a; ap = &(*ap)->chain)
	;
    assert(*ap == a);
    *ap = a->chain;
    dispose(a);
}

/*
 * Print out an alias.
 */
public print_alias(cmd)
Name cmd;
{
    register Alias a;
    register Integer i;
    String s;

    if (cmd != nil) {
	s = findalias(cmd);
	if (s != nil)
	    printf("%s\n", s);
	return;
    }
    /*
     * Dump the alias table.
     */
    for (i = 0; i < ALIASHASH; i++) {
	for (a = aliashashtab[i]; a != nil; a = a->chain) {
	    if (isredirected())
		printf("alias ");
	    printf("%s\t%s\n", ident(a->name), ident(a->expansion));
	}
    }
}
