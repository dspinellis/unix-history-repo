/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)languages.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Language management.
 */

#include "defs.h"
#include "languages.h"
#include "c.h"
#include "pascal.h"
#include "modula-2.h"
#include "asm.h"

#ifndef public

typedef struct Language *Language;

typedef enum {
    L_PRINTDECL, L_PRINTVAL, L_TYPEMATCH, L_BUILDAREF, L_EVALAREF,
    L_MODINIT, L_HASMODULES, L_PASSADDR,
    L_ENDOP
} LanguageOp;

typedef LanguageOperation();

Language primlang;

#endif

struct Language {
    String name;
    String suffix;
    LanguageOperation *op[20];
    Language next;
};

private Language head;

/*
 * Initialize language information.
 *
 * The last language initialized will be the default one
 * for otherwise indistinguised symbols.
 */

public language_init()
{
    primlang = language_define("$builtin symbols", ".?");
    c_init();
    fortran_init();
    pascal_init();
    modula2_init();
    asm_init();
}

public Language findlanguage(suffix)
String suffix;
{
    Language lang;

    lang = head;
    if (suffix != nil) {
	while (lang != nil and not streq(lang->suffix, suffix)) {
	    lang = lang->next;
	}
	if (lang == nil) {
	    lang = head;
	}
    }
    return lang;
}

public String language_name(lang)
Language lang;
{
    return (lang == nil) ? "(nil)" : lang->name;
}

public Language language_define(name, suffix)
String name;
String suffix;
{
    Language p;

    p = new(Language);
    p->name = name;
    p->suffix = suffix;
    p->next = head;
    head = p;
    return p;
}

public language_setop(lang, op, operation)
Language lang;
LanguageOp op;
LanguageOperation *operation;
{
    checkref(lang);
    assert(ord(op) < ord(L_ENDOP));
    lang->op[ord(op)] = operation;
}

public LanguageOperation *language_op(lang, op)
Language lang;
LanguageOp op;
{
    LanguageOperation *o;

    checkref(lang);
    o = lang->op[ord(op)];
    checkref(o);
    return o;
}
