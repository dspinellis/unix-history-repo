/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b0fil.c,v 1.4 85/08/22 16:48:05 timo Exp $
*/

/* Built-in file names.  Some can be overridden by environment variables. */
/* Only recompilation of this file is necessary when a name is changed.   */

#include "b.h"
#include "b0fil.h"

#ifdef IBMPC
Visible char *bpermfile= "PERM.BIF";
Visible char *tempfile= "TEMP.BIF";
Visible char *messfile= "\\MESSAGES.BIF";
#else
Visible char *bpermfile= ".b_perm";
Visible char *tempfile= ".@b_temp";
Visible char *messfile= "/usr/local/lib/B/Messages";
#endif

#ifndef INTEGRATION
#ifndef BED
#define BED "/usr/local/lib/B/bed +" /* Trailing + if '+lineno' arg. */
#endif
Visible char *editorfile= BED;
#endif

Hidden char *setdefault(envname, deflt) char *envname, *deflt; {
	char *p= getenv(envname);
	if (p != NULL && p[0] != '\0') return p;
	return deflt;
}

Visible Procedure set_file_names() {
	messfile= setdefault("B_MESSAGES", messfile);
#ifndef INTEGRATION
	editorfile= setdefault("B_EDITOR", setdefault("BEDITOR", editorfile));
	/* BEDITOR used to be the name; officially it's now B_EDITOR */
#endif
}
