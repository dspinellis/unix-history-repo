/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b0fil.h,v 1.4 85/08/22 16:41:24 timo Exp $
*/

/* Declarations for variables containing file names. */
/* The corresponding initializations are in b0fil.c. */

extern char *bpermfile;
extern char *tempfile;
extern char *messfile;

#ifndef INTEGRATION
extern char *editorfile;
#endif

#define BPERMFILE bpermfile
