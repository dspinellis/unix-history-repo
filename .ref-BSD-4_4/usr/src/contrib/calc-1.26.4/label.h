/*
 * Copyright (c) 1993 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 */


/*
 * Label structures.
 */
typedef struct {
    long l_offset;		  /* offset into code of label */
    long l_chain;		  /* offset into code of undefined chain */
    char *l_name;		  /* name of label if any */
} LABEL;

extern void initlabels(), definelabel(), addlabel();
extern void checklabels(), clearlabel(), setlabel();
extern void uselabel();

/* END CODE */
