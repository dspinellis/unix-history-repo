/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)FRTN.c 1.5 11/12/82";

#include "h00vars.h"

FRTN(frtn, save)
	register struct formalrtn *frtn;
	char *save;
{
	blkcpy(save, &_disply[1], frtn->fbn * sizeof(struct display));
}
