/* Copyright (c) 1982 Regents of the University of California */

static char sccsid[] = "@(#)CASERNG.c 1.1 %G%";

CASERNG(val)
	int val;
{
	ERROR("Label of %D not found in case\n", val);
}
