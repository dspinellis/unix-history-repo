/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)UNWIND.c 1.1 10/29/80";

UNWIND(frame)

	int	*frame;
{
	PCLOSE(*frame);
}
