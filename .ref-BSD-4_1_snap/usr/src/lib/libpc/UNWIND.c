/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)UNWIND.c 1.2 3/7/81";

UNWIND(frame)

	long	*frame;
{
	PCLOSE(*frame);
}
