/*
 * strerror.c --- ANSI C compatible system error routine
 */

/* 
 * Copyright (C) 1986, 1988, 1989, 1991 the Free Software Foundation, Inc.
 * 
 * This file is part of GAWK, the GNU implementation of the
 * AWK Progamming Language.
 * 
 * GAWK is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * GAWK is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GAWK; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

extern int sys_nerr;
extern char *sys_errlist[];

char *
strerror(n)
int n;
{
	static char mesg[30];

	if (n < 0 || n > sys_nerr) {
		sprintf (mesg, "Unknown error (%d)", n);
		return mesg;
	} else
		return sys_errlist[n];
}
