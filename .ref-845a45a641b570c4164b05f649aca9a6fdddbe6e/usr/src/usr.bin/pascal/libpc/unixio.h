(*
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)unixio.h	1.2 (Berkeley) %G%
 *)

const
sccsid = '@(#)unixio.h 1.2 %G%';

type
fileptr = record
	cnt :integer
	end;

function TELL(
var	fptr :text)
{returns} :fileptr;

  external;

procedure SEEK(
 var	fptr :text;
 var	cnt :fileptr);

  external;

procedure APPEND(
 var	fptr :text);

   external;
