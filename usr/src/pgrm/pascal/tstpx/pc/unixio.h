(* Copyright (c) 1979 Regents of the University of California *)

const
sccsid = '@(#)unixio.h 1.1 6/17/81';

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
