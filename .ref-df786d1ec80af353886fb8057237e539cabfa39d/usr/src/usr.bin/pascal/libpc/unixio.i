(* Copyright (c) 1979 Regents of the University of California *)

const
sccsid = '@(#)unixio.i 1.1 %G%';

type
fileptr = record
	cnt :integer
	end;

function TELL(
var	fptr :text)
{returns} :fileptr;

var
filesize, headsize, tailsize :integer;
result :fileptr;

begin
tailsize := 0;
while not eof(fptr) do begin
	get(fptr);
	tailsize := tailsize + 1
	end;
filesize := 0;
reset(fptr);
while not eof(fptr) do begin
	get(fptr);
	filesize := filesize + 1
	end;
reset(fptr);
for headsize := 1 to filesize - tailsize do
	get(fptr);
result.cnt := headsize;
TELL := result
end;

procedure SEEK(
 var	fptr :text;
 var	cnt :fileptr);

var
i :integer;

begin
reset(fptr);
for i := 1 to cnt.cnt do
	get(fptr)
end;

procedure APPEND(
 var	fptr :text);

var
tmp :text;

begin
rewrite(tmp);
reset(fptr);
while not eof(fptr) do begin
	if eoln(fptr) then
		writeln(tmp)
	else
		write(tmp, fptr^);
	get(fptr)
	end;
reset(tmp);
rewrite(fptr);
while not eof(tmp) do begin
	if eoln(tmp) then
		writeln(fptr)
	else
		write(fptr, tmp^);
	get(tmp)
	end
end;
