program gencard(output);
var
cvt :record
	case boolean of
	true:(int :0..255);
	false:(st :set of 0..15);
	end;
i :integer;

begin
for i:=0 to 255 do begin
	cvt.int:=i;
	write((card(cvt.st)):1,', ');
	if (i+1) mod 16 = 0 then
		writeln;
	end;
end.
