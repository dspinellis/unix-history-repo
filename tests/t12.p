program records(output);
type
	cmplx = record
		rp: real;
		ip: real;
	end;
var
	x, y: cmplx;
function cadd(a,b: cmplx): cmplx;
	begin
		cadd.rp := a.rp + b.rp;
		cadd.ip := a.ip + b.ip;
	end;
procedure cprint(a: cmplx);
	begin
		writeln(a.rp, '+', a.ip, 'i');
	end;
begin
	x.rp := 1;
	x.ip := 2;
	y.rp := 3;
	y.ip := 4;
	cprint(cadd(x, y));
end.
