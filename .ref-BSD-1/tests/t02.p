program graph1(output);
const
	d = 0.0625;
	s = 32;
	h = 34;
	c = 6.28318;
var
	x,y: real;
	i,n: integer;
procedure rdr(var f: text; var x: real);
const
	t48 = 281474976710656;
	limit = 56294995342131;
	z = 27;
	lim1 = 322;
	lim2 = -292;
type
	posint = 0..323;
var
	ch: char;
	y: real;
	a, i, e: integer;
	s, ss: boolean;

function ten(e: posint): real;
var
	i: integer;
	t: real;
begin
	i := 0;
	t := 1.0;
	repeat
		if odd(e) then
		case i of
		0: t := t*1.0e1;
		1: t := t*1.0e2;
		2: t := t*1.0e4;
		3: t := t*1.0e8;
		4: t := t*1.0e16;
		5: t := t*1.0e32;
		6: t := t*1.0e64;
		7: t := t*1.0e128;
		8: t := t*1.0e256
		end ;
		e := e div 2;
		i := i+1;
	until e = 0;
	ten := t
end ;

begin
	if eof(f) then
	begin
		message('**tried to read past eos/eof');
		halt
	end ;
	while (f^ = ' ') and (not eof(f)) do
		get(f);
	if not eof(f) then
	begin
		ch := f^;
		if ch = '-' then
		begin
			s := true;
			get(f);
			ch := f^;
		end else
		begin
			s := false;
			if ch = '+' then
			begin
				get(f);
				ch := f^;
			end
		end ;
		if not (ch in ['0'..'9']) then
		begin
			message('**digit expected');
			halt
		end ;
		a := 0;
		e := 0;
		repeat
			if a < limit then
				a := 10*a + ord(ch)-z else
				e := e+1;
			get(f);
			ch := f^;
		until not (ch in ['0'..'9']);
		if ch = '.' then
		begin
			get(f);
			ch := f^;
			while ch in ['0'..'9'] do
			begin
				if a < limit then
				begin
					a := 10*a+ord(ch)-z;
					e := e-1;
				end ;
				get(f);
				ch := f^;
			end
		end;
		if ch = 'e' then
		begin
			get(f);
			ch := f^;
			i := 0;
			if ch = '-' then
			begin
				ss := true;
				get(f);
				ch := f^;
			end else
			begin
				ss := false;
				if ch = '+' then
				begin
					get(f);
					ch := f^;
				end
			end;
			if ch in ['0'..'9'] then
			begin
				i := ord(ch)-z;
				get(f);
				ch := f^;
				while ch in ['0'..'9'] do
				begin
					if i < limit then
						i := 10*i + ord(ch)-z;
					get(f);
					ch := f^;
				end
			end else
			begin
				message(' digit expected');
				halt
			end ;
			if ss then
				e := e-i else
				e := e+i;
		end;
		if e < lim2 then
		begin
			a := 0;
			e := 0;
		end else
		if e > lim1 then
		begin
			message('**number too large');
			halt
		end;
		if a >= t48 then
			y := ((a+1) div 2) * 2.0 else
			y := a;
		if s then
			y := -y;
		if e < 0 then
			x := y/ten(-e) else
		if e <> 0 then
			x := y*ten(e) else
			x := y;
	end;
end;
begin
	for i := 0 to lim do
	begin
		x := d*i;
		y := exp(-x)*sin(c*x);
		n := round(s*y) + h;
		repeat
			write(blank);
			n := n-1;
		until n=0;
		write(aster)
	end
end.
