program test(output);
var
i :integer;
    function f4( var i :integer;
		r : real;
		function f1(r : real): real;
		procedure p1(var i:integer; function bol(r :real): real)
	) :boolean;
	begin
	write('f4', i:2, r:5:2, f1(i):5:2, f1(r):2:0);
	p1(i, f1);
	f4 := true;
	end;
    procedure pr1(var i:integer; function bol(r : real): real);
	begin
	write(' pr1', bol(i):5:2);
	end;
    function oneover(r : real) : real;
	begin
	oneover := 1 / r;
	end;
    begin
	i := 4;
	writeln('I should print->f4 4 0.25 0.25 4 pr1 0.25 true<-');
	writeln(f4(i, 0.25, oneover, pr1):5);
    end.
