program test(output);

	procedure foo0(procedure bar);
		begin
		bar
		end;

	procedure foo1(x :integer; procedure bar(i :integer));
		begin
		bar(x)
		end;

	procedure zero;
		begin
		writeln('I take no parameters');
		end;

	procedure one(z :integer);
		begin
		writeln('My argument is ',z:1);
		end;

	begin
	foo0(zero);
	foo1(12, one);
	writeln('success');
	end.
