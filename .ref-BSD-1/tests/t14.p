program rec(output);
type
	alfa = packed array[1..10] of char;
	status = (married, widowed, divorced, single);
	date = record
		mo: (jan, feb, mar, apr, may, jun,
			july, aug, sept, oct, nov, dec);
		day: 1..31;
		year: integer
		end;
	person = record
		name: record
			first, last: alfa
			end;
		ss: integer;
		sex: (male, female);
		birth: date;
		depdts: integer;
		case ms: status of
			married, widowed: (
				mdate: date);
			divorced: (
				ddate: date;
				firstd: boolean);
			single: (
				indepdt: boolean)
		end;
var
	p: ^person;
begin
	new(p);
	p^.name.last := 'woodyard';
	p^.name.first := 'edward';
	p^.ss := 845680539;
	p^.sex := male;
	p^.birth.mo := aug;
	p^.birth.day := 30;
	p^.birth.year := 1941;
	p^.depdts := 1;
	p^.ms := single;
	p^.indepdt := true;
end.
