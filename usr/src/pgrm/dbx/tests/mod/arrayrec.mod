(*
 * Test of arrays of records.
 *)

module main;
type
    Rec = record
	charValue : char;
	intValue : integer;
	subrange : [0..1000];
	realValue : real;
    end;
    Arr = array [1..10] of Rec;
var
    a : Arr;
begin
    a[1].charValue := 'c';
    a[1].intValue := 3;
    a[1].subrange := 10;
    a[1].realValue := 3.4;
    a[2].charValue := 'd';
    a[2].intValue := 4;
    a[2].subrange := 11;
    a[2].realValue := 4.5;
    a[3].charValue := 'e';
    a[3].intValue := 5;
    a[3].subrange := 12;
    a[3].realValue := 5.6;
end main.
