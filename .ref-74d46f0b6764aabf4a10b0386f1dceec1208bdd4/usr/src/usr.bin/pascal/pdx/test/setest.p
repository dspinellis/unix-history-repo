(*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)setest.p	5.1 (Berkeley) %G%
 *)

program setest(input,output);

type
aset = set of 1..41;
sint = 0 .. 30000;

var
set1, set2 :aset;
ptr :^aset;
i :sint;

begin
writeln('Enter a number between 2 and 41.');
writeln('Numbers 2, 10..12, 17, and 37 print "false" all others "true".');
i := ord(input^);
read(i);
writeln('i =',i:2);
write('The set opcodes work :');
set1 := [5..15];
set2 := [10];
if set1 * [23] = [] then 
write(' []')
else
write(false:6);
if set1-set2 <> [5..9,11..15] then
   write(false:6);
new(ptr);
ptr^ := [2,10,37,i];
set1 := ptr^ - [1..3];
set2 := ptr^ + [10..12,17];
if ptr^ * set1 = set1 then
   write(' mul plus minus')
else
   write(false:6);
if (17 in [10..13,15..20,25..30]) and
   (not (22 in [1..17,36..41])) then
   write(' in')
else
   write(false:6);
if (set1 <> set2) and
   (set1 <  set2) and
   (set2 >  set1) and
   (set2 >= ptr^) and
   (set1 <= set2) and
   (set1 >= set1) and
   (set1 <= set1) and
   (card(set2) = 7) then
   write(' and rel')
else
   write(false:6);
if (set1 <> set1) or
   (set2 <  set1) or
   (set1 >  set2) or
   (ptr^ >= set2) or
   (set2 <= set1) or
   (set1 =  set2) or
   (17 in set2) then
   writeln(' or rel')
else
   writeln(false:6);
i := pred(i);
i := (i + i) div i;
end.
