(*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)setest.p	8.1 (Berkeley) 6/6/93
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
