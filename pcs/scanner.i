procedure nextch;   (*read next character; process line end*) 
begin if cc = ll then 
      begin if eof(input) then
            begin writeln;
               writeln(' program incomplete');
               errormsg; goto 99
            end ;
         if errpos <> 0 then  
            begin writeln; errpos := 0 
            end ;
         write(lc:6, '  ');
         ll := 0; cc := 0;
         while not eoln(input) do
            begin ll := ll+1; read(ch); write(ch); line[ll] := ch 
            end ;
         writeln; ll := ll+1; read(line[ll])
      end ; 
   cc := cc+1; ch := line[cc];
end (*nextch*) ;
  
procedure insymbol;           (*reads next symbol*) 
   label 1,2,3; 
   var i,j,k,e: integer;
  
   procedure readscale; 
      var s, sign: integer;
   begin nextch; sign := 1; s := 0; 
      if ch = '+' then nextch else
      if ch = '-' then begin nextch; sign := -1 end ; 
      while ch in ['0'..'9'] do 
         begin s := 10*s + ord(ch) - ord('0'); nextch 
         end ;
      e := s*sign + e 
   end (*readscale*) ;
  
   procedure adjustscale; 
      var s: integer; d,t: real;
   begin if k+e > emax then error(21) else
         if k+e < emin then rnum := 0 else
     begin s := abs(e); t := 1.0; d := 10.0;
       repeat 
         while not odd(s) do  
            begin s := s div 2; d := sqr(d) 
            end ;
         s := s-1; t := d*t
       until s = 0; 
       if e >= 0 then rnum := rnum*t else rnum := rnum/t
     end 
   end (*adjustscale*) ;
  
begin (*insymbol*) 
1: while (ch = ' ') or (ch = TAB) do nextch; 
   if ch in ['a'..'z'] then
   begin (*identifier or wordsymbol*)  k := 0; id := '          ';
      repeat if k < alng then 
             begin k := k+1; id[k] := ch
             end ; 
         nextch 
      until not (ch in ['a'..'z','0'..'9']); 
      i := 1; j := nkw;   (*binary search*) 
      repeat k := (i+j) div 2;
         if id <= key[k] then j := k-1; 
         if id >= key[k] then i := k+1  
      until i > j; 
      if i-1 > j then sy := ksy[k] else sy := ident 
   end else 
   if ch in ['0'..'9'] then
   begin (*number*) k := 0; inum := 0; sy := intcon;
      repeat inum := inum*10 + ord(ch) - ord('0');
         k := k+1; nextch 
      until not (ch in ['0'..'9']);
      if (k > kmax) or (inum > nmax) then 
        begin error(21); inum := 0; k := 0
        end ; 
      if ch = '.' then
      begin nextch; 
         if ch = '.' then ch := ':' else
            begin sy := realcon; rnum := inum; e := 0;
               while ch in ['0'..'9'] do
               begin e := e-1;
                  rnum := 10.0*rnum + (ord(ch)-ord('0')); nextch
               end ;
               if ch = 'e' then readscale;
               if e <> 0 then adjustscale
            end 
      end else
      if ch = 'e' then
      begin sy := realcon; rnum := inum; e := 0; 
         readscale; if e <> 0 then adjustscale 
      end ; 
   end else
   case ch of 
':' : begin nextch; 
          if ch = '=' then
            begin sy := becomes; nextch 
            end  else sy := colon 
      end ; 
'<' : begin nextch; 
         if ch = '=' then begin sy := leq; nextch end else 
         if ch = '>' then begin sy := neq; nextch end else sy := lss 
      end ; 
'>' : begin nextch; 
         if ch = '=' then begin sy := geq; nextch end else sy := gtr 
      end ; 
'.' : begin nextch; 
         if ch = '.' then 
            begin sy := colon; nextch
            end  else sy := period
      end ; 
'''': begin k := 0; 
    2:  nextch; 
        if ch = '''' then 
          begin nextch; if ch <> '''' then goto 3 
          end ; 
        if sx+k = smax then fatal(7);
        stab[sx+k] := ch; k := k+1; 
        if cc = 1 then
          begin (*end of line*) k := 0; 
          end 
        else goto 2;
    3:  if k = 1 then 
           begin sy := charcon; inum := ord(stab[sx]) 
           end else 
        if k = 0 then 
           begin error(38); sy := charcon; inum := 0
           end else 
           begin sy := string; inum := sx; sleng := k; sx := sx+k 
           end
      end ; 
'(' : begin nextch; 
         if ch <> '*' then sy := lparent else
         begin (*comment*) nextch;
            repeat 
               while ch <> '*' do nextch;
               nextch 
            until ch = ')';
            nextch; goto 1
         end
      end ; 
'+', '-', '*', '/', ')', '=', ',', '[', ']', '#', '&', ';' :
      begin sy := sps[ch]; nextch 
      end ; 
'$', '\', '!', '?', '@', '_', '"', '^' :
      begin error(24); nextch; goto 1
      end 
   end;
end (*insymbol*) ; 
  
