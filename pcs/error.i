  
procedure errormsg; 
   var k: integer; 
       msg: array [0..ermax] of alfa;
begin 
  msg[ 0] := 'undef id  '; msg[ 1] := 'multi def '; 
  msg[ 2] := 'identifier'; msg[ 3] := 'program   '; 
  msg[ 4] := ')         '; msg[ 5] := ':         '; 
  msg[ 6] := 'syntax    '; msg[ 7] := 'ident, var'; 
  msg[ 8] := 'of        '; msg[ 9] := '(         '; 
  msg[10] := 'id, array '; msg[11] := '[         '; 
  msg[12] := ']         '; msg[13] := '..        '; 
  msg[14] := ';         '; msg[15] := 'func. type'; 
  msg[16] := '=         '; msg[17] := 'boolean   '; 
  msg[18] := 'convar typ'; msg[19] := 'type      '; 
  msg[20] := 'prog.param'; msg[21] := 'too big   '; 
  msg[22] := '.         '; msg[23] := 'typ (case)'; 
  msg[24] := 'character '; msg[25] := 'const id  '; 
  msg[26] := 'index type'; msg[27] := 'indexbound'; 
  msg[28] := 'no array  '; msg[29] := 'type id   '; 
  msg[30] := 'undef type'; msg[31] := 'no record '; 
  msg[32] := 'boole type'; msg[33] := 'arith type'; 
  msg[34] := 'integer   '; msg[35] := 'types     '; 
  msg[36] := 'param type'; msg[37] := 'variab id '; 
  msg[38] := 'string    '; msg[39] := 'no.of pars'; 
  msg[40] := 'type      '; msg[41] := 'type      '; 
  msg[42] := 'real type '; msg[43] := 'integer   '; 
  msg[44] := 'var, const'; msg[45] := 'var, proc '; 
  msg[46] := 'types (:=)'; msg[47] := 'typ (case)'; 
  msg[48] := 'type      '; msg[49] := 'store ovfl'; 
  msg[50] := 'constant  '; msg[51] := ':=        '; 
  msg[52] := 'then      '; msg[53] := 'until     '; 
  msg[54] := 'do        '; msg[55] := 'to downto '; 
  msg[56] := 'begin     '; msg[57] := 'end       '; 
  msg[58] := 'factor    ';
  k := 0; writeln; writeln(' key words'); 
  while errs <> [] do
  begin while not (k in errs) do k := k+1;
        writeln(k,'  ',msg[k]); errs := errs - [k]
  end 
end (*errormsg*) ; 
  
  
procedure error(n: integer); 
begin if errpos = 0 then write(' ****');
   if cc > errpos then
      begin write(' ': cc-errpos, '^', n:2);
         errpos := cc+3; errs := errs + [n] 
      end 
end (*error*) ; 
  
procedure fatal(n: integer); 
   var msg: array [1..7] of alfa; 
begin writeln; errormsg;
   msg[ 1] := 'identifier'; msg[ 2] := 'procedures';
   msg[ 3] := 'reals     '; msg[ 4] := 'arrays    ';
   msg[ 5] := 'levels    '; msg[ 6] := 'code      ';
   msg[ 7] := 'strings   ';
   writeln(' compiler table for ', msg[n], ' is too small');
   goto 99    (* terminate compilation*)
end (*fatal*) ; 
  
