procedure interpret;
  (*global code, tab, btab*) 
  var ir: order;      (*instruction buffer*)
      pc: integer;    (*program counter*) 
      ps: (run,fin,caschk,divchk,inxchk,stkchk,linchk,lngchk,redchk); 
      t:  integer;    (*top stack index*) 
      b:  integer;    (*base index*)
      lncnt, ocnt, blkcnt, chrcnt: integer;     (*counters*)
      h1,h2,h3,h4: integer;
      fld: array [1..4] of integer;     (*default field widths*)
  
      display: array [1..lmax] of integer;
      s: array [1..stacksize] of          (*blockmark:              *)
         record case types of             (*   s[b+0] = fct result  *)
           ints:  (i: integer);           (*   s[b+1] = return adr  *)
           reals: (r: real);              (*   s[b+2] = static link *)
           bools: (b: boolean);           (*   s[b+3] = dynamic link*)
           chars: (c: char)               (*   s[b+4] = table index *)
         end ;
  
begin (*interpret*) 
  s[1].i := 0; s[2].i := 0; s[3].i := -1; s[4].i := btab[1].last; 
  b := 0; display[1] := 0;
  t := btab[2].vsize - 1; pc := tab[s[4].i].adr; 
  ps := run;
  lncnt := 0; ocnt := 0; chrcnt := 0;
  fld[1] := 10; fld[2] := 22; fld[3] := 10; fld[4] := 1;
  repeat ir := code[pc]; pc := pc+1; ocnt := ocnt + 1;
    case ir.f of
  0: begin (*load address*) t := t+1;
       if t > stacksize then ps := stkchk 
         else s[t].i := display[ir.x] + ir.y
     end ;
  1: begin (*load value*) t := t+1; 
       if t > stacksize then ps := stkchk 
         else s[t] := s[display[ir.x] + ir.y] 
     end ;
  2: begin (*load indirect*) t := t+1; 
       if t > stacksize then ps := stkchk 
         else s[t] := s[s[display[ir.x] + ir.y].i]  
     end ;
  3: begin (*update display*) 
       h1 := ir.y; h2 := ir.x; h3 := b; 
       repeat display[h1] := h3; h1 := h1-1; h3 := s[h3+2].i
       until h1 = h2
     end ;
  8: case ir.y of
      0: s[t].i := abs(s[t].i); 
      1: s[t].r := abs(s[t].r); 
      2: s[t].i := sqr(s[t].i); 
      3: s[t].r := sqr(s[t].r); 
      4: s[t].b := odd(s[t].i); 
      5: begin 
           if (s[t].i < 0) or (s[t].i > 127) then ps := inxchk 
	else s[t].c := chr(s[t].i)
         end ;
      6: s[t].i := ord(s[t].c) ;
      7: s[t].c := succ(s[t].c);
      8: s[t].c := pred(s[t].c);
      9: s[t].i := round(s[t].r); 
     10: s[t].i := trunc(s[t].r); 
     11: s[t].r := sin(s[t].r); 
     12: s[t].r := cos(s[t].r); 
     13: s[t].r := exp(s[t].r); 
     14: s[t].r := ln(s[t].r);
     15: s[t].r := sqrt(s[t].r);
     16: s[t].r := arctan(s[t].r);
     17: begin t := t+1;
           if t > stacksize then ps := stkchk else s[t].b := eof(input) 
         end ;
     18: begin t := t+1;
           if t > stacksize then ps := stkchk else s[t].b := eoln(input)
         end ;
     19:
     end ;
  9: s[t].i := s[t].i + ir.y;   (*offset*)
 10: pc := ir.y;  (*jump*)
 11: begin (*conditional jump*) 
       if not s[t].b then pc := ir.y;  t := t-1
     end ;
 12: begin (*switch*) h1 := s[t].i; t := t-1; 
       h2 := ir.y; h3 := 0;
       repeat if code[h2].f <> 13 then  
                begin h3 := 1; ps := caschk 
                end else
              if code[h2].y = h1 then
                begin h3 := 1; pc := code[h2+1].y 
                end else  
              h2 := h2 + 2
       until h3 <> 0 
     end ;
 14: begin (*for1up*) h1 := s[t-1].i;
       if h1 <= s[t].i then s[s[t-2].i].i := h1 else
          begin t := t-3; pc := ir.y
          end 
     end ;
 15: begin (*for2up*) h2 := s[t-2].i; h1 := s[h2].i + 1;
       if h1 <= s[t].i then
         begin s[h2].i := h1; pc := ir.y end
       else t := t-3; 
     end ;
 16: begin (*for1down*) h1 := s[t-1].i; 
       if h1 >= s[t].i then s[s[t-2].i].i := h1 else
          begin pc := ir.y; t := t-3
          end 
     end ;
 17: begin (*for2down*) h2 := s[t-2].i; h1 := s[h2].i - 1; 
       if h1 >= s[t].i then
         begin s[h2].i := h1; pc := ir.y end
       else t := t-3; 
     end ;
 18: begin (*mark stack*)  h1 := btab[tab[ir.y].ref].vsize; 
       if t+h1 > stacksize then ps := stkchk else 
         begin t := t+5; s[t-1].i := h1-1; s[t].i := ir.y
         end
     end ;
 19: begin (*call*) h1 := t - ir.y;  (*h1 points to base*) 
       h2 := s[h1+4].i;            (*h2 points to tab*) 
       h3 := tab[h2].lev; display[h3+1] := h1;
       h4 := s[h1+3].i + h1; 
       s[h1+1].i := pc; s[h1+2].i := display[h3]; s[h1+3].i := b; 
       for h3 := t+1 to h4 do s[h3].i := 0; 
       b := h1; t := h4; pc := tab[h2].adr
     end ;
 20: begin (*index1*) h1 := ir.y;      (*h1 points to atab*)
       h2 := atab[h1].low; h3 := s[t].i;
       if h3 < h2 then ps := inxchk else
       if h3 > atab[h1].high then ps := inxchk else 
         begin t := t-1; s[t].i := s[t].i + (h3-h2) 
         end
     end ;
 21: begin (*index*)  h1 := ir.y;      (*h1 points to atab*)
       h2 := atab[h1].low; h3 := s[t].i;
       if h3 < h2 then ps := inxchk else
       if h3 > atab[h1].high then ps := inxchk else 
         begin t := t-1; s[t].i := s[t].i + (h3-h2)*atab[h1].elsize
         end
     end ;
 22: begin (*load block*) h1 := s[t].i; t := t-1; 
       h2 := ir.y + t; if h2 > stacksize then ps := stkchk else 
       while t < h2 do
         begin t := t+1; s[t] := s[h1]; h1 := h1+1
         end
     end ;
 23: begin (*copy block*) h1 := s[t-1].i; 
       h2 := s[t].i; h3 := h1 + ir.y;
       while h1 < h3 do 
         begin s[h1] := s[h2]; h1 := h1+1; h2 := h2+1 
         end ;
       t := t-2 
     end ;
 24: begin (*literal*) t := t+1;
       if t > stacksize then ps := stkchk else s[t].i := ir.y 
     end ;
 25: begin (*load real*) t := t+1;
       if t > stacksize then ps := stkchk else s[t].r := rconst[ir.y] 
     end ;
  64,65: begin t := t + 1; if t > stacksize then ps := stkchk
	else s[t].c := chr(ir.y) end;
 26: begin (*float*) h1 := t - ir.y; s[h1].r := s[h1].i 
     end ;
 27: begin (*read*) 
       if eof(input) then ps := redchk else 
          case ir.y of
           1: read(s[s[t].i].i);
           2: read(s[s[t].i].r);
           4: read(s[s[t].i].c);
          end ; 
       t := t-1 
     end ;
 28: begin (*write string*)
       h1 := s[t].i; h2 := ir.y; t := t-1;
       chrcnt := chrcnt+h1; if chrcnt > lineleng then ps := lngchk;
       repeat write(stab[h2]); h1 := h1-1; h2 := h2+1 
       until h1 = 0 
     end ;
 29: begin (*writ1*) 
       chrcnt := chrcnt + fld[ir.y];
       if chrcnt > lineleng then ps := lngchk else
       case ir.y of 
        1: write(s[t].i: fld[1]); 
        2: write(s[t].r: fld[2]); 
        3: write(s[t].b: fld[3]); 
	4: write(s[t].c);
       end ;
       t := t-1 
     end ;
 30: begin (*write2*) 
       chrcnt := chrcnt + s[t].i; 
       if chrcnt > lineleng then ps := lngchk else
       case ir.y of 
        1: write(s[t-1].i: s[t].i); 
        2: write(s[t-1].r: s[t].i); 
        3: write(s[t-1].b: s[t].i); 
        4: write(s[t-1].c: s[t].i); 
       end ;
       t := t-2 
     end ;
 31: ps := fin; 
 32: begin (*exit procedure*) 
       t := b-1; pc := s[b+1].i; b := s[b+3].i
     end ;
 33: begin (*exit function*) 
       t := b; pc := s[b+1].i; b := s[b+3].i
     end ;
 34: s[t] := s[s[t].i]; 
 35: s[t].b := not s[t].b;
 36: s[t].i := - s[t].i;
 66: s[t].r := - s[t].r;
 37: begin chrcnt := chrcnt + s[t-1].i; 
       if chrcnt > lineleng then ps := lngchk else
          write(s[t-2].r: s[t-1].i: s[t].i);
       t := t-3 
     end ;
 38: begin (*store*) s[s[t-1].i] := s[t]; t := t-2
     end ;
 39: begin t := t-1; s[t].b := s[t].r = s[t+1].r 
     end ;
 40: begin t := t-1; s[t].b := s[t].r <> s[t+1].r 
     end ;
 41: begin t := t-1; s[t].b := s[t].r < s[t+1].r 
     end ;
 42: begin t := t-1; s[t].b := s[t].r <= s[t+1].r 
     end ;
 43: begin t := t-1; s[t].b := s[t].r > s[t+1].r 
     end ;
 44: begin t := t-1; s[t].b := s[t].r >= s[t+1].r 
     end ;
 45: begin t := t-1; s[t].b := s[t].i = s[t+1].i 
     end ;
 46: begin t := t-1; s[t].b := s[t].i <> s[t+1].i 
     end ;
 47: begin t := t-1; s[t].b := s[t].i < s[t+1].i 
     end ;
 48: begin t := t-1; s[t].b := s[t].i <= s[t+1].i 
     end ;
 49: begin t := t-1; s[t].b := s[t].i > s[t+1].i 
     end ;
 50: begin t := t-1; s[t].b := s[t].i >= s[t+1].i 
     end ;
 51: begin t := t-1; s[t].b := s[t].b or s[t+1].b 
     end ;
 52: begin t := t-1; s[t].i := s[t].i + s[t+1].i 
     end ;
 53: begin t := t-1; s[t].i := s[t].i - s[t+1].i 
     end ;
 54: begin t := t-1; s[t].r := s[t].r + s[t+1].r; 
     end ;
 55: begin t := t-1; s[t].r := s[t].r - s[t+1].r; 
     end ;
 56: begin t := t-1; s[t].b := s[t].b and s[t+1].b
     end ;
 57: begin t := t-1; s[t].i := s[t].i * s[t+1].i 
     end ;
 58: begin t := t-1;
       if s[t+1].i = 0 then ps := divchk else 
         s[t].i := s[t].i div s[t+1].i 
     end ;
 59: begin t := t-1;
       if s[t+1].i = 0 then ps := divchk else 
         s[t].i := s[t].i mod s[t+1].i 
     end ;
 60: begin t := t-1; s[t].r := s[t].r * s[t+1].r; 
     end ;
 61: begin t := t-1;
	if s[t+1].r = 0.0 then ps := divchk else s[t].r := s[t].r / s[t+1].r; 
     end ;
 62: if eof(input) then ps := redchk else readln; 
 63: begin writeln; lncnt := lncnt + 1; chrcnt := 0;
        if lncnt > linelimit then ps := linchk
     end 
    end (*case*) ; 
  until ps <> run; 
  
  if ps <> fin then 
  begin writeln;
    write('0halt at', pc:5, ' because of ');
    case ps of
      caschk: writeln('undefined case');
      divchk: writeln('division by 0'); 
      inxchk: writeln('invalid index'); 
      stkchk: writeln('storage overflow');
      linchk: writeln('too much output'); 
      lngchk: writeln('line too long'); 
      redchk: writeln('reading past end of file');
    end ; 
    h1 := b; blkcnt := 10;   (*post mortem dump*) 
    repeat writeln; blkcnt := blkcnt - 1; 
      if blkcnt = 0 then h1 := 0; h2 := s[h1+4].i;
      if h1<>0 then 
        writeln(' ', tab[h2].name, ' called at', s[h1+1].i: 5); 
      h2 := btab[tab[h2].ref].last; 
      while h2 <> 0 do 
      with tab[h2] do 
      begin if obj = variable then
            if typ in stantyps then 
            begin write('    ', name, ' = '); 
              if normal then h3 := h1+adr else h3 := s[h1+adr].i; 
              case typ of 
               ints:  writeln(s[h3].i); 
               reals: writeln(s[h3].r); 
               bools: writeln(s[h3].b); 
               chars: writeln(s[h3].c); 
              end
            end ;
            h2 := link
      end ; 
      h1 := s[h1+3].i 
    until h1 < 0;
  end ;
  writeln; writeln(ocnt, ' steps')
end (*interpret*) ; 
