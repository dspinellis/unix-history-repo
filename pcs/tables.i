procedure enter(x0: alfa; x1: object;
                x2: types; x3: integer);
begin t := t+1;   (*enter standard identifier*)
   with tab[t] do
   begin name := x0; link := t-1; obj := x1;
      typ := x2; ref := 0; normal := true;
      lev := 0; adr := x3 
   end
end (*enter*) ; 
  
procedure enterarray(tp: types; l,h: integer);
begin if l > h then error(27);
   if (abs(l)>xmax) or (abs(h)>xmax) then
      begin error(27); l := 0; h := 0; 
      end ; 
   if a = amax then fatal(4) else 
      begin a := a+1; 
        with atab[a] do 
            begin inxtyp := tp; low := l; high := h 
            end 
      end 
end (*enterarray*) ;
  
procedure enterblock; 
begin if b = bmax then fatal(2) else
      begin b := b+1; btab[b].last := 0; btab[b].lastpar := 0 
      end 
end (*enterblock*) ;
  
procedure enterreal(x: real); 
begin if c2 = c2max-1 then fatal(3) else
      begin rconst[c2+1] := x; c1 := 1; 
         while rconst[c1] <> x do  c1 := c1+1; 
         if c1 > c2 then c2 := c1 
      end 
end (*enterreal*) ; 
  
procedure emit(fct: integer); 
begin if lc = cmax then fatal(6); 
   code[lc].f := fct; lc := lc+1
end (*emit*) ;
  
procedure emit1(fct,b: integer);
begin if lc = cmax then fatal(6); 
   with code[lc] do 
      begin f := fct; y := b end ;
   lc := lc+1 
end (*emit1*) ; 
  
procedure emit2(fct,a,b: integer);
begin if lc = cmax then fatal(6); 
   with code[lc] do 
     begin f := fct; x := a; y := b end ; 
   lc := lc+1 
end (*emit2*) ; 
  
procedure printtables;
   var i: integer; o: order; 
begin 
   writeln('0identifiers          link  obj  typ  ref  nrm  lev  adr'); 
   for i := btab[1].last +1 to t do 
      with tab[i] do
      writeln(i,' ',name,link:5, ord(obj):5, ord(typ):5, ref:5, 
            ord(normal):5, lev:5, adr:5); 
   writeln('0blocks    last lpar psze vsze'); 
   for i := 1 to b do 
      with btab[i] do 
      writeln(i, last:5, lastpar:5, psize:5, vsize:5);
   writeln('0arrays    xtyp etyp eref  low high elsz size');
   for i := 1 to a do 
      with atab[i] do 
      writeln(i, ord(inxtyp):5, ord(eltyp):5, 
              elref:5, low:5, high:5, elsize:5, size:5);
   writeln('0code:'); 
   for i := 0 to lc-1 do
   begin if i mod 5 = 0 then 
         begin writeln; write(i:5)
         end ;
      o := code[i]; write(o.f:5); 
      if o.f < 31 then
        if o.f < 4 then write(o.x:2, o.y:5) 
                    else write(o.y:7)
      else write('       '); 
      write(',')
   end ; 
   writeln
end (*printtables*) ; 
  
