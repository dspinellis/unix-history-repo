procedure block(fsys: symset; isfun: boolean; level: integer);
  
   type conrec = 
      record case tp: types of 
         ints,chars,bools: (i: integer); 
         reals: (r: real)
      end ;
   
   var dx: integer;    (*data allocation index*)  
       prt: integer;   (*t-index of this procedure*) 
       prb: integer;   (*b-index of this procedure*)
       x: integer;  
   procedure skip(fsys: symset; n: integer);
   begin error(n); 
      while not (sy in fsys) do insymbol 
   end (*skip*) ;
 
   procedure test(s1,s2: symset; n: integer); 
   begin if not (sy in s1) then  
         skip(s1+s2,n)
   end (*test*) ;
  
   procedure testsemicolon;
   begin 
     if sy = semicolon then insymbol else 
     begin error(14); 
       if sy in [comma,colon] then insymbol 
     end ;
     test([ident]+blockbegsys, fsys, 6) 
   end (*testsemicolon*) ;
 
   procedure enter(id: alfa; k: object); 
      var j,l: integer;
   begin if t = tmax then fatal(1) else 
         begin tab[0].name := id;
            j := btab[display[level]].last;  l := j; 
            while tab[j].name <> id do  j := tab[j].link;
            if j <> 0 then error(1) else 
            begin t := t+1;
              with tab[t] do 
              begin name := id; link := l;
               obj := k; typ := notyp; ref := 0; lev := level; adr := 0 
              end ; 
              btab[display[level]].last := t
            end
         end 
   end (*enter*) ;  
     
   function loc(id: alfa): integer;
      var i,j: integer;     (*locate id in table*)  
   begin i := level; tab[0].name := id;   (*sentinel*) 
      repeat j := btab[display[i]].last; 
         while tab[j].name <> id do  j := tab[j].link;
         i := i-1;  
      until (i<0) or (j<>0);  
      if j = 0 then error(0);  loc := j 
   end (*loc*) ; 
  
  procedure entervariable;
  begin if sy = ident then
          begin enter(id,variable); insymbol
          end 
        else error(2) 
  end (*entervariable*) ; 
     
   procedure constant(fsys: symset; var c: conrec); 
     var x, sign: integer; 
   begin c.tp := notyp; c.i := 0; 
     test(constbegsys, fsys, 50);
     if sy in constbegsys then 
     begin 
         if sy = charcon then 
           begin c.tp := chars; c.i := inum; insymbol
           end 
         else
           begin sign := 1;
             if sy in [plus,minus] then 
               begin if sy = minus then sign := -1; 
                 insymbol
               end ; 
             if sy = ident then
               begin x := loc(id); 
                 if x <> 0 then 
                   if tab[x].obj <> konstant then error(25) else
                   begin c.tp := tab[x].typ; 
                     if c.tp = reals then c.r := sign*rconst[tab[x].adr]
                                     else c.i := sign*tab[x].adr
                   end ; 
                 insymbol
               end  
             else
             if sy = intcon then 
               begin c.tp := ints; c.i := sign*inum; insymbol
               end else
             if sy = realcon then
               begin c.tp := reals; c.r := sign*rnum; insymbol 
               end else skip(fsys,50)
           end;
         test(fsys, [], 6) 
       end 
   end (*constant*) ; 
   procedure typ(fsys: symset; var tp: types; var rf, sz: integer);
     var x: integer;
         eltp: types; elrf: integer; 
         elsz, offset, t0,t1: integer; 
 
     procedure arraytyp(var aref,arsz: integer); 
        var eltp: types; 
           low, high: conrec; 
           elrf, elsz: integer; 
     begin constant([colon,rbrack,rparent,ofsy]+fsys, low); 
        if low.tp = reals then 
           begin error(27); low.tp := ints; low.i := 0 
           end ; 
        if sy = colon then insymbol else error(13);
        constant([rbrack,comma,rparent,ofsy]+fsys, high);
        if high.tp <> low.tp then 
           begin error(27); high.i := low.i
           end ; 
        enterarray(low.tp, low.i, high.i); aref := a;
        if sy = comma then 
           begin insymbol; eltp := arrays; arraytyp(elrf,elsz)
           end else 
        begin  
           if sy = rbrack then insymbol else 
              begin error(12); 
                 if sy = rparent then insymbol 
              end ; 
           if sy = ofsy then insymbol else error(8); 
           typ(fsys,eltp,elrf,elsz) 
        end ;
        with atab[aref] do 
        begin arsz := (high-low+1)*elsz; size := arsz;
           eltyp := eltp; elref := elrf; elsize := elsz 
        end ;
     end (*arraytyp*) ;
 
   begin (*typ*) tp := notyp; rf := 0; sz := 0;
     test(typebegsys, fsys, 10); 
     if sy in typebegsys then 
       begin 
         if sy = ident then
         begin x := loc(id);  
           if x <> 0 then 
           with tab[x] do 
             if obj <> type1 then error(29) else  
             begin tp := typ; rf := ref; sz := adr; 
               if tp = notyp then error(30) 
             end ;  
           insymbol 
         end else
         if sy = arraysy then 
         begin insymbol;
             if sy = lbrack then insymbol else 
                begin error(11); 
                   if sy = lparent then insymbol  
                end ;
             tp := arrays; arraytyp(rf,sz)
         end else
         begin (*records*) insymbol;
           enterblock; tp := records; rf := b;
           if level = lmax then fatal(5); 
           level := level+1; display[level] := b; offset := 0;
           while sy <> endsy do
           begin (*field section*)
             if sy = ident then 
             begin t0 := t; entervariable;
               while sy = comma do
                 begin insymbol; entervariable
                 end ;
               if sy = colon then insymbol else error(5);
               t1 := t; 
               typ(fsys+[semicolon,endsy,comma,ident],eltp,elrf,elsz);
               while t0 < t1 do 
               begin t0 := t0+1;
                 with tab[t0] do
                 begin typ := eltp; ref := elrf; normal := true;
                   adr := offset; offset := offset + elsz
                 end
               end 
             end ; 
             if sy <> endsy then
             begin if sy = semicolon then insymbol else 
                   begin error(14); 
                     if sy = comma then insymbol 
                   end ;
                test([ident,endsy,semicolon], fsys, 6)
             end
           end ;
           btab[rf].vsize := offset; sz := offset; btab[rf].psize := 0; 
           insymbol; level := level-1
         end ;
         test(fsys, [], 6) 
       end 
   end (*typ*) ; 
 
   procedure parameterlist;     (*formal parameter list*)
      var tp: types; 
          rf, sz, x, t0: integer; 
          valpar: boolean; 
   begin insymbol; tp := notyp; rf := 0; sz := 0; 
     test([ident, varsy], fsys+[rparent], 7); 
     while sy in [ident,varsy] do 
       begin if sy <> varsy then valpar := true else 
               begin insymbol; valpar := false
               end ;
         t0 := t; entervariable;
         while sy = comma do 
            begin insymbol; entervariable;
            end ;
         if sy = colon then
           begin insymbol; 
             if sy <> ident then error(2) else
             begin x := loc(id); insymbol;
               if x <> 0 then  
               with tab[x] do 
                 if obj <> type1 then error(29) else
                   begin tp := typ; rf := ref;
                     if valpar then sz := adr else sz := 1 
                   end ; 
             end ; 
             test([semicolon,rparent], [comma,ident]+fsys, 14)
           end
         else error(5); 
         while t0 < t do
         begin t0 := t0+1;
           with tab[t0] do
           begin typ := tp; ref := rf; 
               normal := valpar; adr := dx; lev := level;
               dx := dx + sz 
           end
         end ;
         if sy <> rparent then
         begin if sy = semicolon then insymbol else 
               begin error(14); 
                 if sy = comma then insymbol
               end ;
            test([ident,varsy], [rparent]+fsys, 6)
         end
       end (*while*) ; 
     if sy = rparent then
       begin insymbol; 
         test([semicolon,colon], fsys, 6) 
       end 
     else error(4)  
   end (*parameterlist*) ; 
 
   procedure constantdeclaration;
     var c: conrec; 
   begin insymbol;  
     test([ident], blockbegsys, 2); 
     while sy = ident do 
       begin enter(id,konstant); insymbol; 
         if sy = eql then insymbol else 
            begin error(16);  
               if sy = becomes then insymbol
            end ;
         constant([semicolon,comma,ident]+fsys,c);
         tab[t].typ := c.tp; tab[t].ref := 0; 
         if c.tp = reals then  
           begin enterreal(c.r); tab[t].adr := c1 end 
         else tab[t].adr := c.i;
         testsemicolon
       end 
   end (*constantdeclaration*) ; 
 
   procedure typedeclaration; 
     var tp: types; rf, sz, t1: integer;
   begin insymbol; 
     test([ident], blockbegsys, 2); 
     while sy = ident do 
       begin enter(id,type1); t1 := t; insymbol; 
         if sy = eql then insymbol else 
            begin error(16);  
               if sy = becomes then insymbol 
            end ;
         typ([semicolon,comma,ident]+fsys, tp, rf, sz); 
         with tab[t1] do
           begin typ := tp; ref := rf; adr := sz 
           end ;
         testsemicolon
       end 
   end (*typedeclaration*) ;  
 
   procedure variabledeclaration;
     var t0, t1, rf, sz: integer; 
         tp: types; 
   begin insymbol;  
     while sy = ident do
     begin t0 := t; entervariable;
       while sy = comma do 
         begin insymbol; entervariable; 
         end ; 
       if sy = colon then insymbol else error(5); 
       t1 := t; 
       typ([semicolon,comma,ident]+fsys, tp, rf, sz); 
       while t0 < t1 do 
       begin t0 := t0+1;
         with tab[t0] do 
         begin typ := tp; ref := rf; 
           lev := level; adr := dx; normal := true;
           dx := dx + sz
         end 
       end ;
       testsemicolon
     end 
   end (*variabledeclaration*) ; 
 
   procedure procdeclaration; 
      var isfun: boolean; 
   begin isfun := sy = functionsy; insymbol;
     if sy <> ident then
        begin  error(2); id := '          '
        end ;
     if isfun then enter(id,funktion) else enter(id,prozedure); 
     tab[t].normal := true;
     insymbol; block([semicolon]+fsys, isfun, level+1); 
     if sy = semicolon then insymbol else error(14);
     emit(32+ord(isfun))    (*exit*)
   end (*proceduredeclaration*) ;
  
(*---------------------------------------------------------statement--*)
  
   procedure statement(fsys: symset);
      var i: integer;
      procedure expression(fsys: symset; var x: item); forward; 
  
      procedure selector(fsys: symset; var v:item); 
         var x: item; a,j: integer; 
      begin (*sy in [lparent, lbrack, period]*)
        repeat
          if sy = period then 
          begin insymbol;  (*field selector*) 
            if sy <> ident then error(2) else
            begin
              if v.typ <> records then error(31) else
              begin (*search field identifier*)
                j := btab[v.ref] .last; tab[0].name := id; 
                while tab[j].name <> id do j := tab[j].link; 
                if j = 0 then error(0); 
                v.typ := tab[j].typ; v.ref := tab[j].ref;
                a := tab[j].adr; if a <> 0 then emit1(9,a) 
              end ; 
              insymbol
            end 
          end else 
          begin (*array selector*)
            if sy <> lbrack then error(11);
            repeat insymbol; 
              expression(fsys+[comma,rbrack], x); 
              if v.typ <> arrays then error(28) else 
                begin a := v.ref; 
                  if atab[a].inxtyp <> x.typ then error(26) else 
                if atab[a].elsize = 1 then emit1(20,a) else emit1(21,a);
                  v.typ := atab[a].eltyp; v.ref := atab[a].elref
                end 
            until sy <> comma; 
            if sy = rbrack then insymbol else 
              begin error(12); if sy = rparent then insymbol
              end
          end 
        until not (sy in [lbrack,lparent,period]); 
        test(fsys, [], 6) 
      end (*selector*) ;

      procedure call(fsys: symset; i: integer);
         var x: item; 
             lastp, cp, k: integer; 
      begin emit1(18,i);  (*mark stack*)
        lastp := btab[tab[i].ref].lastpar; cp := i; 
        if sy = lparent then 
        begin (*actual parameter list*) 
          repeat insymbol;
            if cp >= lastp then error(39) else 
            begin cp := cp+1; 
              if tab[cp].normal then
              begin (*value parameter*) 
                expression(fsys+[comma,colon,rparent], x); 
                if x.typ=tab[cp].typ then 
                  begin 
                    if x.ref <> tab[cp].ref then error(36) else
                  if x.typ = arrays then emit1(22,atab[x.ref].size) else
                    if x.typ = records then emit1(22,btab[x.ref].vsize) 
                  end else
                if (x.typ=ints) and (tab[cp].typ=reals) then
                   emit1(26,0) else 
                   if x.typ<>notyp then error(36); 
              end else
              begin (*variable parameter*)
                if sy <> ident then error(2) else 
                begin k := loc(id); insymbol; 
                  if k <> 0 then 
                  begin if tab[k].obj <> variable then error(37);
                    x.typ := tab[k].typ; x.ref := tab[k].ref; 
                    if tab[k].normal then emit2(0,tab[k].lev,tab[k].adr)
                       else emit2(1,tab[k].lev,tab[k].adr); 
                    if sy in [lbrack,lparent,period] then
                       selector(fsys+[comma,colon,rparent], x); 
                    if (x.typ<>tab[cp].typ) or (x.ref<>tab[cp].ref) then
                       error(36)
                  end 
                end 
              end
            end ;
            test([comma,rparent], fsys, 6)
          until sy <> comma;  
          if sy = rparent then insymbol else error(4) 
        end ; 
        if cp < lastp then error(39); (*too few actual parameters*)
        emit1(19, btab[tab[i].ref].psize-1);
        if tab[i].lev < level then emit2(3, tab[i].lev, level)
      end (*call*) ;
  
      function resulttype(a,b: types): types; 
      begin 
        if (a>reals) or (b>reals) then  
          begin error(33); resulttype := notyp
          end else 
        if (a=notyp) or (b=notyp) then resulttype := notyp else
        if a=ints then
          if b=ints then resulttype := ints else 
            begin resulttype := reals; emit1(26,1)
            end 
        else
          begin resulttype := reals;
            if b=ints then emit1(26,0) 
          end 
      end (*resulttype*) ;

      procedure expression;
        var y:item; op:symbol;
  
        procedure simpleexpression(fsys:symset; var x:item);
          var y:item; op:symbol;
  
          procedure term(fsys:symset; var x:item);
            var y:item; op:symbol; 
  
            procedure factor(fsys:symset; var x:item);
              var i,f: integer; 
  
              procedure standfct(n: integer); 
                 var ts: typset;
              begin (*standard function no. n*)
                if sy = lparent then insymbol else error(9);
                if n < 17 then
                  begin expression(fsys+[rparent],x); 
                    case n of 
(*abs,sqr*)      0,2:  begin ts := [ints,reals]; tab[i].typ := x.typ; 
                         if x.typ = reals then n := n+1 
                       end ; 
(*odd,chr*)      4,5:  ts := [ints];
(*ord*)          6: begin
				if x.typ = ints then n := 19;
				ts := [ints,bools,chars];
			end;
(*succ,pred*)    7,8:  ts := [chars];
(*round,trunc*)  9,10,11,12,13,14,15,16:
(*sin,cos,...*)        begin ts := [ints,reals]; 
                         if x.typ = ints then emit1(26,0)
                       end ; 
                    end ; 
                    if x.typ in ts then emit1(8,n) else 
                    if x.typ <> notyp then error(48); 
                  end else
(*eof,eoln*)      begin (*n in [17,18]*)
                    if sy <> ident then error(2) else
                    if id <> 'input     ' then error(0) else insymbol; 
                    emit1(8,n); 
                  end ; 
                x.typ := tab[i].typ;
                if sy = rparent then insymbol else error(4) 
              end (*standfct*) ;
  
            begin (*factor*) x.typ := notyp; x.ref := 0;
              test(facbegsys, fsys, 58);
              while sy in facbegsys do 
                begin 
                  if sy = ident then
                  begin i := loc(id); insymbol;
                    with tab[i] do
                    case obj of 
              konstant: begin x.typ := typ; x.ref := 0; 
			case x.typ of
				ints: emit1(24,adr);
				reals: emit1(25,adr);
				bools: emit1(64,adr);
				chars: emit1(65,adr);
			end;
                        end ; 
              variable: begin x.typ := typ; x.ref := ref;
                          if sy in [lbrack,lparent,period] then 
                            begin if normal then f := 0 else f := 1; 
                              emit2(f, lev, adr); 
                              selector(fsys,x);
                              if x.typ in stantyps then emit(34)
                            end else
                            begin 
                              if x.typ in stantyps then 
                                if normal then f := 1 else f := 2 
                              else
                                if normal then f := 0 else f := 1;
                              emit2(f, lev, adr) 
                            end 
                        end ; 
              type1, prozedure:    error(44); 
              funktion :begin x.typ := typ; 
                          if lev <> 0 then call(fsys, i) 
                                else standfct(adr)
                        end
                    end (*case,with*)
                  end else
                  if sy in [charcon,intcon,realcon] then
                   begin  
                     if sy = realcon then 
                     begin x.typ := reals; enterreal(rnum); 
                       emit1(25, c1)
                     end else 
			begin
			if sy = charcon then begin
				x.typ := chars;
				emit1(65, inum);
			end else begin
				x.typ := ints;
				emit1(24, inum);
			end;
                     end ;
                     x.ref := 0; insymbol 
                   end else
                  if sy = lparent then 
                   begin insymbol; expression(fsys+[rparent], x); 
                     if sy = rparent then insymbol else error(4)
                   end else
                  if sy = notsy then
                   begin insymbol; factor(fsys,x);
                     if x.typ=bools then emit(35) else
                       if x.typ<>notyp then error(32)
                   end ;
                  test(fsys, facbegsys, 6)
                end (*while*) 
            end (*factor*) ; 

          begin (*term*)
            factor(fsys+[times,rdiv,idiv,imod,andsy], x);
            while sy in [times,rdiv,idiv,imod,andsy] do 
              begin op := sy; insymbol; 
                factor(fsys+[times,rdiv,idiv,imod,andsy], y); 
                if op = times then
                begin x.typ := resulttype(x.typ, y.typ);
                  case x.typ of 
                    notyp: ; 
                    ints : emit(57);
                    reals: emit(60);
                  end 
                end else
                if op = rdiv then 
                begin 
                  if x.typ = ints then 
                    begin emit1(26,1); x.typ := reals 
                    end ; 
                  if y.typ = ints then 
                    begin emit1(26,0); y.typ := reals 
                    end ; 
                  if (x.typ=reals) and (y.typ=reals) then emit(61) else 
                    begin if (x.typ<>notyp) and (y.typ<>notyp) then
                            error(33); 
                          x.typ := notyp
                    end 
                end else
                if op = andsy then
                begin if (x.typ=bools) and (y.typ=bools) then 
                         emit(56) else 
                      begin if (x.typ<>notyp) and (y.typ<>notyp) then 
                               error(32); 
                         x.typ := notyp 
                      end 
                end else
                begin (*op in [idiv,imod]*) 
                  if (x.typ=ints) and (y.typ=ints) then 
                    if op=idiv then emit(58)
                               else emit(59) else 
                    begin if (x.typ<>notyp) and (y.typ<>notyp) then
                             error(34); 
                          x.typ := notyp
                    end 
                end 
              end
          end (*term*) ;

        begin (*simpleexpression*)
          if sy in [plus,minus] then
            begin op := sy; insymbol;
              term(fsys+[plus,minus], x); 
              if x.typ > reals then error(33) else
                if op = minus then
			if x.typ = reals then
				emit(66)
			else
				emit(36)
            end else
          term(fsys+[plus,minus,orsy], x);
          while sy in [plus,minus,orsy] do
            begin op := sy; insymbol;
               term(fsys+[plus,minus,orsy], y);
               if op = orsy then
               begin  
                 if (x.typ=bools) and (y.typ=bools) then emit(51) else
                   begin if (x.typ<>notyp) and (y.typ<>notyp) then
                            error(32); 
                         x.typ := notyp 
                   end
               end else 
               begin x.typ := resulttype(x.typ, y.typ); 
                 case x.typ of
                   notyp: ;
                   ints : if op = plus then emit(52)
                                   else emit(53); 
                   reals: if op = plus then emit(54)
                                   else emit(55) 
                 end
               end 
            end 
        end (*simpleexpression*) ;

      begin (*expression*)
        simpleexpression(fsys+[eql,neq,lss,leq,gtr,geq], x);
        if sy in [eql,neq,lss,leq,gtr,geq] then
          begin op := sy; insymbol; 
             simpleexpression(fsys, y); 
             if (x.typ in [ notyp,ints,bools,chars]) and (x.typ = y.typ) then
               case op of 
                 eql: emit(45); 
                 neq: emit(46); 
                 lss: emit(47); 
                 leq: emit(48); 
                 gtr: emit(49); 
                 geq: emit(50); 
               end else 
             begin if x.typ = ints then 
                     begin x.typ := reals; emit1(26,1)
                     end else 
                   if y.typ = ints then 
                     begin y.typ := reals; emit1(26,0)
                     end ;
               if (x.typ=reals) and (y.typ=reals) then
                 case op of
                   eql: emit(39); 
                   neq: emit(40); 
                   lss: emit(41); 
                   leq: emit(42); 
                   gtr: emit(43); 
                   geq: emit(44); 
                 end
               else error(35) 
             end ; 
             x.typ := bools
          end 
      end (*expression*) ;

      procedure assignment(lv,ad: integer); 
         var x,y: item; f: integer; 
         (*tab[i].obj in [variable,prozedure]*)
      begin x.typ := tab[i].typ; x.ref := tab[i].ref; 
        if tab[i].normal then f := 0 else f := 1; 
        emit2(f, lv, ad); 
        if sy in [lbrack,lparent,period] then 
           selector([becomes,eql]+fsys, x); 
        if sy = becomes then insymbol else
          begin error(51); if sy = eql then insymbol
          end ; 
        expression(fsys, y); 
        if x.typ = y.typ then 
          if x.typ in stantyps then emit(38) else 
          if x.ref <> y.ref then error(46) else
          if x.typ = arrays then emit1(23, atab[x.ref].size)
                            else emit1(23, btab[x.ref].vsize) 
        else
        if (x.typ=reals) and (y.typ=ints) then
          begin emit1(26,0); emit(38)
          end else 
          if (x.typ<>notyp) and (y.typ<>notyp) then error(46) 
      end (*assignment*) ;

      procedure compoundstatement;
      begin insymbol; 
        statement([semicolon,endsy]+fsys);
        while sy in [semicolon]+statbegsys do 
        begin if sy = semicolon then insymbol else error(14); 
          statement([semicolon,endsy]+fsys) 
        end ; 
        if sy = endsy then insymbol else error(57)
      end (*compoundstatemenet*) ;

      procedure ifstatement; 
         var x: item; lc1,lc2: integer; 
      begin insymbol; 
        expression(fsys+[thensy,dosy], x);
        if not (x.typ in [bools,notyp]) then error(17);
        lc1 := lc; emit(11);  (*jmpc*) 
        if sy = thensy then insymbol else 
          begin error(52); if sy = dosy then insymbol 
          end ; 
        statement(fsys+[elsesy]); 
        if sy = elsesy then
          begin insymbol; lc2 := lc; emit(10);
            code[lc1].y := lc; statement(fsys); code[lc2].y := lc 
          end 
        else code[lc1].y := lc
      end (*ifstatement*) ;

      procedure casestatement;
        var x: item;
            i,j,k,lc1: integer; 
            casetab: array [1..csmax] of
                       packed record val, lc: index end ;
            exittab: array [1..csmax] of integer; 
  
        procedure caselabel; 
          var lab: conrec; k: integer; 
        begin constant(fsys+[comma,colon], lab); 
          if lab.tp <> x.typ then error(47) else  
          if i = csmax then fatal(6) else 
            begin i := i+1; k := 0; 
              casetab[i].val := lab.i; casetab[i].lc := lc; 
              repeat k := k+1 until casetab[k].val = lab.i; 
              if k < i then error(1);   (*multiple definition*) 
            end 
        end (*caselabel*) ;
  
        procedure onecase;
        begin if sy in constbegsys then 
          begin caselabel;
            while sy = comma do 
              begin insymbol; caselabel 
              end ; 
            if sy = colon then insymbol else error(5);
            statement([semicolon,endsy]+fsys);
            j := j+1; exittab[j] := lc; emit(10) 
          end 
        end (*onecase*) ; 
  
      begin insymbol; i := 0; j := 0;
        expression(fsys+[ofsy,comma,colon], x);
        if not (x.typ in [ints,bools,chars,notyp]) then error(23); 
        lc1 := lc; emit(12);  (*jmpx*) 
        if sy = ofsy then insymbol else error(8); 
        onecase;
        while sy = semicolon do 
          begin insymbol; onecase 
          end ; 
        code[lc1].y := lc;
        for k := 1 to i do
          begin emit1(13,casetab[k].val); emit1(13,casetab[k].lc) 
          end ; 
        emit1(10,0);
        for k := 1 to j do code[exittab[k]].y := lc;
        if sy = endsy then insymbol else error(57)
      end (*casestatement*) ; 

      procedure repeatstatement;
         var x: item; lc1: integer; 
      begin lc1 := lc;
        insymbol; statement([semicolon,untilsy]+fsys);
        while sy in [semicolon]+statbegsys do 
        begin if sy = semicolon then insymbol else error(14); 
          statement([semicolon,untilsy]+fsys) 
        end ; 
        if sy = untilsy then 
          begin insymbol; expression(fsys, x);
            if not (x.typ in [bools,notyp]) then error(17); 
            emit1(11,lc1) 
          end 
        else error(53)
      end (*repeatstatement*) ; 

      procedure whilestatement; 
         var x: item; lc1,lc2: integer; 
      begin insymbol; lc1 := lc;
        expression(fsys+[dosy], x); 
        if not (x.typ in [bools,notyp]) then error(17);
        lc2 := lc; emit(11); 
        if sy = dosy then insymbol else error(54);
        statement(fsys); emit1(10,lc1); code[lc2].y := lc
      end (*whilestatement*) ;

      procedure forstatement; 
         var cvt: types; x: item; 
             i,f,lc1,lc2: integer;
      begin insymbol; 
        if sy = ident then
          begin i := loc(id); insymbol; 
            if i = 0 then cvt := ints else
            if tab[i].obj = variable then 
              begin cvt := tab[i].typ; 
                if not tab[i].normal then error(37) else
                  emit2(0, tab[i].lev, tab[i].adr); 
                if not (cvt in [notyp,ints,bools,chars]) then error(18)
              end else
              begin error(37); cvt := ints
              end
          end else skip([becomes,tosy,downtosy,dosy]+fsys, 2);
        if sy = becomes then 
          begin insymbol; expression([tosy,downtosy,dosy]+fsys, x);
            if x.typ <> cvt then error(19);
          end else skip([tosy,downtosy,dosy]+fsys, 51); 
        f := 14;
        if sy in [tosy, downtosy] then 
          begin if sy = downtosy then f := 16;
            insymbol; expression([dosy]+fsys, x); 
            if x.typ <> cvt then error(19) 
          end else skip([dosy]+fsys, 55); 
        lc1 := lc; emit(f);
        if sy = dosy then insymbol else error(54);
        lc2 := lc; statement(fsys); 
        emit1(f+1,lc2); code[lc1].y := lc 
      end (*forstatement*) ; 
  
      procedure standproc(n: integer); 
         var i,f: integer;
             x,y: item; 
      begin 
        case n of
   1,2: begin (*read*)
          if not iflag then
            begin error(20); iflag := true
            end ;
          if sy = lparent then
          begin 
            repeat insymbol; 
              if sy <> ident then error(2) else
              begin i := loc(id); insymbol; 
                if i <> 0 then 
                if tab[i].obj <> variable then error(37) else
                begin x.typ := tab[i].typ; x.ref := tab[i].ref; 
                  if tab[i].normal then f := 0 else f := 1; 
                  emit2(f, tab[i].lev, tab[i].adr); 
                  if sy in [lbrack,lparent,period] then 
                    selector(fsys+[comma,rparent], x);
                  if x.typ in [ints,reals,chars,notyp] then 
                    emit1(27, ord(x.typ)) else error(40)
                end 
              end ; 
              test([comma,rparent], fsys, 6); 
            until sy <> comma; 
            if sy = rparent then insymbol else error(4) 
          end ; 
          if n = 2 then emit(62)
        end ; 
   3,4: begin (*write*) 
          if sy = lparent then
          begin 
            repeat insymbol; 
              if sy = string then 
                begin emit1(24,sleng); emit1(28,inum); insymbol 
                end else
              begin expression(fsys+[comma,colon,rparent], x);
                if not (x.typ in stantyps) then error(41);  
                if sy = colon then
                begin insymbol; 
                  expression(fsys+[comma,colon,rparent], y);
                  if y.typ <> ints then error(43); 
                  if sy = colon then
                  begin if x.typ <> reals then error(42);
                    insymbol; expression(fsys+[comma,rparent], y);
                    if y.typ <> ints then error(43); 
                    emit(37) 
                  end 
                  else emit1(30, ord(x.typ))
                end 
                else emit1(29, ord(x.typ))
              end
            until sy <> comma; 
            if sy = rparent then insymbol else error(4) 
          end ; 
          if n = 4 then emit(63)
        end ; 
        end (*case*)
      end (*standproc*) ; 

    begin (*statement*) 
      if sy in statbegsys+[ident] then 
          case sy of
            ident:    begin i := loc(id); insymbol; 
                        if i <> 0 then  
                        case tab[i].obj of
                          konstant, type1: error(45); 
                          variable: assignment(tab[i].lev, tab[i].adr); 
                          prozedure:
                            if tab[i].lev <> 0 then call(fsys, i)
                                    else standproc(tab[i].adr); 
                          funktion: 
                            if tab[i].ref = display[level] then 
                              assignment(tab[i].lev+1, 0) else error(45)
                        end
                      end ;
            beginsy:  compoundstatement;
            ifsy:     ifstatement;
            casesy:   casestatement;
            whilesy:  whilestatement;
            repeatsy: repeatstatement; 
            forsy:    forstatement; 
          end;
        test(fsys, [], 14)
    end (*statement*) ; 

begin (*block*) dx := 5; prt := t;
  if level > lmax then fatal(5);
  test([lparent,colon,semicolon], fsys, 7); 
  enterblock; display[level] := b; prb := b;
  tab[prt].typ := notyp; tab[prt].ref := prb; 
  if sy = lparent then parameterlist;
  btab[prb].lastpar := t; btab[prb].psize := dx; 
  if isfun then 
    if sy = colon then
    begin insymbol;   (*function type*) 
      if sy = ident then
      begin x := loc(id); insymbol; 
        if x <> 0 then 
          if tab[x].obj <> type1 then error(29) else 
            if tab[x].typ in stantyps then tab[prt].typ := tab[x].typ 
              else error(15) 
      end else skip([semicolon]+fsys, 2)
    end else error(5);
  if sy = semicolon then insymbol else error(14); 
  repeat 
    if sy = constsy then constantdeclaration; 
    if sy = typesy then typedeclaration;
    if sy = varsy then variabledeclaration; 
    btab[prb].vsize := dx;
    while sy in [proceduresy,functionsy] do procdeclaration;
    test([beginsy], blockbegsys+statbegsys, 56)
  until sy in statbegsys; 
  tab[prt].adr := lc; 
  insymbol; statement([semicolon,endsy]+fsys);
  while sy in [semicolon]+statbegsys do 
    begin if sy = semicolon then insymbol else error(14);
      statement([semicolon,endsy]+fsys) 
    end ; 
  if sy = endsy then insymbol else error(57); 
  test(fsys+[period], [], 6) 
end (*block*) ; 
  
