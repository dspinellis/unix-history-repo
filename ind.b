main $(
   extrn read, write;
   auto i, c, state, line 100;

loop:
   state = i = 0;
loop1:
   c = read();
   if(c==4) return;
   if(c==':' & state==0) state = 2;
   if((c<'0' ^ c>'9'&c<'a' ^ c>'z') & state==0) state = 1;
   line[i] = c;
   i = i+1;
   if(c!=012) goto loop1;
   if(state==2 ^ i==1) goto noi;
   write('  ');
   write(' ');
noi:
   i = 0;
loop3:
   c = line[i];
   write(c);
   i = i+1;
   if(c!=012) goto loop3;
   goto loop;
$)
