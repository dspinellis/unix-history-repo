main $(
   auto ch;
   extrn read, write;

   goto loop;
   while (ch != 04)
      $( if (ch > 0100 & ch < 0133)
            ch = ch + 040;
      if (ch==015) goto loop;
      if (ch==014) goto loop;
      if (ch==011)
       $( ch = 040040;
          write(040040);
          write(040040);
          $)
      write(ch);
   loop:
      ch = read()&0177;
      $)
  $)