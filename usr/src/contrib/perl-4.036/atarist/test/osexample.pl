require 'osbind.pl';

 &Cconws("Hello World\r\n");
 $str = "This is a string being printed by Fwrite Gemdos trap\r\n";
 &Fwrite(1, length($str), $str);
