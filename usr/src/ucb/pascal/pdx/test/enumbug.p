From peter@UCBERNIE Wed Apr 13 15:22:29 1983
Date: 13 Apr 83 14:04:39 PST (Wed)
From: peter@UCBERNIE (peter b. kessler)
Subject: try ``pi -t t.p ; obj'' and then ``print variant.b''
Message-Id: <8304132204.AA00797@UCBERNIE.ARPA>
Received: by UCBERNIE.ARPA (3.320/3.12)
	id AA00797; 13 Apr 83 14:04:39 PST (Wed)
Received: from UCBERNIE.ARPA by UCBARPA.ARPA (3.332/3.19)
	id AA01057; 13 Apr 83 15:22:23 PST (Wed)
To: linton@UCBERNIE
Status: R

program t(output);
    var
	variant : record case boolean of
			true: (b:boolean);
			false: (i:integer);
		    end;
    begin
	variant.i := -1;
	writeln(variant.b);
    end.

