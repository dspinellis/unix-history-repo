	echo "ProYAM/ZCOMM script to upload minirb and rz/sz to *nix"
	if S>1200 pt1
	ena -t
	if !fminirb.c echo "Can't find minirb.c !!";  abort
	putw "stty -echo; cat >minirb.c\r"
	f -xHr minirb.c
	putw "\r\4"
	putw "stty echo\r"
	dis -h
	pat 1 "rwx"
	pat 2 "%"
	put "cc minirb.c -o minirb; ls -l minirb\r"
	wait -f120
	if 1 goto okok
	echo "The compiiation appears to have failed."
	echo "Please compile minirb.c to minirb, then"
	echo "hit F5 to upload the rest of the rz/sz files."
	set f5
@putw minirb\r; sb README zmodem.h zm.c sz.c rz.c crctab.c rbsb.c *.1 gz ptest.sh
	t
	return
okok:	echo "Minirb Compilation Appears Successful."
	put minirb\r
	sb README ?akefile zmodem.h zm.c sz.c rz.c crctab.c rbsb.c *.1 gz ptest.sh
	t
	return
