	echo "ProYAM/ZCOMM script to upload rz/sz files to VMS"
	if !fvmodem.h echo "Can't find vmodem.h !!";  abort
	pat
	pat 1 "Server"
	pat 2 "unrecognized command verb"
	put "kermit server\r"
	wait -f15
	if 1 goto frog
	if !2 echo "Unexpected response from VMS!"; abort
	expand vuplfile.t vmodem.h rz.c sz.c vrzsz.c vvmodem.c 
	expand vuplfile.t zm.c zmr.c zmodem.h crctab.c init.com
	echo "vupl.t finished."
	return
frog:	send vmodem.h rz.c sz.c vrzsz.c vvmodem.c zm.c
	send zmodem.h crctab.c init.com
	finish
	echo "vupl.t finished."
	return
