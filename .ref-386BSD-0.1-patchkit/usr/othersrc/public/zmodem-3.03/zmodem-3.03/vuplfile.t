	echo "Uploading %item to VMS using DCL create command"
	pat
	if S>2400 pt1; ena -t
	putw "set terminal/hostsync/noecho\r"
	putw "create %item\r"
	sleep 10
	f -xHr %item
	pat 1 "\044"
	put "\032"
	wait -f30
	if !1 echo "create failed!"
	putw "set terminal/echo\r"
	dis -Htx
