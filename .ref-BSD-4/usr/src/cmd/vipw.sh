cp /dev/null /etc/vipw.lock
chmod 0 /etc/vipw.lock
ln /etc/vipw.lock /etc/ptmp > /dev/null 2>& 1
case $? in
0)
	trap '' 1 2 3 15
	vi /etc/passwd
	rm /etc/ptmp
	;;
*)
	echo Temporary file busy, try again later.
	;;
esac
