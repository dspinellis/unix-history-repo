rm -f /tmp/whatis /tmp/whatis$$
cd /usr/man
for i in man1 man2 man3 man4 man5 man6 man7 man8
do
	cd $i
	/usr/lib/getNAME *.*
	cd ..
done >/tmp/whatis
ed - /tmp/whatis <<\!
g/\\-/s//-/
g/\\\*-/s//-/
g/ VAX-11/s///
1,$s/.TH [^ ]* \([^ 	]*\).*	\([^-]*\)/\2(\1)	/
g/	 /s//	/g
w /tmp/whatis2
q
!
/usr/ucb/expand -24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100 /tmp/whatis2 | sort >/tmp/whatis$$
/usr/ucb/unexpand -a /tmp/whatis$$ > /usr/lib/whatis
chmod 644 /usr/lib/whatis
rm -f /tmp/whatis /tmp/whatis$$
