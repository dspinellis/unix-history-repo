: '@(#)rnews	1.6 of 16 Dec 82'
PATH=/bin:/usr/bin ; export PATH
cat > /tmp/rn$$ 
inews -p < /tmp/rn$$ >/tmp/rnm$$ 2>&1
if test -s /tmp/rnm$$ && grep -s -v '^Duplicate article '
then
	( echo rnews ; cat /tmp/rnm$$ ; \
		echo ----- ; cat /tmp/rn$$ ; echo ----- ) | mail usenet
fi
rm -f /tmp/rn$$ /tmp/rnm$$
