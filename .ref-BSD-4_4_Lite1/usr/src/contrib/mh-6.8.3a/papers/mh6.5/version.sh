if [ "x$1" = x ]; then echo 'usage: version.sh top-level-file' 1>&2; exit 1; fi

if [ ! -r $1.major ]; then
    echo 1 > $1.major
    rm -f $1.minor
fi
if [ ! -r $1.minor ]; then echo 0 > $1.minor; fi

echo '\begingroup' > $1.vrsn
echo '    \catcode`\#=12' >> $1.vrsn

echo `cat $1.major $1.minor` | \
awk '	{ major = $1; minor = $2 + 1}\
END	{ printf "    \\gdef\\versiontag/{#%d.%d}%%\n", major, minor >> "'$1.vrsn'"; \
	  printf "%d\n", minor > "'$1.minor'"; }'

echo '    \gdef\versiondate/{'`date`'}%' >> $1.vrsn
echo '\endgroup' >> $1.vrsn
echo '\typeout{Version \versiontag/ of \versiondate/}' >> $1.vrsn
