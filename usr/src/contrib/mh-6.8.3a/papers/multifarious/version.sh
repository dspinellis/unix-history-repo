if [ ! -r version ]; then echo 0 > version; fi
touch version
echo '\catcode`\#=12' > version.tex
awk '	{ version = $1 + 1; }\
END	{ printf "\\def\\versiontag/{#1.%d}%%\n", version >> "version.tex"; \
	  printf "%d\n", version > "version"; }' < version
echo '\def\versiondate/{'`date`'}%' >> version.tex
echo '\catcode`\#=6' >> version.tex
echo '\tell{Version \versiontag/ of \versiondate/}' >> version.tex
