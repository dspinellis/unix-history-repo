: "Convert from B 2.9 to 2.10 spool format, with dots turned into slashes."
L=$1
S=$2
cat $L/active >> xtmp
ed - xtmp << 'X'
g; ;s; .*;;
1,$s;.*;ln & &/*;
1,$s;\.;/;
1,$s;\.\([^ ]* \);/\1;
1,$s; \(.*\) \(.*\); \2 \1;
w
q
X
cat xtmp | (cd $S ; sh -v)
rm -f a.out parents xtmp
