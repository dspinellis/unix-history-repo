#! /bin/sh
# generate prototypes, if your style is the same as mine
for i
do
	rm $i:r.pro 2>/dev/null
	grep -v '[{};:#]' $i | grep '^[A-Za-z]' |
		grep -v static | sed 's/$/;/' >! $i:r.pro
done
