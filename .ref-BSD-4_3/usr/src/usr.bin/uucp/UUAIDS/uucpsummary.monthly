cat <<'E_O_F' >/tmp/$$a
BEGIN {
	printf("\n\t\t\t\tUUCP Traffic Summary\n\n")
	printf("\t\t\t\tFor the previous 30 days\n\n")
	printf("\t       Calls     Minutes      Files          Bytes      Effective Unused\n")
	printf("Site          To/From    To/From     To/From      Sent/Received  Baudrate Bwidth\n")
E_O_F
awk '$1 !~ /#/ && $1 !~ /xxx/{print $1,$3}' /usr/lib/uucp/L.sys | sort -u |
sed 's/\(.*\) \(.*\)/	type["\1"] = "\2"/' >>/tmp/$$a
cat <<'E_O_F' >>/tmp/$$a
}
{
		how = type[$1]
		tcallsto[how] += $2
		tcallsfrom[how] += $3
		tminto[how] += $4
		tminfrom[how] += $5
		tfilesto[how] += $6
		tfilesfrom[how] += $7
		tbytesto[how] += $8
		tbytesfrom[how] += $9
		tbaudrate[how] += $10
		tpercent[how] += $11
		tfailed[how] += $12
		tretry[how] += $13
		tn[how]++
}
name == $1 {
	callsto += $2
	callsfrom += $3
	minto += $4
	minfrom += $5
	filesto += $6
	filesfrom += $7
	bytesto += $8
	bytesfrom += $9
	baudrate += $10
	percent += $11
	failed += $12
	retry += $13
	n++
}
name != $1 {
	if (NR > 1) {
		s = name
		if (failed > 0) {
			s = s "[" failed
			if (retry > 0)
				s = s "/" retry "]"
			else
				s = s "]"
		} else {
			if (retry > 0)
				s = s "[/" retry "]"
		}
		printf("%-12s %4d/%-4d %5d/%-5d %5d/%-5d %9d/%-9d %6d %5d%%\n", \
		s, callsto, callsfrom, minto, minfrom, filesto, \
		filesfrom, bytesto, bytesfrom, baudrate/n, percent/n)
	}

	callsto = $2
	callsfrom = $3
	minto = $4
	minfrom = $5
	filesto = $6
	filesfrom = $7
	bytesto = $8
	bytesfrom = $9
	baudrate = $10
	percent = $11
	failed = $12
	retry = $13
	n = 1
	name = $1
}
END {
	printf("%-12s %4d/%-4d %5d/%-5d %5d/%-5d %9d/%-9d %6d %5d%%\n", \
		name, callsto, callsfrom, minto, minfrom, filesto, \
		filesfrom, bytesto, bytesfrom, baudrate/n, percent/n)
	printf("------------ ----/---- -----/----- -----/----- ---------/--------- ------  -----\n")
	for (i in tn) {

		s = i
		if (tfailed[i] > 0) {
			s = s "[" tfailed[i]
			if (tretry[i] > 0)
				s = s "/" tretry[i] "]"
			else 
				s = s "]"
		} else {
			if (tretry[i] > 0)
				s = s "[/" tretry[i] "]"
		}
		printf("%-12s %4d/%-4d %5d/%-5d %5d/%-5d %10d/%-10d %6d %3d%%\n", \
		s, tcallsto[i], tcallsfrom[i], tminto[i], tminfrom[i], \
		tfilesto[i], tfilesfrom[i], tbytesto[i], tbytesfrom[i], \
		tbaudrate[i]/tn[i], tpercent[i]/tn[i])
		totcallsto += tcallsto[i]
		totcallsfrom += tcallsfrom[i]
		totminto += tminto[i]
		totminfrom += tminfrom[i]
		totfilesto += tfilesto[i]
		totfilesfrom += tfilesfrom[i]
		totbytesto += tbytesto[i]
		totbytesfrom += tbytesfrom[i]
		totbaudrate += tbaudrate[i]
		totpercent += tpercent[i]
		totfailed += tfailed[i]
		totretry += tretry[i]
		totn += tn[i]
	}
	printf("%-12s %4d/%-4d %5d/%-5d %5d/%-5d %10d/%-10d %6d %3d%%\n", \
		"Total", totcallsto, totcallsfrom, totminto, \
		totminfrom, totfilesto, totfilesfrom, totbytesto, \
		totbytesfrom ,  totbaudrate/totn,  totpercen/totn)
}
E_O_F
for i in `grep -l 'UUCP Summary' /usr/msgs/*`
do
sed '1,/^Site/d
/^[ ]*$/,$d
s;\[/;[0/;
s;\[\([0-9]*\)\];[\1/0];
/\[/s;\([^\[]*\)\[\(.*\)/\(.*\)\]\(.*\);\1 \4 \2 \3;
s!/! !g
s/  */ /g
s/%//
' $i
done | sort | awk -f /tmp/$$a
rm -f /tmp/$$*
