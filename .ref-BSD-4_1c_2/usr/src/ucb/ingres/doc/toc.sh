#
foreach i ($*)
ex - $i << 'EOF'
/DESCRIPTION/i
SYNOPSIS
.
0a
.s3
.i0
.in +4
.ti -4
XXX
.
.,/^\.th/-d
s/\.th \(.*\) \(.*\) \(.*\)$/\1(\2) \\-/
+,/NAME/d
s/^/- /
s/^.*- //
-,.join
+,/SYNOPSIS/c
.br
.
g/SYNOPSIS/d
1,/DESCRIPTION/-w! >> ../toc.nr
q!
'EOF'
end
