.ds V 1.5j-betatest
.if t .ds E E\s-2LVIS\s+2
.if n .ds E Elvis
.\"
.\" usage: .Go <section#> <title>
.de Go
.ds LH "\\$1-\\\\n%
.ds RH "\\$1-\\\\n%
.ds CH "\\$2
.NH S \\$1
\\$2
.\"if !\\n%=1 .bp 1
.if n .ul 0
..
