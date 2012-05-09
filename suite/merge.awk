#!/usr/bin/awk -f

# Merges the lines that start with the same field.
# The resulting line contains the common first field and all the other fields 
# of all lines.
# Usage: ./merge.awk <file>

BEGIN { sched = 0; s = ""; }

{
	if ($1 != sched) { if (s != "") { print s; }; s = $1; sched = $1; }
	for (i = 2; i <= NF; i++) { s = s " " $i; }
}

END { if (s != "") { print s; } }

