#!/usr/bin/awk -f

# Injects "what" just after the first parenthesis it finds in each line.
# E.g. ([500,500]) -> (what, [500,500])
# Usage: ./inject.awk what=<what> <file>

{
	for (i=1; i<= NF; i++) {
		if (index($i, "(")) {
			printf "(" what "," substr($i, 2)
		}
		else {
			printf $i
		}
		if (i < NF) {
			printf " ";
		}
	}
	printf "\n";
}

