#!/usr/bin/awk -f

# This script reads lines that contain time and generates lines that contain 
# speedup.
# E.g. <#s> <args1> <time1> <args2> <time2>... -> 
# <#s> <args1> <speedup1> <args2> <speedup2>...
# Usage: ./speedup.awk <file>
 
BEGIN { base[""] = 0; }

{
	for (i = 1; i <= NF; i++) {
		# Do not touch the first or any even field.
		if (i == 1 || i % 2 == 0) { 
			printf $i; 
		}
		else {
			# If the first field is equal to 1, then the times found in this 
			# line should be used as the reference point for the speedup 
			# calculation.
			if ($1 == 1) {
				base[i] = $i;
			}
			# Calculate the speedup and replace the time with it.
			printf("%.3f", base[i]/$i);
		}
		printf " "
	}
	printf("\n");
	next
}

