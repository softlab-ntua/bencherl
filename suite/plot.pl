#!/usr/bin/perl -w

# Plots a diagram based on the given input data, with the given title and 
# stores it in the given file (in PS format).
# Usage: ./plot.pl <title> <input_file> <output_file>

use strict;

sub usage {
	print "Usage: plot.pl TITLE INPUT_FILE OUTPUT_FILE\n";
    exit(1);
}

sub main {
	
	local *PIPE;

	# Not enough arguments.
	if (@ARGV < 3) {
		usage
	}

	my $title = $ARGV[0];
	my $infile = $ARGV[1];
	my $outfile = $ARGV[2];

	# Generate the gnuplot scipt that plots the diagram.
	open PIPE, "| gnuplot" || die "Somethng went wrong with gnuplot\n";

	print PIPE "set title '$title'\n";
	print PIPE "set autoscale\n";
	print PIPE "set key right outside font ',10'\n";
	print PIPE "set xtic auto\n";
	print PIPE "set ytic auto\n";
	print PIPE "set xlabel '# Schedulers'\n";
	print PIPE "set ylabel 'Time (ms)'\n";
	print PIPE "set term post eps enhanced 10 color\n";
	print PIPE "set output '$outfile'\n";
	print PIPE "plot ";
	my $line = `head -n 1 $infile`;
	my @tokens = split(/ /, $line);
	my $ci = 0;
	foreach my $token (@tokens) {
		$ci++;
		$token =~ s/^\s+//;
		$token =~ s/\s+$//;
		if ($token && !($ci % 2)) {
			if ($ci > 2) {
				print PIPE ", ";
			}
			print PIPE "\"$infile\" using 1:" . ($ci + 1) . " title \"$token\" with linespoints";
		}
	}
	print PIPE "\nexit\n";
	close PIPE;
}

main;

