#!/usr/bin/perl -w

# Plots a diagram based on the given input data, with the given title and the 
# given X- and Y-axis labels, and stores it in the given file (in PS format).
# Usage: ./plot.pl <title> <X_axis_label> <Y_axis_label> <input_file> <output_file>

use strict;

sub usage {
	print "Usage: plot.pl TITLE X_AXIS_LABEL Y_AXIS_LABEL INPUT_FILE OUTPUT_FILE\n";
    exit(1);
}

sub main {
	
	local *PIPE;

	# Not enough arguments.
	if (@ARGV < 3) {
		usage
	}

	my $title = $ARGV[0];
	$title =~ s/_/\\_/g;
	my $xaxislabel = $ARGV[1];
	my $yaxislabel = $ARGV[2];
	my $infile = $ARGV[3];
	my $outfile = $ARGV[4];

	# Generate the gnuplot scipt that plots the diagram.
	open PIPE, "| gnuplot" || die "Something went wrong with gnuplot\n";

	print PIPE "set title '$title'\n";
	print PIPE "set autoscale\n";
	print PIPE "set key right font ',14'\n";
	print PIPE "set xtic auto\n";
	print PIPE "set ytic auto\n";
	print PIPE "set xlabel '$xaxislabel'\n";
	print PIPE "set ylabel '$yaxislabel'\n";
	print PIPE "set term post eps enhanced color 14\n";
	print PIPE "set output '$outfile'\n";
	print PIPE "set grid\n";
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
			print PIPE "\"$infile\" using 1:" . ($ci + 1) . " title \"$token\" w l lw 2";
		}
	}
	print PIPE "\nexit\n";
	close PIPE;
}

main;

