#!/usr/bin/env perl

package AOC::Grid;
use Modern::Perl 2018;

$AOC::Grid::neighbor_rule = 'rook'; # bishop, queen/king

# not implemented
#$AOC::Grid::wrap = 0; # set true to allow wrapping

package Grid2D;
	use Moose;

	has 'width' =>   ('is' => 'rw', 'isa' => 'Int', 'default' => 5);
	has 'height' =>  ('is' => 'rw', 'isa' => 'Int', 'default' => 5);
	has 'default' => ('is' => 'rw', 'isa' => 'Str', 'default' => '.');
	has 'data' =>	 ('is' => 'rw', 'isa' => 'HashRef[Str]', 'default' => sub { {} });
	
	sub get {
		my ($self, $row, $col) = @_;
		my $key = "$row:$col";
		return $self->data->{$key} || $self->default;
	}

	sub set {
		my ($self, $value, $row, $col) = @_;
		my $key = "$row:$col";
		$self->data->{$key} = $value;
	}

	sub coords {
		my $self = shift;
		my @coords;
		for my $row (0..$self->height-1) {
			for my $col (0..$self->width-1) {
				push(@coords, [$row, $col]);
			}
		}
		return @coords;
	}
	
	sub neighbor_offsets {
		my ($self, $row, $col) = @_;
		my @offsets;
		if ($AOC::Grid::neighbor_rule ne 'bishop') {
			push(@offsets, [-1, 0]);
			push(@offsets, [ 1, 0]);
			push(@offsets, [ 0,-1]);
			push(@offsets, [ 0, 1]);
		}
		if ($AOC::Grid::neighbor_rule ne 'rook') {
			push(@offsets, [-1,-1]);
			push(@offsets, [ 1,-1]);
			push(@offsets, [-1, +1]);
			push(@offsets, [ 1, +1]);
		}
		return @offsets;
	}
	
	sub neighbor_coords {
		my ($self, $row, $col) = @_;
		my @coords;
		for my $offset ($self->neighbor_offsets()) {
			push(@coords, [$row+$$offset[0], $col+$$offset[1]]);
		}
		return @coords;
	}
	
	sub count_with {
		my ($self, $value) = @_;
		my $count = 0;
		for my $c ($self->coords) {
			$count++ if ($self->get(@$c) eq $value);
		}
		return $count;
	}
	
	sub count_neighbors_with {
		my ($self, $value, $row, $col) = @_;
		my $count = 0;
		for my $c ($self->neighbor_coords($row, $col)) {
			$count++ if ($self->get(@$c) eq $value);
		}
		return $count;
	}
	
	sub draw {
		my $self = shift;
		for my $row (0..$self->height-1) {
			for my $col (0..$self->width-1) {
				print $self->get($row, $col) || $self->default;
			}
			print "\n";
		}
		print "\n";
	}
	
	sub equals {
		my ($self, $other) = @_;
		return 0 if ($self->width != $other->width || $self->height != $other->height);
		for my $c ($self->coords()) {
			return 0 if ($self->get(@$c) ne $other->get(@$c));
		}
		return 1;
	}

	no Moose;
__PACKAGE__->meta->make_immutable;

return 1;