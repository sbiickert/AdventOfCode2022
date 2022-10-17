#!/usr/bin/env perl

package AOC::Geometry;
use Modern::Perl 2018;

package Point2D;
	use Moose;
	
	has 'px' => (is => 'rw', isa => 'Int');
	has 'py' => (is => 'rw', isa => 'Int');
	
	sub move {
		my ($self, $dx, $dy) = @_;
		$self->px( $self->px + $dx );
		$self->py( $self->py + $dy );
	}
	
	sub debugStr {
		my $self = shift;
		return "P2D x: " . $self->px . " y: " . $self->py;
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;

package Line2D;
	use Moose;
	
	has 'from' => 	(is => 'rw', isa => 'Point2D');
	has 'to' => 	(is => 'rw', isa => 'Point2D');
	
	sub dx {
		my $self = shift;
		return $self->to->px - $self->from->px;
	}
	
	sub dy {
		my $self = shift;
		return $self->to->py - $self->from->py;
	}
	
	sub manhattan_distance {
		my $self = shift;
		return abs($self->dx) + abs($self->dy);
	}

	no Moose;
__PACKAGE__->meta->make_immutable;

package Bounds2D;
	use Moose;
	use List::Util qw(min max);
	
	has 'xmin' => 	(is => 'rw', isa => 'Int', default => 0);
	has 'xmax' => 	(is => 'rw', isa => 'Int', default => 0);
	has 'ymin' => 	(is => 'rw', isa => 'Int', default => 0);
	has 'ymax' => 	(is => 'rw', isa => 'Int', default => 0);
	
	sub expand {
		my ($self, $amt) = @_;
		$self->xmin($self->xmin - $amt);
		$self->xmax($self->xmax + $amt);
		$self->ymin($self->ymin - $amt);
		$self->ymax($self->ymax + $amt);
	}
	
	sub grow_to_fit {
		my ($self, $pt) = @_;
		$self->xmin($pt->px) if $pt->px < $self->xmin;
		$self->xmax($pt->px) if $pt->px > $self->xmax;
		$self->ymin($pt->py) if $pt->py < $self->ymin;
		$self->ymax($pt->py) if $pt->py > $self->ymax;
	}
	
	sub shrink_to_fit {
		my ($self, $pt) = @_;
		$self->xmin($pt->px);
		$self->xmax($pt->px);
		$self->ymin($pt->py);
		$self->ymax($pt->py);
	}
	
	sub fit_to_points {
		my $self = shift;
		my @points = @_;
		my @x = (); my @y = ();
		for my $pt (@points) {
			push(@x, $pt->px);
			push(@y, $pt->py);
		}
		$self->xmin(min(@x));
		$self->xmax(max(@x));
		$self->ymin(min(@y));
		$self->ymax(max(@y));
	}
	
	sub size {
		my $self = shift;
		return Point2D->new('px' => ($self->xmax - $self->xmin), 'py' => ($self->ymax - $self->ymin));
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;	

package Point3D;
	use Moose;
	
	extends 'Point2D';
	
	has 'pz' => (is => 'rw', isa => 'Int');
	
	sub dz {
		my $self = shift;
		return $self->to->pz - $self->from->pz;
	}
	
	no Moose;
__PACKAGE__->meta->make_immutable;

package Line3D;
	use Moose;
	
	has 'from' => 	(is => 'rw', isa => 'Point3D');
	has 'to' => 	(is => 'rw', isa => 'Point3D');
	
	sub dx {
		my $self = shift;
		return $self->to->px - $self->from->px;
	}
	
	sub dy {
		my $self = shift;
		return $self->to->py - $self->from->py;
	}
	
	sub dz {
		my $self = shift;
		return $self->to->pz - $self->from->pz;
	}

	no Moose;
__PACKAGE__->meta->make_immutable;
	
1;