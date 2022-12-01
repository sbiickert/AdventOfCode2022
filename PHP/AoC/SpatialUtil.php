<?php

class Coord2D {
	protected int $x;
	protected int $y;
	
	public static function fromString(string $xyString): Coord2D {
		preg_match('/\[(-?\d+),(-?\d+)\]/', $xyString, $matches);
		return new Coord2D($matches[1], $matches[2]);
	}
	
	public static function zero(): Coord2D {
		return new Coord2D(0,0);
	}
		
	function __construct(int $x, int $y) {
		$this->x = $x;
		$this->y = $y;
	}
	
	public function getX(): int { return $this->x; }
	public function setX(int $value): int {
		$this->x = $value;
		return $this->x;
	}

	public function getY(): int { return $this->y; }
	public function setY(int $value): int {
		$this->y = $value;
		return $this->y;
	}
	
	public function equalTo(Coord2D $other): bool {
		return ($this->x == $other->x && $this->y == $other->y);
	}
	
	public function deltaTo(Coord2D $other): Coord2D {
		$delta = new Coord2D($other->x - $this->x, $other->y - $this->y);
		return $delta;
	}
	
	public function manhattanDistanceTo(Coord2D $other): int {
		$delta = $this->deltaTo($other);
		return abs($delta->x) + abs($delta->y);
	}
	
	public function distanceTo(Coord2D $other): float {
		$delta = $this->deltaTo($other);
		return sqrt($delta->x * $delta->x + $delta->y * $delta->y);
	}
	
	public function clone(): Coord2D {
		return new Coord2D($this->x, $this->y);
	}
	
	public function toString(): string {
		return "[$this->x,$this->y]";
	}
}

class Extent2D {
	protected Coord2D $min;
	protected Coord2D $max;
	
	static function build(array $coords): Extent2D {
		assert(count($coords) >= 2, 'Not enough points passed to Extent2D::build');
		$ext = new Extent2D($coords[0], $coords[1]);
		for ($i = 2; $i < count($coords); $i++) {
			$ext->expandToFit($coords[$i]);
		}
		return $ext;
	}
		
	function __construct(Coord2D $a, Coord2D $b) {
		# Not going to trust inputs are min and max
		$xmin = min($a->getX(), $b->getX());
		$xmax = max($a->getX(), $b->getX());
		$ymin = min($a->getY(), $b->getY());
		$ymax = max($a->getY(), $b->getY());
		$this->min = new Coord2D($xmin, $ymin);
		$this->max = new Coord2D($xmax, $ymax);
	}
	
	function expandToFit(Coord2D $c) {
		$this->min->setX( min( $this->min->getX(), $c->getX() ) );
		$this->min->setY( min( $this->min->getY(), $c->getY() ) );
		$this->max->setX( max( $this->max->getX(), $c->getX() ) );
		$this->max->setY( max( $this->max->getY(), $c->getY() ) );
	}
	
	function getMin(): Coord2D {
		return $this->min->clone();
	}
	
	function getMax(): Coord2D {
		return $this->max->clone();
	}
	
	function getWidth(): int {
		return $this->max->getX() - $this->min->getX() + 1;
	}
	
	function getHeight(): int {
		return $this->max->getY() - $this->min->getY() + 1;
	}
	
	function getArea(): int {
		return $this->getWidth() * $this->getHeight();
	}
	
	function getAllCoords(): array {
		$coords = array();
		foreach (range($this->min->getX(), $this->max->getX()) as $x) {
			foreach (range($this->min->getY(), $this->max->getY()) as $y) {
				array_push( $coords, new Coord2D($x, $y) );
			}
		}
		return $coords;
	}
	
	function equalTo(Extent2D $other): bool {
		return ($this->min->equalTo($other->min) && $this->max->equalTo($other->max));
	}
	
	function contains(Coord2D $c): bool {
		return $this->min->getX() <= $c->getX() && $c->getX() <= $this->max->getX() &&
			$this->min->getY() <= $c->getY() && $c->getY() <= $this->max->getY();
	}
	
	function toString(): string {
		return '{min: [' . $this->min->getX() . ',' . $this->min->getY() . '], max: [' . $this->max->getX() . ',' . $this->max->getY() . ']}';
	}
}