<?php

class Coord2D {
	protected int $x;
	protected int $y;
	
	public static function fromString(string $xyString): Coord2D {
		preg_match('/\[(-?\d+),(-?\d+)\]/', $xyString, $matches);
		return new Coord2D($matches[1], $matches[2]);
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
	
	public function toString(): string {
		return "[$this->x,$this->y]";
	}
}