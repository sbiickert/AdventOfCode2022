//
//  AoCSpatial.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-30.
//

import Foundation
import Algorithms

struct AoCCoord2D: Hashable {
	let x: Int
	let y: Int
	
	static func +(left: AoCCoord2D, right: AoCCoord2D) -> AoCCoord2D {
		return AoCCoord2D(x: left.x + right.x, y: left.y + right.y)
	}
	
	static func -(left: AoCCoord2D, right: AoCCoord2D) -> AoCCoord2D {
		return AoCCoord2D(x: left.x - right.x, y: left.y - right.y)
	}
	
	static func readingOrderSort(c0: AoCCoord2D, c1: AoCCoord2D) -> Bool {
		if c0.y == c1.y {
			return c0.x < c1.x
		}
		return c0.y < c1.y
	}

	func manhattanDistance(to other: AoCCoord2D) -> Int {
		return abs(self.x - other.x) + abs(self.y - other.y)
	}
	
	func isAdjacent(to other: AoCCoord2D, rule: AoCAdjacencyRule = .rook) -> Bool {
		switch rule {
		case .rook:
			return self.manhattanDistance(to: other) == 1
		case .bishop:
			return abs(x - other.x) == 1 && abs(y - other.y) == 1
		case .queen:
			return (self.manhattanDistance(to: other) == 1) || (abs(x - other.x) == 1 && abs(y - other.y) == 1)
		}
	}
	
	func getAdjacent(rule: AoCAdjacencyRule = .rook) -> [AoCCoord2D] {
		var result = [AoCCoord2D]()
		for offset in getAdjacentOffsets(rule: rule) {
			result.append(self + offset)
		}
		return result
	}
	
	func getAdjacentOffsets(rule: AoCAdjacencyRule = .rook) -> [AoCCoord2D] {
		switch rule {
		case .rook:
			return [AoCCoord2D(x: -1, y:  0), AoCCoord2D(x:  1, y:  0),
					AoCCoord2D(x:  0, y: -1), AoCCoord2D(x:  0, y:  1)]
		case .bishop:
			return [AoCCoord2D(x: -1, y: -1), AoCCoord2D(x:  1, y:  1),
					AoCCoord2D(x:  1, y: -1), AoCCoord2D(x: -1, y:  1)]
		case .queen:
			return [AoCCoord2D(x: -1, y:  0), AoCCoord2D(x:  1, y:  0),
					AoCCoord2D(x:  0, y: -1), AoCCoord2D(x:  0, y:  1),
					AoCCoord2D(x: -1, y: -1), AoCCoord2D(x:  1, y:  1),
					AoCCoord2D(x:  1, y: -1), AoCCoord2D(x: -1, y:  1)]
		}
	}
	
	func coord(offsetByX x: Int, y: Int) -> AoCCoord2D {
		return AoCCoord2D(x: self.x+x, y: self.y+y)
	}
	
	var description: String {
		return "[x: \(x), y: \(y)]"
	}
}

struct AoCExtent2D: Hashable {
	static func build(from coords: [AoCCoord2D]) -> AoCExtent2D {
		if let (xmin, xmax) = (coords.map { $0.x }).minAndMax(),
		   let (ymin, ymax) = (coords.map { $0.y }).minAndMax() {
			return AoCExtent2D(min: AoCCoord2D(x: xmin, y: ymin), max: AoCCoord2D(x: xmax, y: ymax))
		}
		return AoCExtent2D(min: AoCCoord2D(x: 0, y: 0), max: AoCCoord2D(x: 0, y: 0))
	}
	
	let min: AoCCoord2D
	let max: AoCCoord2D
	
	var width: Int {
		return max.x - min.x + 1
	}
	
	var height: Int {
		return max.y - min.y + 1
	}
	
	var area: Int {
		return width * height
	}
	
	func contains(_ coord: AoCCoord2D) -> Bool {
		return min.x <= coord.x && coord.x <= max.x &&
			   min.y <= coord.y && coord.y <= max.y
	}
}

enum AoCDirection: String {
	case up = "^"
	case down = "v"
	case right = ">"
	case left = "<"
}

enum AoCMapDirection: String {
	case north = "N"
	case south = "S"
	case east = "E"
	case west = "W"
}

enum AoCAdjacencyRule {
	case rook
	case bishop
	case queen
}

class AoCGrid2D {
	let defaultValue: String
	var _data = Dictionary<AoCCoord2D, String>()
	var neighbourRule: AoCAdjacencyRule = .rook
	
	init(defaultValue: String = ".") {
		self.defaultValue = defaultValue
	}
	
	var extent: AoCExtent2D {
		return AoCExtent2D.build(from: [AoCCoord2D](_data.keys))
	}

	func value(at coord: AoCCoord2D) -> String {
		if let v = _data[coord] {
			return v
		}
		return defaultValue
	}
	
	func setValue(_ v: String, at coord: AoCCoord2D) {
		_data[coord] = v
	}
	
	var coords: [AoCCoord2D] {
		return Array(_data.keys)
	}
	
	func getCoords(withValue v: String) -> [AoCCoord2D] {
		let result = _data.filter { $0.value == v }
		return Array(result.keys)
	}
	
	var counts: Dictionary<String, Int> {
		var result = Dictionary<String, Int>()
		let ext = extent
		for (row, col) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			let v = value(at: AoCCoord2D(x: col, y: row))
			if result.keys.contains(v) == false { result[v] = 0 }
			result[v]! += 1
		}
		return result
	}
	
	func neighbourOffsets(at coord: AoCCoord2D) -> [AoCCoord2D] {
		return coord.getAdjacentOffsets(rule: self.neighbourRule)
	}
	
	func neighbourCoords(at coord: AoCCoord2D) -> [AoCCoord2D] {
		return coord.getAdjacent(rule: self.neighbourRule)
	}
	
	func neighbourCoords(at coord: AoCCoord2D, withValue s: String) -> [AoCCoord2D] {
		var result = neighbourCoords(at: coord)
		result = result.filter { self.value(at: $0) == s }
		return result
	}

	func draw(markers: Dictionary<AoCCoord2D, String>? = nil) {
		let ext = extent
		for row in ext.min.y...ext.max.y {
			var values = [String]()
			for col in ext.min.x...ext.max.x {
				let coord = AoCCoord2D(x: col, y: row)
				if let markers = markers,
				   markers.keys.contains(coord) {
					values.append(markers[coord]!)
				}
				else {
					values.append(value(at: coord))
				}
			}
			print(values.joined(separator: " "))
		}
		print("")
	}
}
