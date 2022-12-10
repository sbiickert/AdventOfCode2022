//
//  Day09.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-09.
//

import Foundation

class Day09: AoCSolution {
	override init() {
		super.init()
		day = 9
		name = "Rope Bridge"
		self.emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readGroupedInputFile(named: filename, group: index)
		let moves = input.map { RopeMove(from: $0) }
		
		let p1 = run(rope: Rope(length: 2), moves: moves)
		print("Part One: the number of spaces the tail visited is \(p1)")
		let p2 = run(rope: Rope(length: 10), moves: moves)
		print("Part Two: the number of spaces the tail visited is \(p2)")

		return AoCResult(part1: String(p1), part2: String(p2))
	}
	
	private func run(rope: Rope, moves: [RopeMove]) -> Int {
		let grid = AoCGrid2D(defaultValue: ".")
		for move in moves {
			for _ in 0..<move.count {
				rope.move(move.direction)
				grid.setValue("#", at: rope.tail)
			}
		}
		
		let tailSpots = grid.counts["#"]!
		
//		var markers = Dictionary<AoCCoord2D, String>()
//		markers[AoCCoord2D(x: 0, y: 0)] = "s"
//		markers[rope.head] = "H"
//		for k in 1..<rope.knots.count {
//			markers[rope.knots[k]] = String(k)
//		}
//		grid.draw(markers: markers)

		return tailSpots
	}
}

class Rope {
	var knots: [AoCCoord2D]
	
	init(length: Int) {
		knots = [AoCCoord2D]()
		for _ in 1...length {
			knots.append(AoCCoord2D(x: 0, y: 0))
		}
	}
	
	func move(_ dir: AoCDirection) {
		knots[0] = knots[0] + dir.offset
		for k in 1..<knots.count {
			knots[k] = follow(head: knots[k-1], tail: knots[k])
		}
	}
	
	var head: AoCCoord2D {
		return knots.first!
	}
	
	var tail: AoCCoord2D {
		return knots.last!
	}
	
	private func follow(head: AoCCoord2D, tail: AoCCoord2D) -> AoCCoord2D {
		if tail.distance(to: head) > 1.5 {
			let delta = head - tail;
			var dx = 0
			var dy = 0
			if (delta.x != 0) {
				// Move 1 in x direction
				dx = (head.x < tail.x) ? -1 : 1;
			}
			if (delta.y != 0) {
				// Move 1 in y direction
				dy = (head.y < tail.y) ? -1 : 1;
			}
			return tail + AoCCoord2D(x: dx, y: dy)
		}
		return tail
	}
}

struct RopeMove {
	init(from s:String) {
		let parts = s.split(separator: " ")
		switch String(parts[0]) {
		case "U":
			direction = .up
		case "D":
			direction = .down
		case "L":
			direction = .left
		default:
			direction = .right
		}
		count = Int(String(parts[1]))!
	}
	
	let direction: AoCDirection
	let count: Int
}
