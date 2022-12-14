//
//  Day14.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-14.
//

import Foundation

class Day14: AoCSolution {
	let SAND_POINT = AoCCoord2D(x: 500, y: 0)
	let FALL_OFFSETS = [AoCCoord2D(x:  0, y: 1), // Down
						AoCCoord2D(x: -1, y: 1), // Down, left
						AoCCoord2D(x:  1, y: 1)] // Down, right
	
	override init() {
		super.init()
		day = 14
		name = "Regolith Reservoir"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: false)
		let part1 = solvePartOne(input)
		print("Part One: there were \(part1) units of sand.")
		let part2 = solvePartTwo(input)
		print("Part Two: there were \(part2) units of sand.")

		return AoCResult(part1: String(part1), part2: String(part2))
	}
	
	private func solvePartOne(_ input: [String]) -> Int {
		let grid = parseGrid(input)
		let ext = grid.extent
		
		var sandCount = 0
		
		while (true) {
			let restPoint = dropSand(in: grid)
			if ext.contains(restPoint) {
				sandCount += 1
			}
			else {
				break
			}
		}
		
		return sandCount
	}
	
	private func solvePartTwo(_ input: [String]) -> Int {
		let grid = parseGrid(input)
		var ext = grid.extent
		
		let yMax = ext.max.y + 2
		drawLine(start: AoCCoord2D(x: 490-yMax, y: yMax), end: AoCCoord2D(x: 510+yMax, y: yMax), in: grid)
		ext = grid.extent
		
		var sandCount = 0
		
		while (true) {
			let restPoint = dropSand(in: grid)
			if restPoint == SAND_POINT {
				sandCount += 1
				break
			}
			else {
				sandCount += 1
			}
			//if (sandCount % 1000 == 0) { print ("\(sandCount)")}
		}
		
		return sandCount
	}

	private func dropSand(in grid: AoCGrid2D) -> AoCCoord2D {
		let ext = grid.extent
		var point = SAND_POINT
		while (ext.contains(point)) {
			var bMoved = false;
			for offset in FALL_OFFSETS {
				let checkPoint = point + offset
				if (grid.value(at: checkPoint) == grid.defaultValue) {
					point = checkPoint
					bMoved = true;
					break
				}
			}
			if (bMoved == false) {
				// Came to rest
				grid.setValue("o", at: point)
				return point
			}
		}
		return point
	}
	
	private func parseGrid(_ input: [String]) -> AoCGrid2D {
		let grid = AoCGrid2D(defaultValue: ".")
		
		for line in input {
			let pointDefs = line.split(separator: " -> ").map { String($0) }
			var points = [AoCCoord2D]()
			for pd in pointDefs {
				let xy = pd.split(separator: ",").map { Int($0)! }
				points.append(AoCCoord2D(x: xy.first!, y: xy.last!))
			}
			for c in 1..<points.count {
				drawLine(start: points[c-1], end: points[c], in: grid)
			}
		}
		
		grid.setValue("+", at: SAND_POINT)
		
		return grid
	}
	
	private func drawLine(start: AoCCoord2D, end: AoCCoord2D, in grid: AoCGrid2D) {
		//print("drawLine from \(start) to \(end)")
		let delta = end - start
		let md = start.manhattanDistance(to: end)
		let offset = AoCCoord2D(x: delta.x/md, y: delta.y/md)
		
		var drawCoord = start
		grid.setValue("#", at: drawCoord)
		//print("\t \(drawCoord)")
		for _ in 0..<md {
			drawCoord = drawCoord + offset
			grid.setValue("#", at: drawCoord)
			//print("\t \(drawCoord)")
		}
	}
}
