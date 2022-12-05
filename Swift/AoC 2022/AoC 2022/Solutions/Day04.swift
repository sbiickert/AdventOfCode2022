//
//  Day04.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-04.
//

import Foundation

class Day04: AoCSolution {
	override init() {
		super.init()
		day = 4
		name = "Camp Cleanup"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let pairs = parseInput(lines: input)
		
		var containCount = 0
		var overlapCount = 0
		
		for pair in pairs {
			if pair.r1.overlaps(pair.r2) { overlapCount += 1}
			if (pair.r1.contains(pair.r2) || pair.r2.contains(pair.r1)) {
				containCount += 1
			}
		}
		
		print("Part One: The number of pairs with ranges containing each other is \(containCount)")
		print("Part Two: The number of pairs with overlapping ranges is \(overlapCount)")

		return AoCResult(part1: String(containCount), part2: String(overlapCount))
	}
	
	private func parseInput(lines: [String]) -> [ElfPair] {
		var result = [ElfPair]()
		let regex = /(\d+)-(\d+),(\d+)-(\d+)/
		
		for line in lines {
			let match = line.firstMatch(of: regex)!
			let (_, min1, max1, min2, max2) = match.output
			let ePair = ElfPair(r1: Int(String(min1))!...Int(String(max1))!,
								r2: Int(String(min2))!...Int(String(max2))!)
			result.append(ePair)
		}
		
		return result
	}
}

private struct ElfPair {
	let r1: ClosedRange<Int>
	let r2: ClosedRange<Int>
}
