//
//  Day06.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-06.
//

import Foundation
import Algorithms

class Day06: AoCSolution {
	override init() {
		super.init()
		day = 6
		name = "Tuning Trouble"
		emptyLinesIndicateMultipleInputs = true
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readGroupedInputFile(named: filename, group: index)
		
		let index1 = indexOfMarkerEnd(markerLength: 4, in: input.first!)
		let index2 = indexOfMarkerEnd(markerLength: 14, in: input.first!)
		
		return AoCResult(part1: String(index1), part2: String(index2))
	}
	
	private func indexOfMarkerEnd(markerLength length: Int, in signal: String) -> Int {
		let windows = signal.windows(ofCount: length)
		for (i, window) in windows.enumerated() {
			let s = Set(window)
			if s.count == length {
				print("\(window) was all unique characters.")
				return i + length
			}
		}
		return -1
	}
}
