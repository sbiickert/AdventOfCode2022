//
//  Day03.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-03.
//

import Foundation

class Day03: AoCSolution {
	let priority: Dictionary<String, Int>;
	
	override init() {
		var temp = Dictionary<String,Int>()
		let allLetters = AoCUtil.ALPHABET.split(separator: "") + AoCUtil.ALPHABET.uppercased().split(separator: "")
		for (i, letter) in allLetters.enumerated() {
			temp[String(letter)] = i+1
		}
		priority = temp
		
		super.init()
		day = 3
		name = "Rucksack Reorganization"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let sum1 = solvePartOne(input: input)
		let sum2 = solvePartTwo(input: input)

		return AoCResult(part1: String(sum1), part2: String(sum2))
	}
	
	private func solvePartOne(input: [String]) -> Int {
		var sumPriorities = 0
		for line in input {
			let half:Int = Int(line.count / 2)
			let part1 = line[line.startIndex..<line.index(line.startIndex, offsetBy: half)]
			let part2 = line[line.index(line.startIndex, offsetBy: half)..<line.endIndex]
			//print("\(part1) \(part2)")
			let set1 = Set<String>(part1.split(separator: "").map { String($0) })
			let set2 = Set<String>(part2.split(separator: "").map { String($0) })
			let intersection = set1.intersection(set2)
			assert(intersection.count == 1)
			let commonItem = intersection.first!
			//print("The common item is \(commonItem)")
			sumPriorities += priority[commonItem]!
		}
		
		print("The sum of priorities of common items in compartment 1 and 2 of the rucksacks is \(sumPriorities)")
		
		return sumPriorities
	}
	
	private func solvePartTwo(input: [String]) -> Int {
		var sumPriorities = 0
		for g in stride(from: 0, to: input.count, by: 3) {
			let set0 = Set<String>(input[g+0].split(separator: "").map { String($0) })
			let set1 = Set<String>(input[g+1].split(separator: "").map { String($0) })
			let set2 = Set<String>(input[g+2].split(separator: "").map { String($0) })
			var intersection = set0.intersection(set1)
			intersection = intersection.intersection(set2)
			assert(intersection.count == 1)
			let commonItem = intersection.first!
			//print("The common item is \(commonItem)")
			sumPriorities += priority[commonItem]!
		}
		
		print("The sum of priorities of common items in each elf group is \(sumPriorities)")
		
		return sumPriorities
	}
}
