//
//  Day05.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-05.
//

import Foundation
import RegexBuilder

class Day05: AoCSolution {
	override init() {
		super.init()
		day = 5
		name = "Supply Stacks"
		emptyLinesIndicateMultipleInputs = false
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let stack_input = AoCUtil.readGroupedInputFile(named: filename, group: 0)
		let instr_input = AoCUtil.readGroupedInputFile(named: filename, group: 1)
		
		let stacks = parseStacks(input: stack_input)
		let instructions = parseInstructions(input: instr_input)
		
		let topsPartOne = solvePartOne(stacks, instructions)
		let topsPartTwo = solvePartTwo(stacks, instructions)

		return AoCResult(part1: topsPartOne, part2: topsPartTwo)
	}
	
	private func solvePartOne(_ stacks: [[String]], _ instructions: [CraneInstruction]) -> String {
		var mStacks = stacks // mutable copy
		for instruction in instructions {
			for _ in (1...instruction.num) {
				let crate = mStacks[instruction.from].popLast()!
				mStacks[instruction.to].append(crate)
			}
		}
		
		let result = getResult(mStacks)
		return result
	}
	
	private func solvePartTwo(_ stacks: [[String]], _ instructions: [CraneInstruction]) -> String {
		var mStacks = stacks // mutable copy
		for instruction in instructions {
			var crates = [String]()
			for _ in (1...instruction.num) {
				let crate = mStacks[instruction.from].popLast()!
				crates.insert(crate, at: 0)
			}
			mStacks[instruction.to].append(contentsOf: crates)
		}
		
		let result = getResult(mStacks)
		return result
	}

	private func getResult(_ stacks: [[String]]) -> String {
		var result = ""
		for i in 1..<stacks.count {
			result += stacks[i].last!
		}
		return result
	}
	
	private func parseStacks(input: [String]) -> [[String]] {
		var reversed = [String](input.reversed())
		let colsLine = reversed.remove(at: 0).trimmingCharacters(in: .whitespaces) // column numbers
		let start = colsLine.index(before: colsLine.endIndex)
		let nCols: Int = Int(String(colsLine[start..<colsLine.endIndex]))!
		
		var stacks = [[String]](repeating: [String](), count: nCols + 1)

		let regex = Regex {
			Capture {
				("A"..."Z")
			}
		}
		
		for var line in reversed {
			for i in (1...nCols) {
				var prefix = ""
				if i < nCols {
					let mid = line.index(line.startIndex, offsetBy: 4)
					prefix = String(line[line.startIndex..<mid])
					line = String(line[mid..<line.endIndex])
				}
				else {
					prefix = line
				}
				if let match = prefix.firstMatch(of: regex) {
					stacks[i].append(String(match.output.1))
				}
			}
		}
		
		return stacks
	}
	
	private func parseInstructions(input: [String]) -> [CraneInstruction] {
		var instructions = [CraneInstruction]()
		
		let regex = Regex {
			"move "
			Capture {
				OneOrMore(.digit)
			}
			" from "
			Capture {
				OneOrMore(.digit)
			}
			" to "
			Capture {
				OneOrMore(.digit)
			}
		}
		for line in input {
			let match = line.firstMatch(of: regex)
			let num: Int = Int(String(match!.output.1))!
			let from: Int = Int(String(match!.output.2))!
			let to: Int = Int(String(match!.output.3))!
			instructions.append(CraneInstruction(num: num, from: from, to: to))
		}
		
		return instructions
	}
}

private struct CraneInstruction {
	let num: Int
	let from: Int
	let to: Int
}
