//
//  AoCUtil.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-02.
//

import Foundation
import OSLog
import Algorithms

class AoCSolution {
	var day: Int = 0
	var name: String = ""
	var emptyLinesIndicateMultipleInputs: Bool = true
	@discardableResult func solve(filename: String, index: Int) -> AoCResult {
		print("Day \(String(format: "%02d", arguments: [day])): \(name) input: \(filename) [\(index)]");
		return AoCResult(part1: "", part2: "")
	}
}

struct AoCInput {
	let solution: AoCSolution
	let fileName: String
	let index: Int
	var id: String {
		return "\(fileName)[\(index)]"
	}
}

struct AoCResult {
	let part1: String?
	let part2: String?
}

class AoCUtil {
	public static let INPUT_FOLDER = "~/Developer/Advent of Code/2022/AdventOfCode2022/input"
	public static let ALPHABET = "abcdefghijklmnopqrstuvwxyz"

	public static var solutions: [AoCSolution] {
		get {
			return [Day01(), Day02(), Day03()
				].reversed()
		}
	}

	public static func inputs(for solution: AoCSolution) -> [AoCInput] {
		var inputs = [AoCInput]()
		// Challenge input. N = 1
		inputs.append(AoCInput(solution: solution, fileName: AoCUtil.fileName(day: solution.day, isTest: false), index: 0))
		
		// Test input. N = 0..N
		if solution.emptyLinesIndicateMultipleInputs {
			let testGroups = AoCUtil.readGroupedInputFile(
				named: AoCUtil.fileName(day: solution.day, isTest: true))
			for (i, _) in testGroups.enumerated() {
				inputs.append(AoCInput(solution: solution, fileName: AoCUtil.fileName(day: solution.day, isTest: true), index: i))
			}
		}
		else {
			inputs.append(AoCInput.init(solution: solution, fileName: AoCUtil.fileName(day: solution.day, isTest: true), index: 0))
		}
		
		return inputs
	}

	public static func fileName(day: Int, isTest: Bool) -> String {
		return "\(String(format: "day%02d", arguments: [day]))_\(isTest ? "test" : "challenge").txt"
	}
	
	public static func inputPath(for fileName: String) -> URL {
		// If this is failing, check:
		// 1. Sandboxing removed from Targets > Signing and Capabilities
		// 2. Updated INPUT_FOLDER to current year
		let input_folder = NSString(string: INPUT_FOLDER).expandingTildeInPath
		let folderPath = URL(fileURLWithPath: input_folder, isDirectory: true)
		let filePath = folderPath.appendingPathComponent(fileName)
		//print(filePath)
		return filePath
	}

	public static func readInputFile(named name:String, removingEmptyLines removeEmpty:Bool) -> [String] {
		var results = [String]()
		do {
			let path = inputPath(for: name)
			let data = try Data(contentsOf: path)
			if let input = String(data: data, encoding: .utf8) {
				results = input.components(separatedBy: "\n")
			}
			else {
				os_log("Could not decode \(path) as UTF-8")
			}
		} catch {
			os_log("Could not read file \(name)")
		}
		if removeEmpty {
			results = results.filter { $0.count > 0 }
		}
		return results
	}
	
	public static func readGroupedInputFile(named name: String, group: Int) -> [String] {
		let result = [String]()
		guard group >= 0 else { return result }
		
		let groups = readGroupedInputFile(named: name)
		guard group < groups.count else { return result }
		
		return groups[group]
	}
	
	public static func readGroupedInputFile(named name: String) -> [[String]] {
		var results = [[String]]()
		let lines = readInputFile(named: name, removingEmptyLines: false)
		
		var group = [String]()
		for line in lines {
			if line.count > 0 {
				group.append(line)
			}
			else {
				results.append(group)
				group = [String]()
			}
		}
		if group.count > 0 {
			results.append(group)
		}
		
		return results
	}
	
	static func rangeToArray(r: Range<Int>) -> [Int] {
		var result = [Int]()
		for i in r {
			result.append(i)
		}
		return result
	}
	
	static func cRangeToArray(r: ClosedRange<Int>) -> [Int] {
		var result = [Int]()
		for i in r {
			result.append(i)
		}
		return result
	}
	
	static func numberToIntArray(_ n: String) -> [Int] {
		let arr = Array(n)
		return arr.map { Int(String($0))! }
	}
}
