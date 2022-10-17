//
//  AoCStrings.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-30.
//

import Foundation

extension String {
	subscript(offset: Int) -> Character {
		get {
			self[index(startIndex, offsetBy: offset)]
		}
		set {
			let idx = self.index(startIndex, offsetBy: offset)
			self.replaceSubrange(idx...idx, with: [newValue])
		}
	}
}

extension String {
	func indicesOf(string: String) -> [Int] {
		var indices = [Int]()
		var searchStartIndex = self.startIndex

		while searchStartIndex < self.endIndex,
			let range = self.range(of: string, range: searchStartIndex..<self.endIndex),
			!range.isEmpty
		{
			let d = distance(from: self.startIndex, to: range.lowerBound)
			indices.append(d)
			searchStartIndex = self.index(after: self.index(startIndex, offsetBy: d))
		}

		return indices
	}
}

extension NSRegularExpression {
	convenience init(_ pattern: String) {
		do {
			try self.init(pattern: pattern)
		} catch {
			preconditionFailure("Illegal regular expression: \(pattern).")
		}
	}

	func matches(_ string: String) -> Bool {
		let range = NSRange(location: 0, length: string.utf16.count)
		return firstMatch(in: string, options: [], range: range) != nil
	}

	func positionalMatches(_ string: String) -> [String] {
		var result = [String]()
		let range = NSRange(location: 0, length: string.utf16.count)
		if let match = firstMatch(in: string, range: range) {
			for i in 1..<match.numberOfRanges {
				let r = match.range(at: i)
				let low = string.index(string.startIndex, offsetBy: r.lowerBound)
				let hi = string.index(string.startIndex, offsetBy: r.upperBound)
				result.append(String(string[low..<hi]))
			}
		}
		return result
	}
	
	func allMatches(_ string: String) -> [String] {
		var result = [String]()
		var searchRange = NSRange(location: 0, length: string.utf16.count)
		var matchRange = self.rangeOfFirstMatch(in: string, range: searchRange)
		
		while (matchRange.length > 0) {
			let low = string.index(string.startIndex, offsetBy: matchRange.lowerBound)
			let hi = string.index(string.startIndex, offsetBy: matchRange.upperBound)
			result.append(String(string[low..<hi]))
			searchRange = NSRange(location: matchRange.upperBound, length: string.utf16.count - matchRange.upperBound)
			if searchRange.length <= 0 { break }
			matchRange = self.rangeOfFirstMatch(in: string, range: searchRange)
		}
		
		return result
	}
}
