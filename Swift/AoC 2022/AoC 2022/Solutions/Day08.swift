//
//  Day08.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-08.
//

import Foundation
import Algorithms

class Day08: AoCSolution {
	override init() {
		super.init()
		day = 8
		name = "Treetop Tree House"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let trees = defineTrees(from: input)
		
		let numVisible = solvePartOne(trees)
		let maxScenic = solvePartTwo(trees)

		return AoCResult(part1: String(numVisible), part2: String(maxScenic))
	}
	
	private func solvePartOne(_ trees: [[Tree]]) -> Int {
		let size = trees.count // assuming square
		
		var visibleTrees = Set<Tree>();
		for r in (0..<size) {
			let sightlineEast = trees[r]
			visibleTrees = visibleTrees.union(getVisibleTrees(sightline: sightlineEast))
			let sightlineWest = [Tree](sightlineEast.reversed())
			visibleTrees = visibleTrees.union(getVisibleTrees(sightline: sightlineWest))
		}
		for c in (0..<size) {
			var sightlineSouth = [Tree]()
			for r in (0..<size) {
				sightlineSouth.append(trees[r][c])
			}
			visibleTrees = visibleTrees.union(getVisibleTrees(sightline: sightlineSouth))
			let sightlineNorth = [Tree](sightlineSouth.reversed())
			visibleTrees = visibleTrees.union(getVisibleTrees(sightline: sightlineNorth))
		}
		
		print("Part One: the number of visible trees is \(visibleTrees.count)")
		return visibleTrees.count
	}
	
	private func solvePartTwo(_ trees: [[Tree]]) -> Int {
		let size = trees.count // assuming square
		var maxScenicScore = 0
		
		for (r, c) in product(0..<size, 0..<size) {
			var viewDistances = [Int]()
			let loc = AoCCoord2D(x: c, y: r)
			let tree = trees[r][c]
			for direction in AoCMapDirection.allCases {
				let sightline = buildSightline(trees, viewPoint: loc, direction: direction)
				let vd = getViewDistance(startHeight: tree.height, sightline: sightline)
				viewDistances.append(vd)
			}
			let scenicScore = viewDistances.reduce(1, *)
			maxScenicScore = max(scenicScore, maxScenicScore)
		}
		
		print("Part Two: the maximum scenic score is \(maxScenicScore)")
		return maxScenicScore
	}
	
	private func getVisibleTrees(sightline: [Tree]) -> Set<Tree> {
		var result = Set<Tree>()
		var maxH = -1;
		for tree in sightline {
			if tree.height > maxH {
				result.insert(tree)
				maxH = tree.height
			}
		}
		return result
	}
	
	private func buildSightline(_ trees: [[Tree]], viewPoint vp: AoCCoord2D, direction: AoCMapDirection) -> [Tree] {
		var result = [Tree]()
		let size = trees.count // assuming square
		let offset = direction.offset
		let ext = AoCExtent2D(min: AoCCoord2D(x: 0, y: 0), max: AoCCoord2D(x: size-1, y: size-1))
		
		var loc = vp + offset
		while ext.contains(loc) {
			result.append(trees[loc.y][loc.x])
			loc = loc + offset
		}
		
		return result
	}
	
	private func getViewDistance(startHeight h: Int, sightline: [Tree]) -> Int {
		guard sightline.count > 0 else {return 0}
		var vd = 0
		
		for (i, tree) in sightline.enumerated() {
			if tree.height >= h {
				// blocked
				vd = i + 1
				break
			}
		}
		
		return (vd == 0) ? sightline.count : vd
	}

	private func defineTrees(from input: [String]) -> [[Tree]] {
		var result = [[Tree]]()
		for (y, line) in input.enumerated() {
			var row = [Tree]()
			for x in (0..<line.count) {
				let start = line.index(line.startIndex, offsetBy: x)
				let end = line.index(line.startIndex, offsetBy: x+1)
				let number = Int(String(line[start..<end]))!
				row.append(Tree(height: number, position: AoCCoord2D(x: x, y: y)))
			}
			result.append(row)
		}
		return result
	}
}

private struct Tree: Hashable {
	let height: Int
	let position: AoCCoord2D // Important for hashing into Set
}
