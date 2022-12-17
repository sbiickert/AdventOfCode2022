//
//  Day16.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-16.
//

import Foundation

class Day16: AoCSolution {
	override init() {
		super.init()
		day = 16
		name = "Proboscidea Volcanium"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let g = buildGraph(from: input)
		
		let p1 = solvePartOne(graph: g)
		
		return AoCResult(part1: String(p1), part2: nil)
	}
	
	// https://www.reddit.com/r/adventofcode/comments/znn9xb/2022_day_16_part_1typescript_can_someone_explain/
	private func solvePartOne(graph: Graph) -> Int {
		
		return 0;
	}
	
	private func buildGraph(from input: [String]) -> Graph {
		var nodes = Dictionary<String, GraphNode>()
		let re = /Valve (\w+) .+ rate=(\d+); .+ valves? (.+)/;
		for line in input {
			if let match = line.firstMatch(of: re) {
				let label: String = String(match.output.1)
				let rate: Int = Int(String(match.output.2))!
				let linkList = match.output.3.split(separator: ", ").map { String($0) }
				let node = GraphNode(label: label, rate: rate)
				node.links = linkList
				nodes[label] = node
			}
		}
		let g = Graph(start: nodes["AA"]!)
		g.nodes = nodes
		return g
	}
}

class Graph {
	let start: GraphNode
	var nodes = Dictionary<String, GraphNode>()
	var cache = Dictionary<GraphCacheKey, Int>()
	
	init(start s: GraphNode) {
		start = s
	}
}

class GraphNode {
	let label: String
	let rate: Int
	var open: Bool = false
	var links = [String]()
	
	init(label l: String, rate r: Int) {
		label = l
		rate = r
	}
}

struct GraphCacheKey: Hashable {
	let positionLabel: String
	let openValves: Dictionary<String, Bool>
	let releaseRate: Int
}
