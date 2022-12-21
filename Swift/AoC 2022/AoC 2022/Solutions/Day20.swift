//
//  Day20.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-20.
//

import Foundation

class Day20: AoCSolution {
	override init() {
		super.init()
		day = 20
		name = "Grove Positioning System"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		var dll = DoubleLinkedList(input)
		let p1 = solvePart(linkedList: dll, repeat: 1)
		print("Part One: the sum of the coord values is \(p1).")
		
		dll = DoubleLinkedList(input)
		for node in dll.nodes {
			node.value *= 811589153;
		}
		let p2 = solvePart(linkedList: dll, repeat: 10)
		print("Part Two: the sum of the coord values is \(p2).")
		
		return AoCResult(part1: String(p1), part2: String(p2))
	}
	
	private func solvePart(linkedList dll: DoubleLinkedList, repeat count: Int) -> Int {
		for _ in (1...count) {
			mix(dll)
		}
		
		var coord = [Int]()
		var ptr = dll.zero
		
		for i in 1...3000 {
			ptr = ptr!.next
			if (i % 1000 == 0) { coord.append(ptr!.value)}
		}
		print(coord.map({ String($0) }).joined(separator: ", "))
		let sum = coord.reduce(0, +)
		return sum
	}
	
	private func mix(_ dll: DoubleLinkedList) {
		for i in 0..<dll.nodes.count {
			let mover = dll.nodes[i]
			let diff = abs(mover.value) % (dll.nodes.count - 1)
			dll.move(node: mover, by: diff)
		}
	}
}

class DoubleLinkedList {
	var nodes = [DLLNode]()
	var zero: DLLNode?
	
	init(_ input: [String]) {
		for line in input {
			nodes.append(DLLNode(value: Int(line)!))
		}
		for i in (0..<nodes.count) {
			let prev = i > 0 ? i - 1 : nodes.count - 1
			let next = i < nodes.count-1 ? i + 1 : 0
			nodes[i].prev = nodes[prev]
			nodes[i].next = nodes[next]
			if nodes[i].value == 0 {
				zero = nodes[i]
			}
		}
	}
	
	func move(node mover: DLLNode, by diff: Int) {
		if (diff > 0) {
			// Remove links to mover
			mover.prev!.next = mover.next
			mover.next!.prev = mover.prev

			var dest_prev = mover
			if (mover.value > 0) {
				for _ in 1...diff {
					dest_prev = dest_prev.next!
				}
			}
			else if (mover.value < 0) {
				for _ in 1...diff+1 {
					dest_prev = dest_prev.prev!
				}
			}
			let dest_next = dest_prev.next!

			// Insert mover
			dest_prev.next = mover
			dest_next.prev = mover
			mover.prev = dest_prev
			mover.next = dest_next
		}
	}
	
	func toString() -> String {
		var ptr = zero
		var values = [String]()
		for _ in 0..<nodes.count {
			values.append(String(ptr!.value))
			ptr = ptr!.next
		}
		return values.joined(separator: ", ")
	}
}

class DLLNode {
	var value: Int
	var prev: DLLNode?
	var next: DLLNode?
	
	init(value: Int) {
		self.value = value
	}
}
