//
//  Day07.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-07.
//

import Foundation

class Day07: AoCSolution {
	let LIMIT = 100000
	let TOTAL_SPACE = 70000000
	let REQ_SPACE = 30000000
	
	override init() {
		super.init()
		day = 7
		name = "No Space Left On Device"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let root = buildFileSystem(from: input)
		root.calcSize()
		var folders = root.allSubFolders
		folders.insert(root, at: 0)
		
		var sum = 0
		for folder in folders {
			if folder.size <= LIMIT {
				sum += folder.size
			}
		}
		
		print("Part One: the sum of folder sizes that are under the limit is \(sum)")
		
		let smallest = solvePartTwo(folders)
		
		print("Part Two: the smallest folder to delete is \(smallest)")

		return AoCResult(part1: String(sum), part2: String(smallest))
	}
	
	private func solvePartTwo(_ folders: [Folder]) -> Int {
		let availableSpace = TOTAL_SPACE - folders[0].size
		let minToClear = REQ_SPACE - availableSpace
		var smallest = folders[0].size
		
		for folder in folders {
			if folder.size < smallest && folder.size >= minToClear {
				smallest = folder.size
			}
		}
		return smallest
	}
	
	private func buildFileSystem(from input: [String]) -> Folder {
		let reCommand = /^\$ ?(.*)/;
		let reCD = /cd (.+)/;
		let reFile = /^(\d+) (.+)/;
		let reFolder = /dir (.+)/;
		let root = Folder(name: "/", parent: nil)
		var ptr: Folder = root
		
		for line in input {
			if let match = line.firstMatch(of: reCommand) {
				let cmd = String(match.output.1)
				if let cmdMatch = cmd.firstMatch(of: reCD) {
					let destination = String(cmdMatch.output.1)
					if destination == "/" 	{ ptr = root }
					else					{ ptr = ptr.subFolder(withName: destination)! }
				}
			}
			else if let match = line.firstMatch(of: reFile) {
				let name = String(match.output.2)
				let size = Int(String(match.output.1))!
				let file = FSItem(name: name, parent: ptr)
				file.size = size
				ptr.items.append(file)
			}
			else if let match = line.firstMatch(of: reFolder) {
				let name = String(match.output.1)
				let folder = Folder(name: name, parent: ptr)
				ptr.items.append(folder)
			}
		}
		return root
	}
}

private class FSItem {
	var size: Int = 0
	let name: String
	let parent: Folder?
	init(name: String, parent: Folder?) {
		self.name = name
		self.parent = parent
	}
}

private class Folder: FSItem {
	var items: [FSItem]
	override init(name: String, parent: Folder?) {
		items = [FSItem]()
		super.init(name: name, parent: parent)
	}
	
	func calcSize() {
		size = 0
		for item in items {
			if let folder = item as? Folder {
				folder.calcSize()
			}
			size += item.size
		}
	}
	
	func subFolder(withName name: String) -> Folder? {
		if name == ".." {
			return parent
		}
		for item in items {
			if let folder = item as? Folder,
			   folder.name == name
			{
				return folder
			}
		}
		return nil
	}
	
	var allSubFolders: [Folder] {
		var result = [Folder]()
		
		for item in items {
			if let folder = item as? Folder {
				result.append(folder)
				result.append(contentsOf: folder.allSubFolders)
			}
		}

		return result
	}
}
