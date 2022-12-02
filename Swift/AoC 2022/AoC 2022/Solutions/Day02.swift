//
//  Day02.swift
//  AoC 2022
//
//  Created by Simon Biickert on 2022-12-02.
//

import Foundation

class Day02: AoCSolution {
	override init() {
		super.init()
		day = 2
		name = "Rock Paper Scissors"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let turns = parseTurns(input)
		
		var partOneSum = 0
		var partTwoSum = 0
		
		for turn in turns {
			partOneSum += turn.partOneScore
			partTwoSum += turn.partTwoScore
		}
		
		return AoCResult(part1: String(partOneSum), part2: String(partTwoSum))
	}
	
	private func parseTurns(_ input: [String]) -> [RPSTurn] {
		var turns = [RPSTurn]()
		for line in input {
			turns.append(RPSTurn(line))
		}
		return turns
	}
}

struct RPSTurn {
	let opponentMove: RPSMove
	let myMovePartOne: RPSMove
	let partTwoStrategy: RPSResult
	let myMovePartTwo: RPSMove
	
	init(_ line: String) {
		let letters = line.split(separator: " ")
		opponentMove = RPSMove.fromString(value: String(letters[0]))
		myMovePartOne = RPSMove.fromString(value: String(letters[1]))
		partTwoStrategy = RPSResult.fromString(value: String(letters[1]))
		myMovePartTwo = opponentMove.moveToAchieve(partTwoStrategy)
	}
	
	var partOneScore: Int {
		let result = myMovePartOne.result(otherMove: opponentMove)
		return myMovePartOne.score + result.score
	}
	
	var partTwoScore: Int {
		let result = myMovePartTwo.result(otherMove: opponentMove)
		return myMovePartTwo.score + result.score
	}
}

enum RPSMove {
	case rock
	case paper
	case scissors
	
	static func fromString(value: String) -> RPSMove {
		switch (value) {
		case "A","X":
			return .rock
		case "B","Y":
			return .paper
		default:
			return .scissors
		}
	}
	
	var score: Int {
		switch (self) {
		case .rock:
			return 1
		case .paper:
			return 2
		case .scissors:
			return 3
		}
	}
	
	var winningCounterMove: RPSMove {
		switch (self) {
		case .rock:
			return .paper
		case .paper:
			return .scissors
		case .scissors:
			return .rock
		}
	}
	
	var losingCounterMove: RPSMove {
		switch (self) {
		case .rock:
			return .scissors
		case .paper:
			return .rock
		case .scissors:
			return .paper
		}
	}
	
	func result(otherMove: RPSMove) -> RPSResult {
		if (otherMove == self) {
			return .draw
		}
		else if (otherMove == winningCounterMove) {
			return .lose
		}
		else { //if (otherMove == losingCounterMove)
			return .win
		}
	}
	
	func moveToAchieve(_ result: RPSResult) -> RPSMove {
		switch (result) {
		case .win:
			return winningCounterMove
		case .lose:
			return losingCounterMove
		case .draw:
			return self
		}
	}
}

enum RPSResult {
	case win
	case draw
	case lose
	
	static func fromString(value: String) -> RPSResult {
		switch (value) {
		case "X":
			return .lose
		case "Y":
			return .draw
		default:
			return .win
		}
	}

	var score: Int {
		switch (self) {
		case .win:
			return 6
		case .lose:
			return 0
		case .draw:
			return 3
		}
	}
}
