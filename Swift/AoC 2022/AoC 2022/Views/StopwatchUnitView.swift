//
//  StopwatchUnitView.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-08.
//

import SwiftUI

struct StopwatchUnitView: View {
	var timeUnit: Int
	var numDigits: Int = 2
	
	var timeUnitStrs: [String] {
		let digits = String(timeUnit).compactMap { $0.wholeNumberValue }
		var strings = digits.map { String($0) }
		while strings.count < numDigits { strings.insert("0", at: 0) }
		return strings
	}
	
    var body: some View {
		HStack(spacing: 2) {
			ForEach(0..<timeUnitStrs.count, id: \.self) {
				Text(timeUnitStrs[$0]).frame(width: 10)
			}
		}
    }
}

struct StopwatchUnitView_Previews: PreviewProvider {
    static var previews: some View {
		StopwatchUnitView(timeUnit: 4)
		StopwatchUnitView(timeUnit: 6, numDigits: 3)
		StopwatchUnitView(timeUnit: 123, numDigits: 3)
    }
}
