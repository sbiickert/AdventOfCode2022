//
//  StopwatchView.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-08.
//

import SwiftUI

struct StopwatchView: View {
	@Binding var progressTime: Int // Milliseconds since start
	
	private var hours: Int { (progressTime / 3600000) % 24 }
	private var minutes: Int { (progressTime / 60000) % 60 }
	private var seconds: Int { (progressTime / 1000) % 60 }
	private var millis: Int { (progressTime % 1000) }
	
    var body: some View {
		HStack {
			StopwatchUnitView(timeUnit: hours)
			Text(":")
			StopwatchUnitView(timeUnit: minutes)
			Text(":")
			StopwatchUnitView(timeUnit: seconds)
			Text(":")
			StopwatchUnitView(timeUnit: millis, numDigits: 3)
		}
    }
}

struct StopwatchView_Previews: PreviewProvider {
    static var previews: some View {
		StopwatchView(progressTime: .constant(12))
    }
}
