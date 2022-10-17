//
//  SolutionRow.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-02.
//

import SwiftUI

struct SolutionRow: View {
	var solution: AoCSolution
	var body: some View {
		HStack {
			Text(String(solution.day))
			Text(solution.name)
			Spacer()
		}
    }
}

struct SolutionRow_Previews: PreviewProvider {
    static var previews: some View {
		Group {
			SolutionRow(solution: AoCUtil.solutions[0])
			SolutionRow(solution: AoCUtil.solutions[1])
		}
		.previewLayout(.fixed(width: 300, height: 70))
    }
}
