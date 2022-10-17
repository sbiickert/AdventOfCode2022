//
//  InputRow.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-02.
//

import SwiftUI

struct InputRow: View {
	var input: AoCInput
    var body: some View {
		HStack {
			NavigationLink {
				ResultView(input: input)
			} label: {
				Text(String(input.fileName))
				Text("[\(input.index)]")
				//Spacer()
			}
		}
    }
}

struct InputRow_Previews: PreviewProvider {
    static var previews: some View {
		Group {
			InputRow(input: AoCUtil.inputs(for: AoCUtil.solutions[2])[0])
			InputRow(input: AoCUtil.inputs(for: AoCUtil.solutions[2])[1])
		}
		.previewLayout(.fixed(width: 300, height: 70))

    }
}
