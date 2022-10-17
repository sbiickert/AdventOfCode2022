//
//  InputList.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-02.
//

import SwiftUI

struct InputList: View {
	var selectedSolution: AoCSolution
    var body: some View {
		List(AoCUtil.inputs(for: selectedSolution), id: \.id) { input in
			InputRow(input: input)
		}
    }
}

struct InputList_Previews: PreviewProvider {
    static var previews: some View {
		InputList(selectedSolution: AoCUtil.solutions[0])
    }
}
