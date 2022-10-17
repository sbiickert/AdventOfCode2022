//
//  ContentView.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-02.
//

import SwiftUI

struct ContentView: View {
    var body: some View {
		NavigationView {
			SolutionList()
			EmptyView()
			EmptyView()
		}
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
