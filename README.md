# AdventOfCode2022
 Code solutions for 2022 Advent of Code

## Opening Remarks

It's that time of year again. I can tell you how it will go:
1. First few days: puzzles are done too quickly. I will start doing them in multiple languages just for fun.
2. Middle days: taking me long enough to do the puzzles that I limit myself to only my primary solving language.
3. End days: will have go to the [solutions megathread](https://www.reddit.com/r/adventofcode/wiki/solution_megathreads/) on Reddit at least twice, and some puzzles will take more than a day to complete. Life will suck.
4. Christmas: drinking eggnog while summoning the courage to revisit the puzzles I couldn't solve.

But seriously. In 2020 and 2021 I solved in Swift, with 2021's focus being on using Swift Algorithms wherever possible. This year, I'm switching to Perl. In the off-season, I re-solved all of the 2021 puzzles in Perl and was surprised at how much fun I had doing it. Perl is the first language that I mastered (back in the mid-90's) and you never forget your first love.

If I run into problems, especially with complex data structures and/or the lack of debugging tools, I may revert to Swift. I expect that in the early "multiple languages" phase, I will be playing with Swift, Pascal and maybe even PHP.

## One Week Done

So far, AoC 2022 has been a gentler ramp than I was expecting. Day 6 (Tuning Trouble) was finished in 25 minutes, starting at 10 pm. I thought I would just get a head start, but I started and finished. Todays challenge (No Space Left on Device) looked hard, but I surprised myself by being able to build the file system tree in a small amount of time. Once that was done, getting the answers was easy.

Perl is an excellent language for this kind of work, at least until the data structures start getting complicated. Even so, I am completely "warmed up" and array refs and hash refs don't scare me.

## Difficulty Spike

We've hit the spike, as of Day 16. I barely finished part 1 with code that makes me ill to read it. I will return to this one and approach it again in Swift. Day 17 (Tetris) was easier and a fair bit of fun but it was still in the upper echelons of difficulty. Thankfully Day 18 was a bit of a reprieve, but it still was a 3D challenge.

## And There's the Big Brain Problem

This year, Day 22 started easy, but then the problem wrapped itself into a cube. Trying to understand the relationships among the faces on the cube made my brain hurt. I was forced to do arts and crafts just to be able to visualize it.

![Paper Cubes](https://github.com/sbiickert/AdventOfCode2022/blob/main/Art/aoc_day22_cubes.jpg)

Just don't look directly at [the code](https://github.com/sbiickert/AdventOfCode2022/blob/main/Perl/day22.pl).

## In the End

Doing AoC 2022 in Perl was a success, and I don't really have to make any excuses. I solved everything, and I don't think using a different language would have changed it for better or for worse. There are four days' solutions that are slow: 15, 16, 19 and 24. The problem is the solution, not the language. Everything else solves in a total of 61 seconds on my M2 MacBook Pro. I think the only thing I would change is the naming convention for the AOC::SpatialUtil functions. They are well-named (C2D* for 2D coords, etc.), but a pain to type.
