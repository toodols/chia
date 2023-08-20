// Was attempting to make a transformer that could rewrite all returns in a bunch of if-else statements.
// However blocks and loops make this too challenging but here's some shitty pseudo code magic.
// branches = [root]
// current is branches.last()
// append normal statements to current:
// when return is encountered
// 		pop all branches --> exit
// when if is encountered
// 		push a new branch for the if block
// 		append normal statements to current
// 		when return is encountered:
// 			pop all branches, leave an else branch
// 			do shit for else branch
// 		pop branch
// exit
