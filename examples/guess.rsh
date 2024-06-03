
let num = randInt(1, 100)
let mut lives = 10

while true {
    let guess = ask("Guess the number: ")

    let guess = try { $guess.parseInt() } catch _ { null }

    if $guess == null {
        continue
    }

    if $guess > $num {
        echo "Lower!"
    } else if $guess < $num {
        echo "Higher!"
    } else {
        echo "You win! :D"
        break
    }

    $lives = $lives - 1

    if $lives > 0 {
        echo "Too bad... You have $lives attempt(s) left!"
    } else {
        echo "Too bad... You've lost :("
        break
    }
}
