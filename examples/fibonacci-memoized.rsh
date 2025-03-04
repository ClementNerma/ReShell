
let mut calls = 0
let mut memo = [0, 1]

fn fibo(n: int) -> int {
    $calls = $calls + 1

    if $memo.len() > $n {
        $memo[$n]
    } else if $n < 2 {
        $n
    } else {
        let result = fibo($n - 1) + fibo($n - 2)
        $memo[] = $result
        $result
    }
}

for i in 0..=90 {
    $calls = 0
    echo "fibo($i) = `fibo($i)` (function calls: $calls)"
}
