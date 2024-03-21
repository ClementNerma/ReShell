
let mut calls = 0

fn fibo(n: int) -> int {
    $calls = $calls + 1

    return if $n < 2 {
        $n
    } else {
        fibo($n - 1) + fibo($n - 2)
    }
}

for i in range(1, 100) {
    $calls = 0
    echo "fibo($i) = `fibo($i)` [$calls]"
}
