
fn fibo(n: int) -> int {
    return if $n < 2 {
        $n
    } else {
        fibo($n - 1) + fibo($n - 2)
    }
}

for i in range(1, 10) {
    echo "fibo($i) = ${ fibo($i) }"
}
