
echo "Parent: ${pid()}"

echo (howlong((|| {
    detached (|| {
        echo "Child: ${pid()}"
    })
})))

echo "Parent: ${pid()}"
