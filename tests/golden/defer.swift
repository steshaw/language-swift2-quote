func f() {
    defer { print("First") }
    defer { print("Second") }
    defer { print("Third") }
}
f()
