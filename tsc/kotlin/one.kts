class Rectangle(val width: Int, val height: Int) {
    fun area(): Int { return width * height }
}

fun main() {
    val rect = Rectangle(5, 10)
    println("The area of the rectangle is ${rect.area()}")
}
