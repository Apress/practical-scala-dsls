def bubbleSort(arr_input: Array[Int]): Array[Int] = {
  val size = arr_input.size - 1
  for (a <- 1 to size) {
    for (b <- size to a by -1) {
      if (arr_input(b) < arr_input(b - 1)) {
        val x = arr_input(b)
        arr_input(b) = arr_input(b - 1)
        arr_input(b - 1) = x
      }
    }
  }
  arr_input
}