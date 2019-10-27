object Boot {
  def main(args: Array[String]): Unit = {
    var isGameOn = true
    while (isGameOn) {
      if (System.in.available > 0) {
        val x = System.in.read.toByte
        x match {
          case 'A' => println("UP")
          case 'B' => println("DOWN")
          case 'q' => isGameOn = false
          case _   => ()
        }
      }
    }
  }
}
