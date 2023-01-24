package algo

object DecimalToBinary extends App {
  def toBinary(n:Int): String = {
    n match {
      case 0 => "0"
      case 1 => "1"
      case n if n %2 == 0 => "0" + toBinary(n/2)
      case n if n %2 == 1 => "1" + toBinary(n/2)
    }
  }

  println(toBinary(20).reverse)
}
