package algo

object Tua extends App {
  var count = 0
  ((1 until 100000 by 4).toList) foreach { x =>
    count = count + 1;
    print(x);
    print(" ");
    if ((count % 5) == 0) println
  }
}
