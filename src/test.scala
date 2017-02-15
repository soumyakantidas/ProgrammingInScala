/**
  * Created by Soumyakanti on 15-02-2017.
  */
object test {
  def main(args: Array[String]): Unit = {
    require(args.length == 1, "One argument must be passed!!")
    println(args(0))
  }

}
