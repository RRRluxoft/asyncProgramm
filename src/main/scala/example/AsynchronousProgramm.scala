package example

object AsynchronousProgramm extends App {

   case class Coffee()

  def makeCoffee(coffee: Coffee => Unit): Unit = coffee(Coffee())


  def makeTwoCoffees(coffeesDone: (Coffee, Coffee) => Unit): Unit = {
    var firstCoffee: Option[Coffee] = None

    val k = {coffee: Coffee =>
      firstCoffee match {
        case None => firstCoffee = Some(coffee)
        case Some(coffee2) => coffeesDone(coffee, coffee2)
      }
    }
    makeCoffee(k)
    makeCoffee(k)
  }

  val cup1 = Coffee
  val cup2 = Coffee

  makeTwoCoffees((cup1, cup2) => println(s"${cup1}, ${cup2}"))
}
