package example

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

object AsynchronousProgramm extends App {

   case class Coffee()
   case class GroundCoffee()

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



  trait Future[+T] extends ((Try[T] => Unit) => Unit) {
//    def apply(k: Try[T] => Unit): Unit
    def onComplete(k: Try[T] => Unit)(implicit ec: ExecutionContext): Unit
    // success
    def map[B](f: T => B): Future[B]
    def flatMap[B](f: T => Future[B]): Future[B]
    def zip[B](fb: Future[B]): Future[(T, B)]
    // failure
    def recover(f: Exception => T): Future[T]
    def recoverWith(f: Exception => Future[T]): Future[T]
  }

  def gridBeans(): Future[GroundCoffee]
  def brew(groundCoffee: GroundCoffee): Future[Coffee]

  def makeCoffee(): Future[Coffee] = gridBeans().flatMap(groundCoffee => brew(groundCoffee))

  def makeCoffee(coffeeDone: Coffee => Unit, onFailure: Exception => Unit): Unit

  def drink(coffee: AsynchronousProgramm.Coffee): Unit = coffee(Coffee())

  def coffeeBreak(): Unit = {
    val eventuallyCoffee = makeCoffee()
    eventuallyCoffee.onComplete{
      case Success(coffee) => drink(coffee)
      case Failure(reason) => ()
    }
    println("chat")
  }

}
