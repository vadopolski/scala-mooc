package homeworks.futures


import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
//  def fullSequence[A](futures: List[Future[A]])
//                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = ???

type PartitionedFuture[A] = Future[(List[A], List[Throwable])]

def fullSequence[A](futures: List[Future[A]])
                   (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
{
  val futuresWithError = futures.map { future =>
    future
      .map(Right(_))
      .recover {
        case ex: Throwable =>
          Left(ex)
      }
  }

  Future.sequence(futuresWithError).map { list =>
    list.foldRight((List.empty[A], List.empty[Throwable])) {
      case (value, (results, errors)) =>
        value match {
          case Left(error) =>
            (results, error :: errors)
          case Right(result) =>
            (result :: results, errors)
        }
    }
  }
}




}

//
//implicit val ec = ExecutionContext.global
//
//val talk = Seq(
//Future {
//  Thread.sleep(1000)
//  "red"
//},
//  Future.failed(new RuntimeException("exception1")),
//  Future.successful("blue"),
//  Future.failed(new RuntimeException("exception2")),
//  Future.successful("green"),
//  Future.failed(new RuntimeException("exception3"))
//  )
//
//  val res = talk.foldLeft(Future(Seq[String](), Seq[Throwable]())) { (a, x) =>
//  a.flatMap {
//  case (win, fail) => x.map(p => (p +: win, fail)).recover { case f => (win, f +: fail) }
//}
//}