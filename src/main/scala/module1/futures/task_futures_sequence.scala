package module1.futures

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
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val listF = futures.map{
      x =>
        x.transformWith {
          case Success(res) => Future(res)
          case Failure(er) => Future(er)
        }
    }

    Future.foldLeft(listF)(List.empty[A],List.empty[Throwable]){(acc, x) =>
      if (x.isInstanceOf[Throwable]) (acc._1, acc._2 :+ x.asInstanceOf[Throwable])
      else (acc._1 :+ x.asInstanceOf[A], acc._2 )
    }
  }

}
