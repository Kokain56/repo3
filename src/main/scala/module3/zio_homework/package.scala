
package module3

import module3.zioConcurrency.{currentTime, printEffectRunningTime}
import module3.zio_homework.PrintEffectRunningTimeService
import zio.clock.Clock
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random.Random
import zio.{Has, Task, ULayer, ZIO, ZLayer}

import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram = for{
    console <- ZIO.environment[Console].map(_.get)
    _ <- console.putStrLn("угадайте число от 1 до 3")
    num <- zio.random.nextIntBetween(0, 4)
    pr <- console.getStrLn
    _ <- if(num == pr.toInt) console.putStrLn("Вы угадали") else console.putStrLn("Вы не угадали")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile[A](f: A => Boolean, a: A): Task[Unit] = if(f(a)) ZIO.succeed() else doWhile(f, a)

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault = for{
    config <- config.load.orElse(ZIO.effect(println("default config")) >>> ZIO.effect(config.AppConfig("", "")))
  }yield (config)


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = ZIO.sleep(1 second) *> zio.random.nextIntBetween(0, 10)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.fill(10)(eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = printEffectRunningTime(for {
    list <- ZIO.collectAll(effects)
    sum <- ZIO.succeed(list.foldLeft(0)(_ + _))
    _ <- ZIO.effect(println(sum))
  }yield (sum))


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = printEffectRunningTime(for {
    list <- ZIO.collectAllPar(effects)
    sum <- ZIO.succeed(list.foldLeft(0)(_ + _))
    _ <- ZIO.accessM[Console](_.get.putStrLn(sum.toString))
  }yield (sum))


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.console.putStrLn например
   */

  type PrintEffectRunningTime = Has[PrintEffectRunningTimeService.Service]

  @accessible
  object PrintEffectRunningTimeService{
    trait Service{
      def print[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A]
    }

    class ServiceImpl extends Service{

      override def print[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A] = for{
        start <- currentTime
        r <- zio
        end <- currentTime
        _ <- ZIO.effect(println(s"Running time ${end - start}")).orDie
      } yield r
    }

    val live: ZLayer[Clock, Nothing, PrintEffectRunningTime] = ZLayer.fromService[Clock.Service, PrintEffectRunningTimeService.Service](_ => new ServiceImpl)
  }


  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   *
   *
   */

  lazy val appWithTimeLogg = for {
    print <- ZIO.environment[PrintEffectRunningTime].map(_.get)
    res <- ZIO.effect(print.print(app))
  }yield(res)

  /**
   *
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  val env: ZLayer[Clock, Nothing, PrintEffectRunningTime] = PrintEffectRunningTimeService.live
  lazy val runApp = appWithTimeLogg.provideSomeLayer[Clock with Random](env)

}
