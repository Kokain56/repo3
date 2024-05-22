package catsconcurrency.cats_effect_homework

import cats.Monad
import cats.effect.{IO, IOApp, Spawn}
import cats.implicits._

import scala.concurrent.duration.DurationInt

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      // todo: запустить все файберы и ждать ввода от пользователя чтобы завершить работу
      fiber1 <- Spawn[IO].start(loop(topup(wallet1, 100)))
      fiber2 <- Spawn[IO].start(loop(topup(wallet2, 500)))
      fiber3 <- Spawn[IO].start(loop(topup(wallet3, 2000)))
      show <- Spawn[IO].start(loop(showBalancesWallets(wallet1, wallet2, wallet3)))
      _ <- IO.readLine
      _ <- fiber1.cancel *> fiber2.cancel *> fiber3.cancel *> show.cancel
    } yield ()

  def loop(v: => IO[Unit]): IO[Unit] =
    Monad[IO].whileM_(IO(true)) { v }

  def topup(wallet: Wallet[IO], pause: Int) = for {
    _ <-wallet.topup(100)
    res <-IO.sleep(pause.milliseconds)
  } yield res

  def showBalancesWallets(wallet1: Wallet[IO], wallet2: Wallet[IO], wallet3: Wallet[IO]) = for {
    _ <- wallet1.balance.flatMap(x => IO.println(s"wallet 1: $x"))
    _ <- wallet2.balance.flatMap(x => IO.println(s"wallet 2: $x"))
    _ <- wallet3.balance.flatMap(x => IO.println(s"wallet 3: $x"))
    res <- IO.sleep(1.second)
  } yield res

}
