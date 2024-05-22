package module4.homework.services

import module4.homework.dao.entity.{Role, RoleCode, User, UserId}
import module4.homework.dao.repository.UserRepository
import module4.phoneBook.db
import zio.{Has, RIO, ZIO, ZLayer}
import zio.macros.accessible

@accessible
object UserService {
  type UserService = Has[Service]

  trait Service {
    def listUsers(): RIO[db.DataSource, List[User]]

    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]

    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]

    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
  }

  class Impl(userRepo: UserRepository.Service) extends Service {
    val dc = db.Ctx

    def listUsers(): RIO[db.DataSource, List[User]] =
      userRepo.list()


    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]] = for {
      users <- userRepo.list()
      usersWithRoles <- ZIO.collectAll(users.map(u => userRepo.userRoles(UserId(u.id)).map(l => u -> l)))
      res <- ZIO.succeed(usersWithRoles.map(u => UserDTO(u._1, u._2.toSet)))
    } yield res

    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = for {
      u <- userRepo.createUser(user)
      _ <- userRepo.insertRoleToUser(roleCode, user.typedId)
      roles <- userRepo.userRoles(UserId(u.id))
      res <- ZIO.succeed(UserDTO(u, roles.toSet))
    } yield res

    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]] = for {
      u <- userRepo.listUsersWithRole(roleCode)
      usersWithRoles <- ZIO.collectAll(u.map(u => userRepo.userRoles(UserId(u.id)).map(l => u -> l)))
      res <- ZIO.succeed(usersWithRoles.map(u => UserDTO(u._1, u._2.toSet)))
    } yield res


  }

  val live: ZLayer[UserRepository.UserRepository, Nothing, UserService] = ZLayer.fromService[UserRepository.Service, UserService.Service](dao => new UserService.Impl(dao))
}

case class UserDTO(user: User, roles: Set[Role])