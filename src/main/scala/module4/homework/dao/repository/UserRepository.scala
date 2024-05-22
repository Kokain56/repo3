package module4.homework.dao.repository

import io.getquill.context.ZioJdbc._
import module4.homework.dao.entity._
import module4.phoneBook.db
import zio.{Has, ULayer, ZLayer}


object UserRepository {


  val dc = db.Ctx

  import dc._

  type UserRepository = Has[Service]

  trait Service {
    def findUser(userId: UserId): QIO[Option[User]]

    def createUser(user: User): QIO[User]

    def createUsers(users: List[User]): QIO[List[User]]

    def updateUser(user: User): QIO[Unit]

    def deleteUser(user: User): QIO[Unit]

    def findByLastName(lastName: String): QIO[List[User]]

    def list(): QIO[List[User]]

    def userRoles(userId: UserId): QIO[List[Role]]

    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]

    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]

    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
  }

  class ServiceImpl extends Service {
    lazy val userSchema = quote {
      querySchema[User](""""User"""")
    }

    lazy val userToRoleSchema = quote {
      querySchema[UserToRole]("\"User_to_Role\"")
    }

    lazy val roleSchema = quote {
      querySchema[Role]("\"Role\"")
    }

    override def findUser(userId: UserId): QIO[Option[User]] = run(userSchema.filter(_.id == lift(userId.id)))
      .map(_.headOption)

    override def createUser(user: User): QIO[User] = run(userSchema.insert(lift(user))).as(user)

    override def createUsers(users: List[User]): QIO[List[User]] = run(
      quote {
        liftQuery(users).foreach(u => userSchema.insert(u).returning(u => u))
      })

    override def updateUser(user: User): QIO[Unit] = run(userSchema.filter(_.id == lift(user.id)).update(lift(user))).unit

    override def deleteUser(user: User): QIO[Unit] = run(userSchema.filter(_.id == lift(user.id)).delete).unit

    override def findByLastName(lastName: String): QIO[List[User]] = run(userSchema.filter(_.lastName == lift(lastName)))

    override def list(): QIO[List[User]] = run(userSchema)

    override def userRoles(userId: UserId): QIO[List[Role]] = run(for {
      user <- userSchema.filter(_.id == lift(userId.id))
      userToRole <- userToRoleSchema.join(_.userId == user.id)
      role <- roleSchema.join(_.code == userToRole.roleId)
    } yield role)

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = run(userToRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id)))).unit

    override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = run(for {
      role <- roleSchema.filter(_.code == lift(roleCode.code))
      userToRole <- userToRoleSchema.join(_.roleId == role.code)
      user <- userSchema.join(_.id == userToRole.userId)
    } yield user)

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] = run(roleSchema.filter(_.code == lift(roleCode.code))).map(_.headOption)
  }

  val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl)
}