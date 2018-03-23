package application.usecase

import application.cont.ActionCont
import application.support.ActionCont
import domain.lifcycle.{IOContext, WarriorRepository}
import domain.model.character.warrior.{Warrior, WarriorId}
import domain.model.weapon.Weapon

import scala.concurrent.Future

trait WarriorService {

  def find(id: WarriorId): ActionCont[Warrior]

  case object WarriorNotFound extends EntityNotFound {
    val cause: String = "該当する戦士が存在しません"
  }

  def equippedNewWeapon(warrior: Warrior, newWeapon: Weapon): ActionCont[UseCaseResult]

  case object InvalidCondition extends AbnormalCase {
    val cause: String = "この武器を装備するための条件を満たしていません"
  }

}

final class WarriorServiceImpl[Ctx <: IOContext](
  ctx: Ctx,
  repository: WarriorRepository[Future]
) extends WarriorService {

  def find(id: WarriorId): ActionCont[Warrior] =
    ActionCont { f =>
      repository.resolveBy(id).flatMap {
        case Some(w) => f(w)
        case None => Future.successful(WarriorNotFound)
      }(ctx.ec)
    }

  def equippedNewWeapon(warrior: Warrior, newWeapon: Weapon): ActionCont[UseCaseResult] =
    ActionCont { f =>
      warrior.setNewWeapon(newWeapon) match {
        case Right(w) => repository.store(w).flatMap(_ => f(NormalCase))(ctx.ec)
        case Left(_) => Future.successful(InvalidCondition)
      }
    }

}
