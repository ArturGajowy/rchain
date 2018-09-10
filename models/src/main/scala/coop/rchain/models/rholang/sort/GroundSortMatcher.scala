package coop.rchain.models.rholang.sort

import cats.effect.Sync
import coop.rchain.models.Expr.ExprInstance
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models._
import cats.implicits._

private[sort] object GroundSortMatcher extends Sortable[ExprInstance] {
  def sortMatch[F[_]: Sync](g: ExprInstance): F[ScoredTerm[ExprInstance]] =
    g match {
      case gb: GBool =>
        Sortable.sortMatch(gb).map { sorted =>
          ScoredTerm(g, sorted.score)
        }
      case gi: GInt    => ScoredTerm(g, Leaves(Score.INT, gi.value)).pure[F]
      case gs: GString => ScoredTerm(g, Node(Score.STRING, Leaf(gs.value))).pure[F]
      case gu: GUri    => ScoredTerm(g, Node(Score.URI, Leaf(gu.value))).pure[F]
      case EListBody(gl) =>
        for {
          sortedPars <- gl.ps.toList.map(par => Sortable.sortMatch(par)).sequence
        } yield
          ScoredTerm(EListBody(gl.withPs(sortedPars.map(_.term.get))),
                     Node(Score.ELIST, sortedPars.map(_.score): _*))
      case ETupleBody(gt) =>
        for {
          sortedPars <- gt.ps.toList
                         .map(par => Sortable.sortMatch(par))
                         .sequence
        } yield
          ScoredTerm(ETupleBody(gt.withPs(sortedPars.map(_.term.get))),
                     Node(Score.ETUPLE, sortedPars.map(_.score): _*))
      // Note ESet and EMap rely on the stableness of Scala's sort
      // See https://github.com/scala/scala/blob/2.11.x/src/library/scala/collection/SeqLike.scala#L627
      case ESetBody(gs) =>
        for {
          pars <- gs.ps.sortedPars
                   .map(par => Sortable.sortMatch(par))
                   .sequence
          sortedPars        = pars.sorted
          remainderScoreOpt = gs.remainder.map(_ => Leaf(Score.REMAINDER))
        } yield
          ScoredTerm(
            ESetBody(
              ParSet(SortedParHashSet(sortedPars.map(_.term.get)),
                     gs.connectiveUsed,
                     gs.locallyFree,
                     gs.remainder)),
            Node(Leaf(Score.ESET) :: sortedPars.map(_.score) ::: remainderScoreOpt.toList)
          )
      case EMapBody(gm) =>
        def sortKeyValuePair(key: Par, value: Par): F[ScoredTerm[(Par, Par)]] =
          for {
            sortedKey   <- Sortable.sortMatch(key)
            sortedValue <- Sortable.sortMatch(value)
          } yield ScoredTerm((sortedKey.term, sortedValue.term), sortedKey.score)

        for {
          pars       <- gm.ps.sortedMap.map(kv => sortKeyValuePair(kv._1, kv._2)).sequence
          sortedPars = pars.sorted
        } yield
          ScoredTerm(EMapBody(ParMap(sortedPars.map(_.term), gm.connectiveUsed, gm.locallyFree)),
                     Node(Score.EMAP, sortedPars.map(_.score): _*))
      case GByteArray(ba) =>
        ScoredTerm(g, Node(Score.EBYTEARR, Leaf(ba.toStringUtf8))).pure[F]
      case _ => //TODO(mateusz.gorski): rethink it
        Sync[F].raiseError(
          new IllegalArgumentException("GwroundSortMatcher passed unknown Expr instance"))
    }
}
