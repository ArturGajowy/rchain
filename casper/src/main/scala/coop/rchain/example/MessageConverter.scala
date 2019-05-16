package coop.rchain.example

import cats.FlatMap
import cats.data.NonEmptyList
import coop.rchain.comm.{transport, PeerNode}
import coop.rchain.example.Validations.nonEmpty
import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.comm.protocol.routing.Packet
import coop.rchain.comm.transport.{Blob, TransportLayer}
import coop.rchain.shared.Log

//THE BADDIES:

sealed trait CasperMsg //current CasperMessage

//current ApprovedBlock
case class ApprovedBlockMsg(
    candidate: Option[ApprovedBlockCandidateMsg],
    sigs: Seq[ByteString]
) extends CasperMsg

//current ApprovedBlockCandidate
case class ApprovedBlockCandidateMsg(
    blockMsg: BlockMsg,
    requiredSigs: Int
)

//current BlockMessage
case class BlockMsg()

//THE GOODIES:

sealed trait CasperMessage //no counterpart in rchain currently, future CasperMessage

case class ApprovedBlock(
    candidate: ApprovedBlockCandidate,
    sigs: NonEmptyList[ByteString]
) extends CasperMessage

case class ApprovedBlockCandidate(
    blockMsg: BlockMessage,
    requiredSigs: Int
)

case class BlockMessage()

// THE CONVERTER

trait MessageConverter[A, B] {
  def messageId: String
  def readValid(a: A): Either[String, B]
  def write(b: B): A
}

object MessageConverter {
  def apply[A, B](implicit ev: MessageConverter[A, B]) = ev

  implicit class Ops[A](a: A) {
    def readValid[B](implicit ev: MessageConverter[A, B]): Either[String, B] = ev.readValid(a)
  }

  implicit class CoOps[A, B](b: B)(implicit ev: MessageConverter[A, B]) {
    def write: A          = ev.write(b)
    val messageId: String = ev.messageId
  }
}

//put into the same file as the case class
object ApprovedBlock {

  import MessageConverter._

  implicit val converter: MessageConverter[ApprovedBlockMsg, ApprovedBlock] =
    new MessageConverter[ApprovedBlockMsg, ApprovedBlock] {

      override def messageId: String = transport.ApprovedBlock.id

      override def readValid(a: ApprovedBlockMsg): Either[String, ApprovedBlock] =
        (
          nonEmpty(a.candidate) >>= (_.readValid[ApprovedBlockCandidate]),
          nonEmpty(a.sigs)
        ).mapN(ApprovedBlock.apply)

      override def write(b: ApprovedBlock): ApprovedBlockMsg =
        ApprovedBlockMsg(
          b.candidate.write.some,
          b.sigs.toList
        )
    }
}

object ApprovedBlockCandidate {
  implicit def converter: MessageConverter[ApprovedBlockCandidateMsg, ApprovedBlockCandidate] = ???
}

object Validations {

  def nonEmpty[A](opt: Option[A]): Either[String, A]            = ???
  def nonEmpty[A](seq: Seq[A]): Either[String, NonEmptyList[A]] = ???

}

// Handling

object Handle {

  def handle[F[_]: ProtocolTransportLayer](msg: CasperMessage): F[Unit] = {
    val irrelevant: PeerNode = null

    ProtocolTransportLayer[F].stream(
      from = irrelevant,
      to = irrelevant,
      // Look, ma! No Option-s!
      ApprovedBlock(ApprovedBlockCandidate(BlockMessage(), 42), NonEmptyList(ByteString.EMPTY, Nil))
    )
  }

}

// Receiving
object CasperPacketHandler {

  def handlePacket[F[_]: FlatMap: Log: ProtocolTransportLayer](
      peer: PeerNode,
      packet: Packet
  ): F[Unit] =
    toCasperMsg(packet)
      .flatMap(toCasperMessage)
      .fold(
        error => Log[F].warn(s"Could not extract casper message from packet sent by $peer: $error"),
        message => Handle.handle[F](message)
      )

  //current: toCasperMessage(peer, packet), peer omitted below
  def toCasperMsg(packet: Packet): Either[String, CasperMsg] = ???

  import MessageConverter._

  def toCasperMessage(msg: CasperMsg): Either[String, CasperMessage] = msg match {
    case a: ApprovedBlockMsg => a.readValid[ApprovedBlock]
  }
}

// Sending
trait ProtocolTransportLayer[F[_]] {
  import MessageConverter._

  implicit def tl: TransportLayer[F]

  def stream[A <: CasperMsg, B](from: PeerNode, to: PeerNode, message: B)(
      implicit ev: MessageConverter[A, B]
  ): F[Unit] = {
    val packet = Packet(message.messageId, serialize(message.write))
    TransportLayer[F].stream(to, Blob(from, packet))
  }

  private def serialize(casperMsg: CasperMsg): ByteString = ???
}

object ProtocolTransportLayer {
  def apply[F[_]](implicit ev: ProtocolTransportLayer[F]) = ev

  def fromTL[F[_]](tl: TransportLayer[F]): ProtocolTransportLayer[F] = ???
}
