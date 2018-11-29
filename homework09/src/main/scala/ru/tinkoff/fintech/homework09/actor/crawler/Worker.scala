package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}
import ru.tinkoff.fintech.homework09.crawler.Url

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.{Failure, Success}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {

  implicit val ec: ExecutionContextExecutor = ExecutionContext.global

  override def receive: Receive = {
    case url: Url => http.get(url).onComplete{
      case Success(body) => parser.links(body)
      case Failure(exception) => println("doesn't work")}
    case _ => println("doesn't work!!!!")
  }
}
