package com.knoldus.concept_learning.app

import akka.actor.{ActorSystem, Props}
import akka.pattern._
import akka.util.Timeout
import com.knoldus.concept_learning.actors.{CandidateEliminationActor, GetVersionSpace}
import com.knoldus.concept_learning.domains.FindS.DataObject
import com.knoldus.concept_learning.domains.{TrainingData, TumorReport}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random



object ConceptLearningApp extends App {

  implicit val timeout = Timeout(5 seconds)
  val actorSystem = ActorSystem("FindS-ActorSystem")
//  val findSActor = actorSystem.actorOf(Props[FindSActor])
  val candidateEliminationActor = actorSystem.actorOf(Props[CandidateEliminationActor])
  /********************************
    * TRAINING
    ********************************/

  val trainingDataSamples: List[TrainingData] = TrainingDataGenerator.generateTrainingData

  println("**************************Start Training************************************\n\n")
  //Training of actor is being start
  trainingDataSamples foreach { trainingData =>
//    println(s"Training data: $trainingData")
//    findSActor ! trainingData
    candidateEliminationActor ! trainingData
  }

  Thread.sleep(3000)

  /***************************************
    * HYPOTHESIS
    **************************************/
  /*(findSActor ? GetHypothesis) map {
    case res: Option[Concept] =>
      println("Final hypothesis.........>>>>>>>>>>>>>>>", res)
  }*/
  (candidateEliminationActor ? GetVersionSpace) map {
    case (specificSet, generalSet)=>
      println(s"***************************************** Specific set $specificSet\n\n")
      println(s"***************************************** General set $generalSet\n\n")
  }

  /*****************************************************
   * TESTING
   ******************************************************/

  Thread.sleep(2000)
  println("*****************************Training finished****************************\n\n")
  println("*****************************Testing****************************\n\n")

  /*(findSActor ? new DataObject("circular", "large", "dark", "smooth")) map {
    case Some(positive: Boolean) => if (positive) {
      println("Positive.......!!\n\n")
    } else {
      println("Negative.......!!\n\n")
    }
    case msg: String =>
      println("ERROR: " + msg)
  }*/

  (candidateEliminationActor ? new DataObject("circular", "large", "light", "smooth")) map {
    case positive: Boolean => if (positive) {
      println("Positive.......!!\n\n")
    } else {
      println("Negative.......!!\n\n")
    }
    case msg: String =>
      println("ERROR: " + msg)
  }

  actorSystem.terminate()
}


object TrainingDataGenerator {

  val shape = List("oval", "circular")
  val size = List("large", "small")
  val color = List("dark", "light")
  val surface = List("smooth", "irregular")

  def generateTrainingData: List[TrainingData] = {
   /* List.range(0, 40).map { res =>
       TrainingData(
        TumorReport(shape(random), size(random), color(random), surface(random)),
        if (random == 1) {
          true
        } else {
          false
        }
      )
    }*/
    List(TrainingData(TumorReport("circular", "large", "light", "smooth"), true),
      TrainingData(TumorReport("circular", "large", "light", "irregular"), true),
      TrainingData(TumorReport("oval", "small", "light", "smooth"), false),
      TrainingData(TumorReport("oval", "large", "light", "irregular"), true))
  }

  private def random = Random.nextInt(2)
}
