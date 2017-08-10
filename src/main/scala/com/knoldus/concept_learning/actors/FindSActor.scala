package com.knoldus.concept_learning.actors

import akka.actor.Actor
import com.knoldus.concept_learning.domains.FindS.{Concept, DataObject}
import com.knoldus.concept_learning.domains.TrainingData

import scala.util.Try


case object GetHypothesis

class FindSActor extends Actor {

  var conceptOpt: Option[Concept] = None

  def receive = {

    case trainingData: TrainingData =>
      println("Prcessing training data......>>>>>>>", trainingData)
      import trainingData._
      if (result) {
        println(s"Result : $result, leaning......")
        learnFrom(trainingData)
      } else {
        println(s"Ignoring the sample: ${sample}")
      }

    case GetHypothesis => sender ! conceptOpt

    case dataObject: DataObject => sender ! Try(predict(dataObject)).getOrElse("Incorrect training data")

    case msg => println(s"Did not understand the message: ${msg}")

  }

  private def learnFrom(sampleData: TrainingData): Unit = {
    import sampleData._
    conceptOpt match {
      case Some(_) => findConjuctiveConcept(sample)
      case None => conceptOpt = Some(sample)
    }
  }

  private def findConjuctiveConcept(newH: Concept): Option[Concept] = {
    conceptOpt.flatMap { concept =>
      conceptOpt = Some(
        new Concept(
          if (newH.shape == concept.shape) {
            concept.shape
          } else {
            "?"
          },
          if (newH.size == concept.size) {
            newH.size
          } else {
            "?"
          },
          if (newH.color == concept.color) {
            newH.color
          } else {
            "?"
          },
          if (newH.surface == concept.surface) {
            newH.surface
          } else {
            "?"
          }
        )
      )
      conceptOpt
    }
  }

  private def predict(dataObject: DataObject): Option[Boolean] = {
    println(s"Test Data***********************$dataObject****************")
    conceptOpt map { concept =>
      if (concept.shape == "?" && concept.size == "?" && concept.color == "?" && concept.surface == "?") {
        throw new Exception("Incorrect training data!")
      } else {
        (if (dataObject.shape == concept.shape) true else if (concept.shape == "?") true else false) &&
          (if (dataObject.size == concept.size) true else if (concept.size == "?") true else false) &&
          (if (dataObject.color == concept.color) true else if (concept.color == "?") true else false) &&
          (if (dataObject.surface == concept.surface) true else if (concept.surface == "?") true else false)
      }
    }
  }

}
