package com.knoldus.concept_learning.actors

import akka.actor.Actor
import com.knoldus.concept_learning.domains.FindS.{DataObject, Concept}
import com.knoldus.concept_learning.domains.TrainingData

import scala.collection.mutable.ListBuffer
import scala.util.Try

case object GetVersionSpace

class CandidateEliminationActor extends Actor {

  val specificConcepts = ListBuffer[Concept]() += new Concept("ϕ", "ϕ", "ϕ", "ϕ")
  val generalConcepts = ListBuffer[Concept]() += new Concept("?", "?", "?", "?")

  def receive = {

    case trainingData: TrainingData =>
      println(s"Training data: $trainingData")
      import trainingData._
      if (result) {
        println(s"Result : $result, leaning from positive data......\n\n")
        learnFromPositiveDataObject(trainingData)
      } else {
        println(s"Result : $result, leaning from negative data......\n\n")
        learnFromNegativeDataObject(trainingData)
      }

    case GetVersionSpace => sender !(specificConcepts, generalConcepts)

    case dataObject: DataObject => sender ! Try(predict(dataObject)).getOrElse("Incorrect training data")
  }

  private def predict(dataObject: DataObject) = {

    println(s"Test Data***********************$dataObject****************")

    def innerPredict(dataObject: DataObject, concept: DataObject): Boolean = {
      (if (dataObject.shape == concept.shape) true else if (concept.shape == "?") true else false) &&
        (if (dataObject.size == concept.size) true else if (concept.size == "?") true else false) &&
        (if (dataObject.color == concept.color) true else if (concept.color == "?") true else false) &&
        (if (dataObject.surface == concept.surface) true else if (concept.surface == "?") true else false)
    }

    if(specificConcepts.isEmpty || generalConcepts.isEmpty) {
      throw new Exception("Incorrect training data!")
    } else {
      val specificBoundaryResult = specificConcepts map { concept =>
        innerPredict(dataObject, concept)
      }
      val generalBoundaryResult = generalConcepts map { concept =>
        innerPredict(dataObject, concept)
      }
      println(s"**************************************************** specificBoundaryResult is $specificBoundaryResult \n\n")
      println(s"**************************************************** generalBoundaryResult is $generalBoundaryResult \n\n")

      if (specificBoundaryResult.contains(true) && generalBoundaryResult.contains(true)) {
        sender ! true
      } else {
        sender ! false
      }
    }

  }

  private def learnFromPositiveDataObject(sampleData: TrainingData): Unit = {

    import sampleData._

    val sampleConcept = new Concept(sample.shape, sample.size, sample.color, sample.surface)
    val checkSample = new Concept("ϕ", "ϕ", "ϕ", "ϕ")
    if (specificConcepts.contains(checkSample)) {
      maintainGeneralConsistencyForPositive(sampleConcept)
      specificConcepts -= checkSample
      specificConcepts += sampleConcept
    } else {
      maintainGeneralConsistencyForPositive(sampleConcept)
      minimalGeneralizeSpecificLayer(sampleConcept)
    }
  }

  private def minimalGeneralizeSpecificLayer(hypothesis: Concept): ListBuffer[Boolean] = {
    specificConcepts map {
      concept =>
        if (hypothesis.shape == concept.shape) {
          if (hypothesis.size == concept.size) {
            if (hypothesis.color == concept.color) {
              if (hypothesis.surface == concept.surface) {
                true
              } else {
                specificConcepts -= concept
                specificConcepts += concept.copy(surface = "?")
                println(s"**************************** minimal generalizing specific layer turning ${
                  concept.surface
                } to  ? ")
                false
              }
            } else {
              specificConcepts -= concept
              specificConcepts += concept.copy(color = "?")
              println(s"**************************** minimal generalizing specific layer turning ${
                concept.color
              } to  ? ")
              false
            }
          } else {
            specificConcepts -= concept
            specificConcepts += concept.copy(size = "?")
            false
          }
        } else {
          specificConcepts -= concept
          specificConcepts += concept.copy(shape = "?")
          false
        }

    }
  }

  private def learnFromNegativeDataObject(sampleData: TrainingData): Unit = {

    import sampleData._

    val sampleConcept = new Concept(sample.shape, sample.size, sample.color, sample.surface)
    maintainSpecificConsistency(sampleConcept)
    minimalSpecifyGeneralLayer(sampleConcept)
  }

  private def maintainSpecificConsistency(hypothesis: Concept): ListBuffer[Boolean] = {
    specificConcepts map {
      concept =>
        if (hypothesis.shape != concept.shape) {
          if (hypothesis.size != concept.size) {
            if (hypothesis.color != concept.color) {
              if (hypothesis.surface != concept.surface) {
                true
              } else {
                false
              }
            } else {
              false
            }
          } else {
            false
          }
        } else {
          specificConcepts -= concept
          false
        }
    }
  }

  private def minimalSpecifyGeneralLayer(hypothesis: Concept): Unit = {
    val checkConcept = new Concept("?", "?", "?", "?")
    if (generalConcepts.contains(checkConcept)) {
      generalConcepts -= checkConcept
      val features = List(("oval", "circular"), ("large", "small"), ("dark", "light"), ("smooth", "irregular")).zipWithIndex
      generalConcepts ++= (generalConcepts ++ features.flatMap {
        case ((data1, data2), index) =>
          (index: @unchecked) match {
            case 0 => ListBuffer(new Concept(data1, "?", "?", "?"), new Concept(data2, "?", "?", "?"))
            case 1 => ListBuffer(new Concept("?", data1, "?", "?"), new Concept("?", data2, "?", "?"))
            case 2 => ListBuffer(new Concept("?", "?", data1, "?"), new Concept("?", "?", data2, "?"))
            case 3 => ListBuffer(new Concept("?", "?", "?", data1), new Concept("?", "?", "?", data2))
          }
      })
      maintainGeneralConsistencyForNegative(hypothesis)
      specificConcepts foreach {
        value =>
          maintainGeneralConsistencyForPositive(value)
      }
    } else {
      maintainGeneralConsistencyForNegative(hypothesis)
      specificConcepts foreach {
        value =>
          maintainGeneralConsistencyForPositive(value)
      }
    }
  }

  private def maintainGeneralConsistencyForPositive(hypothesis: Concept): ListBuffer[Boolean] = {

    generalConcepts map {
      concept =>
        if (hypothesis.shape == concept.shape || concept.shape == "?") {
          if (hypothesis.size == concept.size || concept.size == "?") {
            if (hypothesis.color == concept.color || concept.color == "?") {
              if (hypothesis.surface == concept.surface || concept.surface == "?") {
                true
              } else {
                generalConcepts -= concept
                false
              }
            } else {
              generalConcepts -= concept
              false
            }
          } else {
            generalConcepts -= concept
            false
          }
        } else {
          generalConcepts -= concept
          false
        }
    }
  }

  private def maintainGeneralConsistencyForNegative(hypothesis: Concept): ListBuffer[Boolean] = {

    generalConcepts map {
      concept =>
        if (hypothesis.shape != concept.shape || concept.shape == "?") {
          if (hypothesis.size != concept.size || concept.size == "?") {
            if (hypothesis.color != concept.color || concept.color == "?") {
              if (hypothesis.surface != concept.surface || concept.surface == "?") {
                true
              } else {
                generalConcepts -= concept
                false
              }
            } else {
              generalConcepts -= concept
              false
            }
          } else {
            generalConcepts -= concept
            false
          }
        } else {
          generalConcepts -= concept
          false
        }
    }
  }

}
