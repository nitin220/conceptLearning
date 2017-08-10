package com.knoldus.concept_learning.domains


case class TumorReport(
                        shape: String,
                        size: String,
                        color: String,
                        surface: String
                      )

case class TrainingData(
                         sample: TumorReport,
                         result: Boolean
                       )


