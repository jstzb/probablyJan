import java.io.File

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.learning.{EMWithBP, EMWithImportance, EMWithMH, EMWithVE}
import com.cra.figaro.algorithm.sampling.{Importance, MetropolisHastings, ProposalScheme}
import com.cra.figaro.language.{Element, Flip, Universe}
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.patterns.learning.{ModelParameters, ParameterCollection}

import scala.util.Random

/**
  * Created by jan on 14.07.16.
  *
  * Figure : x -> y -> z
  *
  *
  * Task 3
  * This exercise points out a pitfall with the EM algorithm. Con-
  * sider the simple Bayesian network shown in figure 13.11.
  * You’re going to learn the parameters of this network.
  *
  * Subtask a
  * Create a Figaro program to represent this network using
  * the model parameters learning pattern. Use a Beta(1, 1)
  * prior for all parameters.
  *
  * Subtask b
  * Create a training set of about 10 instances in which only
  * the variables X and Z are observed. Make sure that in
  * every training instance, X and Z take on the same value. Sometimes they’re
  * both true and sometimes they’re both false.
  *
  * Subtask c
  * Use the EM algorithm to learn the parameters of the model. Experiment with
  * different variants of EM .
  *
  * Subtask d
  * Now create a test case using the posterior parameters. Observe that X is true
  * and compute the probability that Z is true. You’ll probably find that this
  * probability is not close to 1, even though X is true and X and Z were always
  * equal in the training set. Try to explain why this might be.
  *
  * Subtask e
  * Now let’s fix the problem. Change the prior for Y given that X is true to
  * Beta(2, 1), and the prior for X given that Y is true to Beta(1, 2). Again, use
  * EM to learn the parameters and run your test. This time, the probability that
  * Z is true should be close to 1. Why do you think learning worked in this case?
  * What moral can you derive for your own use of EM ?
  *
  *
  * Task 4
  * Now repeat question 3, using Bayesian learning instead of EM .
  *
  * Try different inference algorithms on this problem. Which algorithm works
  * best? Why do you think that is?
  * Subtask a
  * Compare and contrast the results of Bayesian learning with the EM results.
  * Subtask b
  * Do the results of Bayesian learning differ significantly whether you use
  * Beta(1, 1) priors for all variables or you use Beta(2, 1) and Beta(1, 2) priors
  * for Y? Are the results with the Beta(2, 1) and Beta(1, 2) priors significantly
  * different from EM ? If so, why do you think that is?
  *
  *
  * Task 5
  * Now that you’ve seen how parameter learning works, it’s time to revisit the
  * spam filter example from chapter 3.
  * Subtask a
  * Rewrite the example to use the model parameters learning pattern.
  * Subtask b
  * Change the learning method from the basic EM method used in chapter 3,
  * which trains on all emails simultaneously, to an offline learning method.
  */

object HW7 {

  val impParticle = 500
  val mhParticle = 10000
  val bpParticle = 1000
  val trainSize = 10

  class ModelA(paramCollection:ParameterCollection) {
    val x=Flip(paramCollection.get("xP"))
    val y=CPD(x,
      true -> Flip(paramCollection.get("y1P")),
      false -> Flip(paramCollection.get("y2P")))
    val z=CPD(y,
      true -> Flip(paramCollection.get("z1P")),
      false -> Flip(paramCollection.get("z2P")))
  }
  class ModelB(es: Array[Element[Double]]) {
    if(es.size!=5) sys.error("Array has to consist of 5 elements")
    val x=Flip(es(0))
    val y=CPD(x,
      true -> Flip(es(1)),
      false -> Flip(es(2)))
    val z=CPD(y,
      true -> Flip(es(3)),
      false -> Flip(es(4)))
  }

  def train(i:Int,params:ModelParameters) : Unit = {
    for( i <- 1 to i){
      val model = new ModelA(params.priorParameters)
      val rnd = Random.nextBoolean()
      model.x.observe(rnd)
      model.z.observe(rnd)
    }
  }

  def train(i:Int,es:Array[Element[Double]]) : Unit = {
    for( i <- 1 to i){
      val model = new ModelB(es)
      val rnd = Random.nextBoolean()
      model.x.observe(rnd)
      model.z.observe(rnd)
    }
  }

  def initParaA():ModelParameters = {
    val params = ModelParameters()
    val xP = Beta(1,1)("xP",params)
    val y1P = Beta(1,1)("y1P",params)
    val y2P = Beta(1,1)("y2P",params)
    val z1P = Beta(1,1)("z1P",params)
    val z2P = Beta(1,1)("z2P",params)
    params
  }

  def initParaB():ModelParameters = {
    val params = ModelParameters()
    val xP = Beta(1,1)("xP",params)
    val y1P = Beta(2,1)("y1P",params)
    val y2P = Beta(1,2)("y2P",params)
    val z1P = Beta(1,1)("z1P",params)
    val z2P = Beta(1,1)("z2P",params)
    params
  }

  def emLearn(algo:String, params:ModelParameters): Double ={
    train(trainSize,params)
    algo match {
      case "im" =>
        val learningAlg = EMWithImportance(trainSize,impParticle,params)
        learningAlg.start()
        val futureModel = new ModelA(params.posteriorParameters)
        futureModel.x.observe(true)
        val result = VariableElimination.probability(futureModel.z,true)
        learningAlg.kill()
        result
      case "mh" =>
        val learningAlg = EMWithMH(trainSize,mhParticle,ProposalScheme.default,params)
        learningAlg.start()
        val futureModel = new ModelA(params.posteriorParameters)
        futureModel.x.observe(true)
        val result = VariableElimination.probability(futureModel.z,true)
        learningAlg.kill()
        result
      case "bp" =>
        val learningAlg = EMWithBP(trainSize,bpParticle,params)
        learningAlg.start()
        val futureModel = new ModelA(params.posteriorParameters)
        futureModel.x.observe(true)
        val result = VariableElimination.probability(futureModel.z,true)
        learningAlg.kill()
        result
      case _ =>
        val learningAlg = EMWithVE(trainSize,params)
        learningAlg.start()
        val futureModel = new ModelA(params.posteriorParameters)
        futureModel.x.observe(true)
        val result = VariableElimination.probability(futureModel.z,true)
        learningAlg.kill()                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
        result
    }
  }

  def bLearn(algo:String, model:String): Double = {
    var es :Array[Element[Double]] = Array.empty
    if(model == "b")
    {es = Array(Beta(1,1),Beta(2,1),Beta(1,2),Beta(1,1),Beta(1,1))}
    {es = Array.fill(5)(Beta(1,1))}
    train(trainSize,es)
    val futureModel = new ModelB(es)
    futureModel.x.observe(true)
    algo match {
      case "bp" =>
        val alg = BeliefPropagation(bpParticle,futureModel.x,futureModel.y,futureModel.z)
        alg.start()
        alg.probability(futureModel.z,true)
      case "mh" =>
        val alg = MetropolisHastings(mhParticle,ProposalScheme.default,futureModel.x,futureModel.y,futureModel.z)
        alg.start()
        alg.probability(futureModel.z,true)
      case _ =>
        val alg = Importance(impParticle,futureModel.x,futureModel.y,futureModel.z)
        alg.start()
        alg.probability(futureModel.z,true)
    }
  }

  def printTask(taskNum:String, subtaskNum: String, algo:String):Unit = {
    println()
    println("#######################################")
    println("Running Task " + taskNum + " " +subtaskNum + ")")
    println("Using algorithm " + algo.toUpperCase())
    println("#######################################")
    println()
    (taskNum,subtaskNum) match {
      case ("3","e") =>
        val result = emLearn(algo,initParaB())
        println("Calulated probability with learned model is " + result.toString)
      case ("3",_) =>
        val result = emLearn(algo,initParaA())
        println("Calulated probability with learned model is " + result.toString)
      case ("4",model) =>
        val result = bLearn(algo,model)
        println("Calulated probability with learned model is " + result.toString)

    }
    println()
    println()
  }

  def main(args: Array[String]) {
    args match {
      case Array("3",x,y) =>
        printTask("3",x,y)
      case Array("4",x,y) =>
        printTask("4",x,y)
      case _ =>
        println()
        println("Input not recognized")
        println()
    }

}}
