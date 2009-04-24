package pgep

import scala.collection.mutable.HashMap
import pgep.GeneticOperators.Reproducers.CreateRandom

object EngineParameters {
  def apply(popsize:Int, ngenes: Int, headLen : Int, maxNrGenerations : Int, goodEnoughFitness: Double,
            functions: List[Func], variables: List[Var], tp: TermProbabilities,
            geneLinkingFunction: Func, ops : OperatorSet, constgen : Map[Class[_], () => Any],
            constantMutationProbability : Double, nConstants : Int, geneResultTypes : List[Class[_]]) = {
    
    require(ngenes > 0)
    require((geneLinkingFunction == null) != (geneResultTypes == null))
    require(geneLinkingFunction == null || ngenes == geneLinkingFunction.nparams)
    require(geneResultTypes == null || geneResultTypes.length == ngenes)
    
    val functypes = Set.empty ++ (functions map (_.resultType))
    val funcMap: Map[Class[_], Alphabet[Func]] = Map(functypes.map(t => (t, new AlphabetRO(functions filter (_.resultType == t): _*))).toList: _*)
    val vartypes = Set.empty ++ (variables.map(_.typ))
    val varMap: Map[Class[_], Alphabet[Var]] = Map(vartypes.map(t => (t, new AlphabetRO(variables filter (_.typ == t): _*))).toList: _*)
    
    var hlen = headLen
    var tlen = hlen * (Iterable.max(funcMap.values.map(_.maxParams).toList) - 1) + 1
    if (functions.isEmpty) {
      tlen += hlen
      hlen = 0
      assert(tlen == 1)
    }
    
    val grt = if (geneResultTypes != null) geneResultTypes else geneLinkingFunction.parameterTypes
    
    val consts = new HashMap[Class[_], AlphabetRW[Const]]
    for ((typ, fn) <- constgen)
      consts(typ) = new AlphabetRW[Const]((0 until nConstants).map (i => Const(Symbol("C" + i), typ, fn())): _*)
    val gtparams = GenotypeParameters(ngenes, hlen, tlen, geneLinkingFunction, grt, funcMap, varMap, Map(consts.elements toList: _*), tp)
    
    val nrep = ops.reproducers map (_.nchildren) reduceLeft (_ + _)
    require(nrep <= popsize)
    val operators = if (nrep == popsize) ops
    				else OperatorSet(ops.fitnessFunction, ops.mpf,
    								 ops.reproducers ++ List(new CreateRandom(gtparams, popsize - nrep)) ++ ops.modifiers)
    
    new EngineParameters(popsize, gtparams, maxNrGenerations, goodEnoughFitness, consts, operators, constgen, constantMutationProbability, nConstants)
  }
}

class EngineParameters private (val popsize:Int,
                                val gtparams: GenotypeParameters,
                                val maxNrGenerations: Int,
                                val goodEnoughFitness: Double,
                                val constants: HashMap[Class[_], AlphabetRW[Const]],
                                val operators: OperatorSet,
                                val constgen: Map[Class[_], () => Any],
                                val constantMutationProbability: Double,
                                val nConstants: Int)
