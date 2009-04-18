package pgep

import scala.collection.mutable.HashMap

object Engine {
  def apply(params: EngineParameters, populationSize: Int) = {
    val population = new Array[Genotype](populationSize)
    
    val consts = new HashMap[Class[_], AlphabetRW[Const]]
    for ((typ, fn) <- params.constgen)
      consts(typ) = new AlphabetRW[Const]((0 until params.nConstants).map (i => Const(Symbol("C" + i), typ, fn())): _*) 

    val gtParams = params.createGenotypeParameters(Map(consts.elements toList: _*))
    
    val engine = new Engine(params, population, consts, gtParams)
    engine.randomize()
    
    engine
  }
}

class Engine(params: EngineParameters, pop: Array[Genotype], constants: HashMap[Class[_], AlphabetRW[Const]], gtParams: GenotypeParameters) {
  protected val random = RNGProvider()
  
  protected var population = pop
  
  private var _fittest: Genotype = _
  def fittest = _fittest
  protected def fittest_=(gt: Genotype) = _fittest = gt
  
  var totalMatingProbability: Double = _
  
  private var _generation: Int = 0
  def generation = _generation
  protected def generation_=(gen: Int) = _generation = gen
  
  protected def evolve(gts: Array[Genotype]) = {
    val newGenotypes = (0 until population.length) map (_ => Genotype(gtParams)) toArray
    
    var dstIdx = 0
    for (rep <- params.operators.reproducers)
      dstIdx = rep(gts, newGenotypes, dstIdx)
    
    for (i <- (dstIdx until newGenotypes.length))
      newGenotypes(i).randomize()
    
    for (mod <- params.operators.modifiers)
      mod(newGenotypes)
    
    mutateConsts()
    
    newGenotypes
  }
  
  protected def calculateFitness() {
    for (gt <- population)
      gt.fitness = params.operators.fitnessFunction(gt)
    
    val invalid = (gt: Genotype) => gt.fitness.isNaN || gt.fitness.isInfinity
    fittest = population.toList.remove(invalid).reduceLeft((_fittest, curr) => if (_fittest.fitness > curr.fitness) _fittest else curr)
    
    fittest.cloneConsts()
    
    totalMatingProbability = params.operators.mpf.setMatingProbability(population.toList)
  }
  
  def run() {
    calculateFitness()
    if (fittest.fitness > 0 && generation < params.maxNrGenerations) {
      generation += 1
      population = evolve(population)
      run()
    }
  }
  
  protected def mutateConsts() {
    for (typ <- constants.keys)
      while (random.nextDouble() < params.constantMutationProbability) {
        val i = random.nextInt(params.nConstants)
        val c = constants(typ)(i)
        constants(typ)(i) = Const(c.name, c.typ, params.constgen(typ)())
      }
  }
  
  protected def randomize() {
    for (i <- (0 until population.length)) {
      population(i) = Genotype(gtParams)
      population(i).randomize()
    }
  }
}
