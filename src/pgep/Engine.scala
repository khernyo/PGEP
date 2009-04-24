package pgep

object Engine {
  def apply(params: EngineParameters) = {
    val population = new Array[Genotype](params.popsize)
    val engine = new Engine(params, population)
    engine.randomize()
    
    engine
  }
}

class Engine private (params: EngineParameters, pop: Array[Genotype]) {
  protected val random = RNGProvider()
  
  protected var population = pop
  
  private var _fittest: Genotype = _
  def fittest = _fittest
  protected def fittest_=(gt: Genotype) = _fittest = gt
  
  var totalMatingProbability: Double = _
  
  private var _generation: Int = 0
  def generation = _generation
  protected def generation_=(gen: Int) = _generation = gen
  
  private var _evaluationTime: Long = 0
  def evaluationTime = _evaluationTime
  protected def evaluationTime_=(t: Long) = _evaluationTime = t
  def avgEvalTime = evaluationTime.toDouble / (generation + 1)
  
  private var _mutationTime: Long = 0
  def mutationTime = _mutationTime
  protected def mutationTime_=(t: Long) = _mutationTime = t
  def avgMutationTime = mutationTime.toDouble / generation
  
  protected def evolve(gts: Array[Genotype]) = {
    val newpop = params.operators.reproducers flatMap {rep => rep(gts)} toArray
    
    for (mod <- params.operators.modifiers)
      mod(newpop)
    
    mutateConsts()
    
    newpop
  }
  
  protected def calculateFitness() {
    for (gt <- population)
      gt.fitness = params.operators.fitnessFunction(gt)
    
    val invalid = (gt: Genotype) => gt.fitness.isNaN || gt.fitness.isInfinity
    fittest = population.toList.remove(invalid).reduceLeft((_fittest, curr) => if (_fittest.fitness < curr.fitness) _fittest else curr)
    
    fittest.cloneConsts()
    
    totalMatingProbability = params.operators.mpf.setMatingProbability(population)
  }
  
  protected def timeit(body: => Unit) = {
    val startTime = scala.compat.Platform.currentTime
    body
    val endTime = scala.compat.Platform.currentTime
    endTime - startTime
  }
  
  def run(onNextGen: => Unit, onOptimalFound: => Unit, onMaxGenReached: => Unit) {
    var stop = false
    
    while (!stop) {
      evaluationTime += timeit { calculateFitness() }
	  onNextGen
   
	  if (fittest.fitness <= params.goodEnoughFitness) {
	    onOptimalFound
	    stop = true
	  } else if (generation >= params.maxNrGenerations) {
	    onMaxGenReached
        stop = true
      }
   
	  if (fittest.fitness > params.goodEnoughFitness && generation < params.maxNrGenerations) {
	    generation += 1
	    mutationTime += timeit { population = evolve(population) }
	  }
    }
  }
  
  protected def mutateConsts() {
    for (typ <- params.constants.keys)
      while (random.nextDouble() < params.constantMutationProbability) {
        val i = random.nextInt(params.nConstants)
        val c = params.constants(typ)(i)
        params.constants(typ)(i) = Const(c.name, c.typ, params.constgen(typ)())
      }
  }
  
  protected def randomize() {
    for (i <- population.indices) {
      population(i) = Genotype(params.gtparams)
      population(i).randomize()
    }
  }
}