package pgep.GeneticOperators.Reproducers

class Clone(nChildren: Int, selection: GenotypeSelector) extends Reproducer(nChildren, nChildren, selection) {
  protected[Reproducers] override def apply(selected: List[Genotype]) =
    selected map (_.clone())
}
