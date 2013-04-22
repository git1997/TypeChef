package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureModel, FeatureExpr}

// this trait provides standard routines of the monotone framework
// (a general framework) for dataflow analyses such as liveness,
// available expression, ...
// in contrast to the original idea this implementation
// is variability-aware; for more information about monotone frameworks
// see "Principles of Program Analysis" by (Nielson, Nielson, Hankin)
trait MonotoneFW[T] extends Variables {

    // since C allows variable shadowing we need to track variable usages
    // to their corresponding declarations
    type UseDeclMap = java.util.IdentityHashMap[Id, List[Id]]

    protected val incache = new IdentityHashMapCache[Map[T, FeatureExpr]]()
    protected val outcache = new IdentityHashMapCache[Map[T, FeatureExpr]]()
    protected var env: ASTEnv = null
    protected var udm: UseDeclMap = null
    protected var fm: FeatureModel = null

    def setEnv(newenv: ASTEnv) {
        env = newenv
    }

    def setUseDeclMap(newudm: UseDeclMap) {
        udm = newudm
    }

    def setFm(newfm: FeatureModel) {
        fm = newfm
    }

    // trait members cannot be abstract; therefore, we enforce
    // overriding of id2SetT when writing an own analysis
    // and using explodeIdUse
    protected def id2SetT(i: Id): Set[T] = {
        assert(assertion = false, "You have to override is2SetT when using it")
        Set()
    }

    // while monotone framework usually works on Sets
    // we use maps here for efficiency reasons:
    //   1. the obvious shift from non-variability-aware monotone framework to
    //      a variability-aware version is to change the type of the result set
    //      from Set[Id] to Set[Opt[Id]]. However this changes involves many lookups
    //      and changes to the set.
    //   2. We use Map[T, FeatureExpr] since T is our basic element of interest.
    //      FeatureExpr do not matter so far (they are prominent when using Opt
    //      nodes with List or Set). Since T matters operations on feature expression
    //      are easy and can be delayed to the point at which we *really* need
    //      the result. The delay also involves simplifications of feature
    //      expressions such as "a or (not a) => true".
    type ResultMap = Map[T, FeatureExpr]

    protected def diff(map: ResultMap, d: Set[T]) = {
        var curmap = map
        for (e <- d)
            curmap = curmap.-(e)
        curmap
    }

    protected def join(map: ResultMap, fexp: FeatureExpr, j: Set[T]) = {
        var curmap = map
        for (e <- j) {
            curmap.get(e) match {
                case None    => curmap = curmap.+((e, fexp))
                case Some(x) => curmap = curmap.+((e, fexp or x))
            }
        }
        curmap
    }
}
