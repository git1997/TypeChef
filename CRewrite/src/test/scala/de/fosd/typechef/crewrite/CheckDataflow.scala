package de.fosd.typechef.crewrite

import de.fosd.typechef.parser.c._
import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureModel}
import de.fosd.typechef.typesystem.{CDeclUse, CTypeSystemFrontend}

object CheckDataflow extends IntraCFG with CFGHelper with Liveness {

    def checkDataflow(tunit: TranslationUnit, fm: FeatureModel = FeatureExprFactory.default.featureModelFactory.empty) {
      val fdefs = filterAllASTElems[FunctionDef](tunit)
      val ts = new CTypeSystemFrontend(tunit, fm) with CDeclUse
      val udm = ts.getUseDeclMap
      fdefs.map(intraDataflowAnalysis(_, fm, udm))
    }

    private def intraDataflowAnalysis(f: FunctionDef, fm: FeatureModel, udm: UseDeclMap) {
      if (f.stmt.innerStatements.isEmpty) return

      val env = CASTEnv.createASTEnv(f)
      setEnv(env)
      val ss = getAllSucc(f.stmt.innerStatements.head.entry, FeatureExprFactory.empty, env)
      setFm(fm)
      setUseDeclMap(udm)

      val nss = ss.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])
      for (s <- nss) in(s)
    }

}
