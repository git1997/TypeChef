package de.fosd.typechef.crewrite

import java.io.{Writer, StringWriter}

import de.fosd.typechef.featureexpr._
import de.fosd.typechef.parser.c.{TranslationUnit, FunctionDef}
import de.fosd.typechef.typesystem._


class CAnalysisFrontend(tunit: TranslationUnit, fm: FeatureModel = FeatureExprFactory.default.featureModelFactory.empty) extends CFGHelper {

    def dumpCFG(writer: Writer = new StringWriter()) {
        val fdefs = filterAllASTElems[FunctionDef](tunit)
        val dump = new DotGraph(writer)
        val env = CASTEnv.createASTEnv(tunit)
        dump.writeHeader("CFGDump")

        for (f <- fdefs) {
            dump.writeMethodGraph(getAllSucc(f, fm, env), env, Map())
        }
        dump.writeFooter()
        dump.close()

        if (writer.isInstanceOf[StringWriter])
            println(writer.toString)
    }

    def doubleFree() {

        val ts = new CTypeSystemFrontend(tunit, fm)
        //assert(ts.checkASTSilent, "typecheck fails!")
        val env = CASTEnv.createASTEnv(tunit)
        val udm = ts.getUseDeclMap

        val fdefs = filterAllASTElems[FunctionDef](tunit)
        val errors = fdefs.flatMap(doubleFreeFunctionDef(_, env, udm))

        if (errors.isEmpty) {
            println("No double frees found!")
        } else {
            println(errors)
        }

        errors.isEmpty
    }

    private def doubleFreeFunctionDef(f: FunctionDef, env: ASTEnv, udm: UseDeclMap): List[AnalysisError] = {
        var res: List[AnalysisError] = List()

        // TODO: head.entry is wrong head might be optional and part of an alternative!
        val ss = getAllSucc(f.stmt.innerStatements.head.entry, fm, env)
        val df = new DoubleFree(env, udm, fm)

        val nss = ss.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])

        for (s <- nss) {
            val g = df.gen(s)
            val out = df.out(s)

            for ((i, _) <- out)
                for ((_, j) <- g) {
                    j.find(_ == i) match {
                        case None =>
                        case Some(x) => res ::= new AnalysisError(env.featureExpr(x), "warning: Try to free a memory block that has been released", x)
                    }
                }
        }

        res
    }
}
