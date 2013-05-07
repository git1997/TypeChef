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

        val casestudy = {
            tunit.getFile match {
                case None => ""
                case Some(x) => {
                    if (x.contains("linux")) "linux"
                    else if (x.contains("openssl")) "openssl"
                    else ""
                }
            }
        }

        val ts = new CTypeSystemFrontend(tunit, fm)
        //assert(ts.checkASTSilent, "typecheck fails!")
        val env = CASTEnv.createASTEnv(tunit)
        val udm = ts.getUseDeclMap

        val fdefs = filterAllASTElems[FunctionDef](tunit)
        println("#functions " + fdefs.size)
        val errors = fdefs.flatMap(doubleFreeFunctionDef(_, env, udm, casestudy))

        if (errors.isEmpty) {
            println("No double frees found!")
        } else {
            println(errors.map(_.toString + "\n").reduce(_ + _))
        }

        errors.isEmpty
    }

    private def doubleFreeFunctionDef(f: FunctionDef, env: ASTEnv, udm: UseDeclMap, casestudy: String): List[AnalysisError] = {
        var res: List[AnalysisError] = List()

        // It's ok to use FeatureExprFactory.empty here.
        // Using the project's fm is too expensive since control
        // flow computation requires a lot of sat calls.
        // We use the proper fm in DoubleFree (see MonotoneFM).
        val ss = getAllSucc(f, FeatureExprFactory.empty, env).reverse
        val df = new DoubleFree(env, udm, FeatureExprFactory.empty, casestudy)
        val dfp = new DoubleFree(env, udm, fm, casestudy)

        val nss = ss.map(_._1).filterNot(x => x.isInstanceOf[FunctionDef])

        for (s <- nss) {
            val g = df.gen(s)
            val out = df.out(s)

            for ((i, _) <- out)
                for ((_, j) <- g) {
                    j.find(_ == i) match {
                        case None =>
                        case Some(_) => {
                            val gp = dfp.gen(s)
                            val outp = dfp.out(s)
                            for ((ip, _) <- outp)
                                for ((_, jp) <- gp)
                                    jp.find(_ == ip) match {
                                        case None =>
                                        case Some(x) => res ::= new AnalysisError(env.featureExpr(x), "warning: Try to free a memory block that has been released", x)
                                    }

                        }
                    }
                }
        }

        res
    }
}
