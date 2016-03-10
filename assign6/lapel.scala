package cs162.lapel

//——————————————————————————————————————————————————————————————————————————————
// Abstract syntax

// x ∈ Variable  f ∈ FunctionSymbol  p ∈ PredicateSymbol
//    t ∈ Term    ::= x | f(t⋯)
//    g ∈ Goal    ::= p(t⋯) | g1 ∧ g2 | g1 ∨ g2 | a ⊃ g | ∃x⋯.g | output
//    a ∈ Assume  ::= p(t⋯) | p(t⋯) ⊂ g | ∀x⋯.a
// prog ∈ Program ::= a⋯ ⊢ g

sealed abstract class Term
case class Var(name: String) extends Term
case class Function(name: String, args: Seq[Term]) extends Term

sealed abstract class Goal
case class Predicate(name: String, args: Seq[Term]) extends Goal
case class Conjunct(g1: Goal, g2: Goal) extends Goal
case class Disjunct(g1: Goal, g2: Goal) extends Goal
case class Hypothetical(a: Assume, g: Goal) extends Goal
case class Exists(vars: Set[Var], g: Goal) extends Goal
case class Output() extends Goal

// we'll assume that all assumptions are universally quantified, and
// the ones that aren't will just have the empty set of variables
sealed abstract class Assume
case class Fact(vars: Set[Var], pred: Predicate) extends Assume
case class Clause(vars: Set[Var], hd: Predicate, body: Goal) extends Assume

case class Program(as: Seq[Assume], query: Goal)


//——————————————————————————————————————————————————————————————————————————————
// Values and equivalence classes

// values are ground terms (functions without any variables) where
// "ground" includes logic variables standing for unknown terms
//
// the 'contains' method returns whether or not a value contains a
// given logic variable; it's used for the unification occurs check
//
sealed abstract class Value {
  def contains(x: LogicV): Boolean
}
case class FunV(name: String, args: Seq[Value]) extends Value {
  def contains(x: LogicV) = args exists (_ contains x)
}
case class LogicV(id: Int) extends Value {
  def contains(x: LogicV) = x == this
}

// create unique logic variables using "LogicV()"; this should be the
// only way that logic variables are ever created during execution in
// order to ensure freshness
object LogicV {
  private var counter = 0
  def apply(): LogicV = { counter += 1; LogicV(counter) }
}

// equivalence classes involving logic variables (functions are always
// the set representative if they are present in an equivalence class)
case class Equiv(eq: Map[LogicV, Value]) {
  // look up a value and return its set representative in the
  // equivalence class
  def apply(v: Value): Value = {
    //println("1"); 
    //http://docs.scala-lang.org/tutorials/FAQ/finding-symbols.html
    //use @ symbol on variable binding on pattern matching
    v match{
      //1. recur
      case FunV(name, args) => {
        FunV(name, args.map(apply(_)) )
      }
      //2. get logicV object
      case logv @ LogicV(_) => {//http://docs.scala-lang.org/tutorials/FAQ/finding-symbols.html
        val mp = eq.getOrElse(logv, logv)
         //mp match{
          //case logv => logv
          //case default => apply(mp)
        //}
        if (mp == logv) logv
        else apply(mp)
      }

    }
  }



//def Contains(logv: LogicV, v: Value): Boolean = {
    //println("2"); 
    //val mp1 = apply(v)
    //mp1 match {
      //http://oldfashionedsoftware.com/2009/07/10/scala-code-review-foldleft-and-foldright/
      //case FunV(_, po2) => po2.foldLeft(true)(
       //(p1, args) => Contains(logv, apply(args)) && p1
       //     )
      //case LogicV(id) => (logv.id != id)
   // }

 // }


  def Contains(logv: LogicV, v: Value): Boolean = {
    //println("2"); 
    val mp1 = apply(v)
    mp1 match {
      //http://oldfashionedsoftware.com/2009/07/10/scala-code-review-foldleft-and-foldright/
      case FunV(_, po2) => po2.foldLeft(false)(
                                (p1, args) => Contains(logv, apply(args)) && p1
            )
      case LogicV(id) => (logv.id == id)
    }

  }

  // unify two values 
  //Some(eq) if rep =rep  12 
  //Some(eq[X →rep]) if rep =X and X does not occur in rep 21 2 
  //Some(eq[X →rep]) if rep =X and X does not occur in rep 121 
  //unifyTerms(v⃗ zipv⃗,eq) ifrep = f(v⃗),rep = f(v⃗),|v⃗|=|v⃗| 34132434
  //None otherwise
  //handout 9 page 6
  def unify(v1: Value, v2: Value): Option[Equiv] = {
    //println("3"); 
    v1 match{
      case v2 => Some(Equiv(eq))
    }
    val rep1 = apply(v1)
    val rep2 = apply(v2)
    if (rep1 == rep2) Some(Equiv(eq))
    rep1 match {
      //case rep2 => Some(Equiv(eq))
      case FunV(name1, args1) => rep2 match {
        //1. 
        case logv @ LogicV(id2) => {
          if (Contains(logv, rep1)) {
            None
          }
          else Some(Equiv(eq + (logv -> rep1)))
        }

        //2. unifyTerms(pairs, eq) = pairs.foldLeft( Some(eq) )(
        case FunV(name2, args2) => {
          if ( (args1.size == args2.size) && (name1 == name2) ) {
            //scala zip example
            //http://oldfashionedsoftware.com/2009/07/10/scala-code-review-foldleft-and-foldright/
            (args1.zip(args2)).foldLeft[Option[Equiv]]( Some(Equiv(eq)) )(  (po1, po2) => po1 match {
              //case Equiv(newEq) => Equiv(newEq).unify(po2._1, po2._2) //http://danielwestheide.com/blog/2012/12/19/the-neophytes-guide-to-scala-part-5-the-option-type.html
              case Some(Equiv(newEq)) => Equiv(newEq).unify(po2._1, po2._2)

              case None => None
            }
            )
          }
          else None
        }

        //3. default
        case _ => None
      }


      case logv @ LogicV(id1) => rep2 match {
        //1. 
        case logv2 @ LogicV(id2) => {//http://docs.scala-lang.org/tutorials/FAQ/finding-symbols.html
          if (id1 != id2) Some(Equiv(eq + (logv -> rep2)))
          else Some(Equiv(eq))
        }

        //2. 
        case FunV(name2, args2) => {
          //make sure our logic variable is not in the FunV
          if (Contains(logv, rep2)){
            None
          }
          else Some(Equiv(eq + (logv -> rep2)))
        }

        //3. 
        case _ => None
      }


      case _ => None

    }






  }

  // translate a value into a string, mapping logic variables to
  // functions wherever possible
  def valToString(v: Value): String =
    apply(v) match {
      case LogicV(id) ⇒ "L" + id
      case FunV(name, args) ⇒
        if (args.size == 0) name
        else name + "(" + (args map valToString).mkString(", ") + ")"
    }
}


//——————————————————————————————————————————————————————————————————————————————
// Interpreter

import scala.io.Source.fromFile
import scala.language.postfixOps

// the main entry point of the interpreter
object Interpreter {
  // environments map variables to logic variables
  type Env = Map[Var, LogicV]

  // closures contain a goal, its environment, and the assumptions
  // that can be used to prove the goal
  case class Closure(g: Goal, env: Env, as:Seq[Assume]) 

  // entry point
  def main(args: Array[String]) {
    if (args.length < 1) sys.error("need to provide a filename")
    val filename = args(0)
    val input = fromFile(filename).mkString

    val prog = LPLParser.program.run(input, filename) match {
      case Left(error) ⇒ sys.error("parse error: " + error)
      case Right(p) ⇒ p
    }

    eval(
      Seq(Closure(prog.query, Map(), prog.as)),
      Equiv(Map())
    )
  }

  // finds all satisfying solutions to the given stack of closures
  // using the given equivalence relation (where the stack is
  // represented by a sequence, with the head of the sequence being
  // the top of the stack)
  def eval(goals: Seq[Closure], eq: Equiv): Unit =
    if (goals nonEmpty) goals.head match {
      // atomic predicate goal

      //When attempting to prove an atomic predicate goalp(⃗t),the interpreter must choose which assumption to 
      //use. In some cases the choice is obvious (e.g., in the example when trying to prove the predicate isodd
      // there is only one applicable assumption), but in other cases it isn’t obvious (e.g., in the same example when trying to prove 
      //parity there are multiple applicable assumptions and it isn’t clear which one will work until we try to prove the resulting goals).
      case goalClo @ Closure(p: Predicate, env, as) ⇒{
        //reference: http://docs.scala-lang.org/overviews/collections/maps.html
        //println("4"); 
        as.map(_ match {
            //1. 
            case Fact(vars, pred) => {
              val cl = Closure(pred, (vars.map(_ -> LogicV())).toMap, as)
              matchPredicates(cl, goalClo, eq) match {
                //case newEq => eval(goals.tail, newEq) //http://danielwestheide.com/blog/2012/12/19/the-neophytes-guide-to-scala-part-5-the-option-type.html
                case Some(newEq) => eval(goals.tail, newEq)
                case None => {}
              }
            }
            
            //2. 
            case Clause(vars, hd, body) => {
              val vm = (vars.map(_ -> LogicV())).toMap
              val clo_new = Closure(hd, vm, as)
              val tmp = matchPredicates(clo_new, goalClo, eq)
              tmp match {
                //case newEq => eval(Closure(body, vm, as) +: goals.tail, newEq)
                case Some(newEq) => eval(Closure(body, vm, as) +: goals.tail, newEq)
                //if None, then return empty set and initialize 
                case None => {}
              }
            }

            case _ => None


          })


      }

      //conjunct goal
      //When applying the conjunction rule, which has multiple judgements as premises, 
      //the interpreter must choose which judgement premise to attempt to prove first
      case Closure(Conjunct(g1, g2), env, as) ⇒{
        //println("5"); 
        //http://docs.scala-lang.org/tutorials/FAQ/finding-symbols.html   1 +: List(2, 3) :+ 4
        //reference common method in doc 
        eval(Closure(g1, env, as) +: Closure(g2, env, as) +: goals.tail,
            eq)
      }


      // disjunct goal
      //When applying the disjunction rule, which has one of two possible 
      //judgement premises, the interpreter must choose which judgement 
      //premise to attempt to prove. 
      case Closure(Disjunct(g1, g2), env, as) ⇒{
        //println("6"); 
        //http://docs.scala-lang.org/tutorials/FAQ/finding-symbols.html
        //reference common method in doc 
        eval(Closure(g1, env, as) +: goals.tail, 
            eq)
        eval(Closure(g2, env, as) +: goals.tail, 
            eq) 
    }

      // hypothetical implication goal
      case Closure(Hypothetical(a, g), env, as) ⇒ {
        //println("7"); 
        eval(Closure(g, env, a +: as) +: goals.tail, 
            eq)
      }

      // existential quantification goal
      case Closure(Exists(vars, g), env, as) ⇒{
        //println("8"); 
        val ev = vars.foldLeft(env)((tmpEnv, curVar) => tmpEnv + curVar -> LogicV())
        eval(Closure(g, ev, as) +: goals.tail, eq)

      }

      // output goal
      case Closure(_:Output, env, _) ⇒
        //println("9"); 
        if (env nonEmpty) {
          env map {
            case (x, v) ⇒ println(x.name + " ↦ " + eq.valToString(v))
          }
        }
        else println("true")
        println("========")
        eval(goals.tail, eq)
    }

  // HELPER FUNCTION USED BY THE "ATOMIC PREDICATE GOAL" CASE ABOVE
  //
  // given an "assumption closure" for a predicate coming from a fact
  // or the head of a clause (aClo) and a "goal closure" for the
  // current goal (gClo) and the current equivalence relation (eq),
  // try to match up the assumption and the goal, potentially creating
  // a new equivalence relation in the process
  //
  // this will be the method by which we determine whether a given
  // assumption can help us prove a given atomic predicate goal
  def matchPredicates(aClo: Closure, gClo: Closure, eq: Equiv): Option[Equiv] = {
    //println("10"); 

    (aClo.g, gClo.g) match {
      case (Predicate(po1, po_1), Predicate(po2, po_2)) => {
        if (po1 == po2 && po_1.size == po_2.size) {

          val logM = (
                      (po_1.map( termToValue(_, aClo.env))).zip( (po_2.map(termToValue(_, gClo.env))))
                      )
          
          //http://oldfashionedsoftware.com/2009/07/10/scala-code-review-foldleft-and-foldright/
          logM.foldLeft[Option[Equiv]](Some(eq))((tmp, tmp1) => 
            tmp match {
              case Some(middleEq) => middleEq.unify(tmp1._1, tmp1._2)
              case _ => None
            }
          )
        }
        else None
      }

      case _ => None
    }
  }

  // HELPER FUNCTION USED BY "matchPredicates"
  //
  // transform a term (i.e., variable or function on terms) into a
  // value (i.e., logic variable or function on values)
  def termToValue(t: Term, env: Env): Value =
    t match {
      case x:Var ⇒ env(x)
      case Function(name, args) ⇒ FunV(name, args map (termToValue(_, env)))
    }
}


//——————————————————————————————————————————————————————————————————————————————
// Parser
//
// — see the test programs test〈X〉.lpl for examples of concrete syntax.

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.language.implicitConversions

object LPLParser extends StdTokenParsers with PackratParsers {
  type Parser[T] = PackratParser[T]
  type Error = String
  type Tokens = StdLexical

  val lexical = new StdLexical

  lexical.delimiters ++= Seq(".", "(", ")", ":-", ",", ";", "{", "}", "?", "=>")
  lexical.reserved ++= Seq("output")

  class Runnable[A](val xs: Parser[A]) {
    def run(stream: String, filename: String): Either[Error, A] = {
      val commentFree = stream.replaceAll("--.*", "")
      val tokens = new lexical.Scanner(commentFree)

      phrase(xs)(tokens) match {
        case e: NoSuccess    ⇒ Left(e.toString)
        case Success(res, _) ⇒ {/*println(res);*/ Right(res)}
      }
    }
  }

  implicit def parserToRunnable[A](p: Parser[A]): Runnable[A] = new Runnable(p)

  lazy val program: Parser[Program] =
    (factOrClause*) ~ "?" ~ goal ~ "." ^^ {
      case fcs ~ "?" ~ g ~ "." ⇒ Program(fcs, g)
    }

  lazy val factOrClause: Parser[Assume] =
    fact | clause

  lazy val fact: Parser[Fact] =
    ( vars ~ predicate ~ "." ^^ { case v ~ p ~ "." ⇒ Fact(v, p) } |
      predicate ~ "." ^^ { case p ~ "." ⇒ Fact(Set(), p) }
    )

  lazy val clause: Parser[Clause] =
    ( vars ~ predicate ~ ":-" ~ goal ~ "." ^^ {
        case v ~ p ~ ":-" ~ g ~ "." ⇒ Clause(v, p, g) } |
      predicate ~ ":-" ~ goal ~ "." ^^ {
        case p ~ ":-" ~ g ~ "." ⇒ Clause(Set(), p, g)
      }
    )

  lazy val factOrClauseNoDot: Parser[Assume] =
    clauseNoDot | factNoDot | "(" ~> factOrClauseNoDot <~ ")"

  lazy val factNoDot: Parser[Fact] =
    ( vars ~ predicate ^^ { case v ~ p ⇒ Fact(v, p) } |
      predicate ^^ { case p ⇒ Fact(Set(), p) })

  lazy val clauseNoDot: Parser[Clause] =
    ( vars ~ predicate ~ ":-" ~ goal ^^ {
        case v ~ p ~ ":-" ~ g ⇒ Clause(v, p, g) } |
      predicate ~ ":-" ~ goal ^^ {
        case p ~ ":-" ~ g ⇒ Clause(Set(), p, g) }
    )

  lazy val vars: Parser[Set[Var]] =
    "{" ~> repsep(upperid, ",") <~ "}" ^^ {
      case vs ⇒ vs.map(v ⇒ Var(v)).toSet
    }

  lazy val goal: Parser[Goal] =
    conjunct | disjunct | hypothetical | output | predicate | exists | parens

  lazy val parens: Parser[Goal] =
    "(" ~> goal <~ ")"

  lazy val predicate: Parser[Predicate] =
    ident2 ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
      case id ~ "(" ~ tms ~ ")" ⇒ Predicate(id, tms)
    }

  lazy val conjunct: Parser[Conjunct] =
    goal ~ "," ~ goal ^^ { case g1 ~ "," ~ g2 ⇒ Conjunct(g1, g2) }

  lazy val disjunct: Parser[Disjunct] =
    goal ~ ";" ~ goal ^^ { case g1 ~ ";" ~ g2 ⇒ Disjunct(g1, g2) }

  lazy val hypothetical: Parser[Hypothetical] =
    factOrClauseNoDot ~ "=>" ~ goal ^^ { case a ~ _ ~ g ⇒ Hypothetical(a, g) }

  lazy val exists: Parser[Exists] =
    vars ~ goal ^^ { case vs ~ g ⇒ Exists(vs, g) }

  lazy val output: Parser[Output] =
    "output" ^^^ Output()

  lazy val term: Parser[Term] =
    function | ident2 ^^ {
      case s ⇒ 
        if (Character.isUpperCase(s.charAt(0))) Var(s) else Function(s, Seq())
    }

  lazy val function: Parser[Function] =
    ident2 ~ "(" ~ repsep(term, ",") ~ ")" ^^ {
      case id ~ "(" ~ tms ~ ")" ⇒ Function(id, tms)
    }

  lazy val ident2: Parser[String] =
    numericLit ~ ident ^^ { case n ~ i ⇒ n + i } | ident | numericLit

  lazy val upperid: Parser[String] =
    ident flatMap { s ⇒ {
      if (Character.isUpperCase(s.charAt(0)))
        success(s)
      else
        failure("unexpected lowercase letter")
    }}
}
