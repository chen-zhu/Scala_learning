import scala.io._
import cs162.assign3.syntax._
import Aliases._
import scala.io.Source.fromFile

//—————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          getType( program.e, new TypeEnv())
          println("This program is well-typed:\n")
          println(Pretty.prettySyntax(program))
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  // Gets all the constructors associated with a given type name.
  // For example, consider the following typedefs:
  //
  // type Either = Left num | Right bool
  // type Maybe = Some num | None
  //
  // With respect to the above typedefs, `constructors` will return
  // the following underneath the given arguments:
  //
  // constructors(Label("Either")) = Map(Label("Left") -> NumT, Label("Right") -> BoolT)
  // constructors(Label("Maybe")) = Map(Label("Some") -> NumT, Label("None") -> UnitT)
  // constructors(Label("Fake")) throws Illtyped
  //
  def constructors(name: Label): Map[Label, Type] =
    typeDefs.find(_.name == name).map(_.constructors).getOrElse(throw Illtyped)

  // Gets the type of the constructor.
  // For example, considering the typedefs given in the `constructors` comment above,
  // `typename` will return the following with the given arguments:
  //
  // typename(Label("Left")) = Label("Either")
  // typename(Label("Right")) = Label("Either")
  // typename(Label("Some")) = Label("Maybe")
  // typename(Label("None")) = Label("Maybe")
  //
  def typename(constructor: Label): Label =
    typeDefs.find(_.constructors.contains(constructor)).getOrElse(throw Illtyped).name

  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      // variables
      case x:Var => env.getOrElse(x,throw Illtyped) 

      // numeric literals
      case _:Num => NumT 

      // boolean literals
      case _:Bool => BoolT 

      // `nil` - the literal for unit
      case _: NilExp => UnitT 

      // builtin arithmetic operators
      case Plus | Minus | Times | Divide => FunT(Seq(NumT, NumT), NumT)  //NUMT & NUMT --> return type NUMT! 

      // builtin relational operators
      case LT | EQ => FunT(Seq(NumT, NumT), BoolT)  //NUMT & NUMT --> return type BoolT! 

      // builtin logical operators
      case And | Or => FunT(Seq(BoolT, BoolT), BoolT) //NUMT & NUMT --> return type BoolT!, Binary & |

      // builtin logical operators
      case Not => FunT(Seq(BoolT), BoolT) //return type --> BOOLT->BOOLT

      // function creation
      case Fun(params, body) =>{
        FunT(params map (_._2), getType(body, env ++ (params.toMap))) //specify params and types 
      }

      // function call
      case Call(fun, args) => {
        getType(fun, env) match {
            case FunT(whatever, forreturn) => {
              if (whatever == args.map((args:Exp) => getType(args,env))) 
                forreturn
              else throw Illtyped
            }
            case _ => throw Illtyped //else, default case for illtyped throwing error
        }
      }

      // conditionals 
      case If(e1, e2, e3) => {
          getType(e1, env) match {
            case BoolT => {
              if(getType(e2, env) == getType(e3, env)) getType(e2, env)
              else throw Illtyped
            }
            case _ => throw Illtyped
          } 
      }

      // let binding
      case Let(x, e1, e2) => getType(e2, env + (x -> getType(e1, env)))

      // recursive binding
      case Rec(x, t1, e1, e2) => {
        val e1t = getType(e1, env + (x -> t1))
        val e2t = getType(e2, env + (x -> t1))
        if(e1t == t1) 
          e2t 
        else 
          throw Illtyped
      }

      // record literals
      case Record(fields) => {
        RcdT(fields.map(
          (t:(Label,Exp)) => (t._1 -> getType(t._2,env))
          ))
      }


      // record access
      case Access(e, field) => {
        getType(e, env) match {
          case RcdT(fields) => fields.getOrElse(field,throw Illtyped)
          case _ => throw Illtyped 
        }
      }

      // constructor use
      case Construct(constructor, e) => {
        val type_name = typename(constructor)
        val Map_result = constructors(type_name)
        val t1 = Map_result.getOrElse(constructor, throw Illtyped)
        if (t1 != getType(e, env)) 
          throw Illtyped
        else 
          TypT(type_name)
      }



      // pattern matching (case ... of ...)
      case Match(e, cases)  => getType(e,env) match {
        
        case TypT(sample) => {
          val tmp0 = cases.map((t:(Label,Var,Exp)) => t._1)

          val tmp1 = cases.map((t:(Label,Var,Exp)) => 
              getType(t._3, env + (t._2 -> constructors(sample).getOrElse(t._1, throw Illtyped))))

          //refer stackoverflow 
          if(tmp0.distinct.sorted != constructors(sample).map( (t:(Label,Type) ) => t._1).toSeq.sorted || tmp0.distinct.length != tmp0.length ) 
            throw Illtyped

          else if(tmp1.distinct.length == 1) 
            tmp1.head

          else 
            throw Illtyped
        
        }

        case _ => throw Illtyped
      }














    }





}
