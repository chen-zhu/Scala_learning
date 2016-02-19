import scala.io._
import cs162.assign4.syntax._
import Aliases._
import scala.io.Source.fromFile

//——————————————————————————————————————————————————————————————————————————————
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
          println(Pretty.prettySyntax(program))
          getType( program.e, new TypeEnv())
          println("This program is well-typed")
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  // Gets a listing of the constructor names associated with a given type definition.
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructors`, along with return values:
  //
  // constructors("Either") = Set("Left", "Right")
  // constructors("Foo") = a thrown Illtyped exception
  //
  def constructors(name: Label): Set[Label] =
    typeDefs.find(_.name == name).map(_.constructors.keySet).getOrElse(throw Illtyped)

  // Takes the following parameters:
  // -The name of a user-defined type
  // -The name of a user-defined constructor in that user-defined type
  // -The types which we wish to apply to the constructor
  // Returns the type that is held within the constructor.
  //
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructorType`, along with return values:
  //
  // constructorType("Either", "Left", Seq(NumT, BoolT)) = NumT
  // constructorType("Either", "Right", Seq(NumT, BoolT)) = BoolT
  // constructorType("Either", "Left", Seq(NumT)) = a thrown Illtyped exception
  // constructorType("Either", "Right", Seq(BoolT)) = a thrown Illtyped exception
  // constructorType("Either", "Foo", Seq(UnitT)) = a thrown Illtyped exception
  // constructorType("Bar", "Left", Seq(UnitT)) = a thrown Illtyped exception
  //
  def constructorType(name: Label, constructor: Label, types: Seq[Type]): Type = 
    (for {
      td <- typeDefs
      rawType <- td.constructors.get(constructor)
      if (types.size == td.tvars.size)
    } yield replace(rawType, td.tvars.zip(types).toMap)).headOption.getOrElse(throw Illtyped)

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

  // Given a type and a mapping of type variables to other types, it
  // will recursively replace the type variables in `t` with the
  // types in `tv2t`, if possible.  If a type variable isn't
  // in `tv2t`, it should simply return the original type.  If a
  // `TFunT` is encountered, then whatever type variables it defines
  // (the first parameter in the `TFunT`) should overwrite whatever is in
  // `tv2t` right before a recursive `replace` call.  In other words,
  // type variables can shadow other type variables.
  //
  def replace( t:Type, tv2t:Map[TVar, Type] ): Type =
    t match {
      case NumT | BoolT | UnitT => t

      case FunT(params, ret) => {
        //println("1") 
        //params: Seq[cs162.assign4.syntax.Type]
        val new_param = params.map( (tp: Type) => replace(tp,tv2t) ) 
        FunT(new_param,replace(ret,tv2t))
      }



      //used as Poly function place holder replacement 
      //--> need to reaplace mapvalue (place holder) with actual expression type 
      case RcdT(fields) => {
        //println("2") 
        //scala.collection.immutable.Iterable --> ask TA for help
        //RcdT(fields.map( {case (str, tp) => replace(tp, tv2t) } )) 
        RcdT(fields.map( {
          case (str: Label, tp: Type) => (str -> replace(tp, tv2t)) 
          } ))
        /*
          found   : scala.collection.immutable.Iterable[cs162.assign4.syntax.Type]
          required: Map[cs162.assign4.syntax.Aliases.Label,cs162.assign4.syntax.Type]
          (which expands to)  Map[String,cs162.assign4.syntax.Type]
        */
        //fields: Map[cs162.assign4.syntax.Aliases.Label,  cs162.assign4.syntax.Type]
        //RcdT(fields.mapValues(tp=>replace(tp, tv2t)))//mapValue, returns a view on the original map 
        //http://blog.bruchez.name/2013/02/mapmap-vs-mapmapvalues.html
      }
      

      //user defined, Either, poly
      case TypT(name, typs) => {
        //println("3") 
        //typs: Seq[cs162.assign4.syntax.Type]
        val sequence = typs.map((single: Type) => replace(single, tv2t))
        
        TypT(name, sequence)
      } 

      case tv:TVar => tv2t.getOrElse(tv,t)





      //ask stackoverflow && github
      case TFunT(tvars, funt) => { //User definied function --> shadowing 
        /*
        println("4") 
        val tv2t_2 = tvars.foldLeft(tv2t){
          (po2, po1) =>  (po2 - po1)
        }
        replace(funt, tv2t_2)
        */
        //val newtv2t = tv2t.diff(tvars) //???
        val newtv2t = tv2t -- tvars

        //val newtv2t = tv2t.map()
        //val newtv2t = tv2t -- (tv2t.filterKeys{tvars.contains(_) == false})

        //http://alvinalexander.com/scala/how-to-cast-objects-class-instance-in-scala-asinstanceof
        /*
              checker.scala:135: error: type mismatch;
              found   : cs162.assign4.syntax.Type
              required: cs162.assign4.syntax.FunT
              val funt_2:FunT = replace(funt,newtv2t)
        */
        //val funt_2:FunT = replace(funt,newtv2t).asInstanceOf[FunT]
        val funt_2 = replace(funt,newtv2t)

        funt_2 match{
          case a: FunT => {
            TFunT(tvars,funt_2.asInstanceOf[FunT])//force type casting
          }
          case _ => throw Illtyped
        }
      } 


    }

  // HINT - the bulk of this remains unchanged from the previous assignment.
  // Feel free to copy and paste code from your last submission into here.
  def getType( e:Exp, env:TypeEnv ): Type =
    e match {

      // copy from ASSIGN 3 
      case x:Var => env.getOrElse(x,throw Illtyped) 

      // copy from ASSIGN 3 
      case _:Num => NumT 

      // copy from ASSIGN 3 
      case _:Bool => BoolT

      // copy from ASSIGN 3 
      case _: Unit => UnitT 

      // copy from ASSIGN 3 
      case Plus | Minus | Times | Divide => FunT(Seq(NumT, NumT), NumT)

      // copy from ASSIGN 3 
      case LT | EQ => FunT(Seq(NumT, NumT), BoolT)

      // copy from ASSIGN 3 
      case And | Or => FunT(Seq(BoolT, BoolT), BoolT)

      // copy from ASSIGN 3 
      case Not => FunT(Seq(BoolT), BoolT)

      // copy from ASSIGN 3 
      case Fun(params, body) =>{
        //println("5") 
        FunT(params map (_._2), getType(body, env ++ (params.toMap))) //specify params and types 
      }

      // copy from ASSIGN 3 
      case Call(fun, args) => {
        //println("6") 
        getType(fun, env) match {
            case FunT(whatever, forreturn) => {
              if (whatever == args.map((args:Exp) => getType(args,env))) 
                forreturn
              else throw Illtyped
            }
            case _ => throw Illtyped //else, default case for illtyped throwing error
        }
      }

      // copy from ASSIGN 3 
      case If(e1, e2, e3) => {
        //println("7") 
          getType(e1, env) match {
            case BoolT => {
              if(getType(e2, env) == getType(e3, env)) getType(e2, env)
              else throw Illtyped
            }
            case _ => throw Illtyped
          } 
      }

      // copy from ASSIGN 3 
      case Let(x, e1, e2) => getType(e2, env + (x -> getType(e1, env)))

      // copy from ASSIGN 3 
      case Rec(x, t1, e1, e2) => {
        //println("8") 
        val e1t = getType(e1, env + (x -> t1))
        val e2t = getType(e2, env + (x -> t1))
        if(e1t == t1) 
          e2t 
        else 
          throw Illtyped
      }

      // copy from ASSIGN 3 
      case Record(fields) => {
        //println("9") 
        RcdT(fields.map(
          (t:(Label,Exp)) => (t._1 -> getType(t._2,env))
          ))
      }

      // copy from ASSIGN 3 
      case Access(e, field) => {
        //println("10") 
        getType(e, env) match {
          case RcdT(fields) => fields.getOrElse(field,throw Illtyped)
          case _ => throw Illtyped 
        }
      }


      // copy from ASSIGN 3 
      case c @ Construct(constructor, typs, e) => {
        //println("11") 
        val type_name = typename(constructor)

        //constructorType("Either", "Left", Seq(NumT, BoolT)) = NumT
        val c_type_name = constructorType(type_name,constructor,typs)

        val tp1 = getType(e,env)

        if(c_type_name == tp1) TypT(type_name,typs)

        else throw Illtyped
      }



      /*
        type EitherNB = Left bool
              | Right num

        case Left(true) of
        | Left(b) => Left(b)
        | Right(b) => Left(true)
      */
      //the number should match!
      case Match(e, cases)  => {
        //println("12") 
        getType(e,env) match {
          case TypT(name,typs) => { 
            val cons = constructors(name) //set[Label]
            //val c_t_set = cases.map( (ca) => ca._1).toSet
            val c_t_s = cases.map( (single_case: (Label,Var,Exp)) => single_case._1).toSeq //Seq[(String, cs162.assign4.syntax.Var, cs162.assign4.syntax.Exp)]
            if (c_t_s.lengthCompare(cons.toSeq.length) != 0 || cons != c_t_s.toSet ) throw Illtyped
            //the type of e must always be a user-defined type!!!!!
            val final_case = cases.map( ( single_case: (Label,Var,Exp) ) =>  //Seq[(String, cs162.assign4.syntax.Var, cs162.assign4.syntax.Exp)]
              { 
                //println(single_case._1)
                //constructorType("Either", "Left", Seq(NumT, BoolT)) = NumT, update current environment and ask for type
                getType(single_case._3, env+(single_case._2->constructorType(name,single_case._1,typs))) 

              } ).toSet.toList //convert to set first, eliminate duplicate, then convert it back 

            //constructorType("Either", "Left", Seq(NumT, BoolT)) = NumT //update environment here--> and need to remoce dupe

            if(final_case.length!=1) throw Illtyped
            final_case.head //return first one.

          }


          case _ => throw Illtyped
        }
      }
        
        /*case TypT(sample, env) => {
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

        case _ => throw Illtyped*/
      

      case TAbs(tvars, fun) =>{
        //println("13") 
        val tp1 = getType(fun,env) 

        /*if(tp1 == ???) TFunT(tvars, f)

        else throw Illtyped*/

        tp1 match {
          case a:FunT => { //dp type pattern match here--> no if statement 
            TFunT(tvars, a)
          }
          case _ => throw Illtyped
        }

      }



      //stackoverflow 
      case TApp(e, typs) => {
        //println("14") 
        var tp1 = getType(e,env)
        tp1 match {
            case TFunT(tvars, funt)=> {
              //println("15") 
              if(tvars.length != typs.length) throw Illtyped  //stackoverflow 
              //List(1,2,3).zip(List("one","two","three")) ==> List((1,one), (2,two), (3,three))
              //typs: Seq[Type]  tvars: Seq[TVar]
              replace(funt, (tvars zip typs).toMap)
            }

            case _ => {
              //println("16") 
              throw Illtyped
            }

        }
      }






    }
}
