package tuner

import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.IMain

trait InterpretedFunction {
  def apply(args:List[Float]) : Float = {
    if(args.length != argCount) {
      throw new IllegalArgumentException("passed args length (" + args.length + ") does not equal needed args (" + argCount + ")")
    }

    applyInternal(function, args)
  }

  val argCount:Int
  val function:Function1[Float,_]

  private def applyInternal(f:Function1[Float,_], args:List[Float]) : Float = 
    // This method is driven by the remaining args, not 
    // the characteristics of f!
    args match {
      case hd :: Nil => f(hd).asInstanceOf[Float]
      case hd :: tl  => applyInternal(f(hd).asInstanceOf[Function1[Float,_]], tl)
  }
}

object FunctionCompiler {

  // TODO: use a better error function!
  def compileError(str:String) = println(str)

  def compile(src:String) : InterpretedFunction = {
    val i = new IMain(new Settings(compileError))

    // Compile the source function
    i.interpret(src)
    
    // Conveniently, there should only be one symbol in a new interpreter
    val functionName = i.definedTerms(0) toString

    // Create storage for the new function
    var res = Array[AnyRef](null)
    i.bind("result", "Array[AnyRef]", res)

    // Extraction!
    i.interpret("result(0) = " + functionName + " _")

    // Fun with Scala's specificity over function arguments
    res(0) match {
      case x:Function1[Float,Float] => 
        new InterpretedFunction {
          val argCount = 1
          val function = x
        }
      case x:Function2[Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 2
          val function = x.curried
        }
      case x:Function3[Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 3
          val function = x.curried
        }
      case x:Function4[Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 4
          val function = x.curried
        }
      case x:Function5[Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 5
          val function = x.curried
        }
      case x:Function6[Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 6
          val function = x.curried
        }
      case x:Function7[Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 7
          val function = x.curried
        }
      case x:Function8[Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 8
          val function = x.curried
        }
      case x:Function9[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 9
          val function = x.curried
        }
      case x:Function10[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 10
          val function = x.curried
        }
      case x:Function11[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 11
          val function = x.curried
        }
      case x:Function12[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 12
          val function = x.curried
        }
      case x:Function13[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 13
          val function = x.curried
        }
      case x:Function14[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 14
          val function = x.curried
        }
      case x:Function15[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 15
          val function = x.curried
        }
      case x:Function16[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 16
          val function = x.curried
        }
      case x:Function17[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 17
          val function = x.curried
        }
      case x:Function18[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 18
          val function = x.curried
        }
      case x:Function19[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 19
          val function = x.curried
        }
      case x:Function20[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 20
          val function = x.curried
        }
      case x:Function21[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 21
          val function = x.curried
        }
      case x:Function22[Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float,Float] => 
        new InterpretedFunction {
          val argCount = 22
          val function = x.curried
        }
    }
  }

}

