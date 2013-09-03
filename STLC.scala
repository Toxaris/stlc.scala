object STLC {
  // HELPER FUNCTIONS

  def ??? = throw new Exception("???")

  def typeErrorNotTheSame(context: String, expected: Any, actual: Any) =
    typeError("expected %s instead of %s in %s".format(expected, actual, context))

  def typeErrorNotDefined(name : Name) =
    typeError("undefined identifier " + name)

  def typeError(msg: String) = throw new Exception("Type error: " + msg)

  // TYPES

  trait Type
  case object NumberType extends Type
  case class FunctionType(domain: Type, range: Type) extends Type
  case class MapType(domain: Type, range: Type) extends Type

  // TYPED VARIABLES
  type Name = String
  case class Variable(name: Name, _type: Type)

  // TERMS
  trait Typed {
    /** Returns type or fails horribly. */
    def _type : Type
  }

  trait Constant extends Typed
  case class LookupConstant(domain : Type, range : Type) extends Constant {
    override def _type : Type = FunctionType(MapType(domain, range), FunctionType(domain, range))
  }

  trait Term extends Typed {
    // forces early failure for object-level type checking
    _type
  }

  case class VarTerm(variable: Variable) extends Term {
    override def _type = variable._type
  }

  case class AppTerm(operator: Term, operand: Term) extends Term {
    override def _type = (operator._type, operand._type) match {
      case (FunctionType(expectedDomain, range), actualDomain) =>
        if (expectedDomain == actualDomain)
          range
        else
          typeErrorNotTheSame("operand position", expectedDomain, operand + " : " + actualDomain)
      case _ => typeErrorNotTheSame("operator position", "function", operator._type)
    }
  }

  case class AbsTerm(variable: Variable, body: Term) extends Term {
    override def _type = FunctionType(variable._type, body._type)
  }

  case class ConstTerm(constant : Constant) extends Term {
    override def _type = constant._type
  }

  // TYPING CONTEXTS

  type TypingContext = List[Variable]

  def lookup(context: TypingContext, name: Name) : Variable =
    context filter (_.name == name) match {
      case (result :: _) => result
      case _ => typeErrorNotDefined(name)
    }

  def insert(variable: Variable, context: TypingContext) : TypingContext =
    variable :: context

  // SMART CONSTRUCTORS

  type Term2 = TypingContext => Term

  def appTerm2(operator: Term2, operand: Term2): Term2 =
    context =>
      AppTerm(operator(context), operand(context))

  def absTerm2(variable: Variable, body: Term2): Term2 =
    context =>
      AbsTerm(variable, body(insert(variable, context)))

  def varTerm2(name: Name): Term2 =
    context =>
      VarTerm(lookup(context, name))

  def lookupTerm2(map: Term2, key: Term2): Term2 =
    context => {
      val typedMap = map(context)
      val typedKey = key(context)
      typedMap._type match {
        case MapType(keyType, valType) =>
          AppTerm(AppTerm(ConstTerm(LookupConstant(keyType, valType)),
                          typedMap),
                  typedKey)
        case _ =>
          typeErrorNotTheSame("use of lookup", "map", typedMap._type)
      }
    }

  // Hack for testing

  case object Type1 extends Type
  case object Type2 extends Type

  // should and does succeed:

  val test1 =
    absTerm2(Variable("theMap", MapType(Type1, Type1)),
             absTerm2(Variable("theKey", Type1),
                      lookupTerm2(varTerm2("theMap"), varTerm2("theKey"))))

  val test2 =
    absTerm2(Variable("theMap", MapType(Type1, Type2)),
             absTerm2(Variable("theKey", Type1),
                      lookupTerm2(varTerm2("theMap"), varTerm2("theKey"))))

  // should fail and does fail:

  val test3 =
    absTerm2(Variable("theMap", MapType(Type1, Type1)),
             absTerm2(Variable("notTheKey", Type1),
                      lookupTerm2(varTerm2("theMap"), varTerm2("theKey"))))

  val test4 =
    absTerm2(Variable("theMap", MapType(Type2, Type2)),
             absTerm2(Variable("theKey", Type1),
                      lookupTerm2(varTerm2("theMap"), varTerm2("theKey"))))

  val test5 =
    absTerm2(Variable("theMap", MapType(Type1, Type2)),
             absTerm2(Variable("theKey", Type2),
                      lookupTerm2(varTerm2("theMap"), varTerm2("theKey"))))

  val test6 =
    absTerm2(Variable("theMap", MapType(Type1, Type1)),
             absTerm2(Variable("theKey", Type1),
                      lookupTerm2(varTerm2("theMap"), varTerm2("theMap"))))

  // HOAS INTERFACE :)

  object fresh extends Function2[TypingContext, Name, Name]{
    var index = 0;

    def apply(context : TypingContext, prefix : Name) : Name = {
      val result = prefix + index
      index += 1
      result
    }
  }

  def absTerm3(domain : Type, body: Term2 => Term2): Term2 =
    context => {
      val name = fresh(context, "x")
      val variable = Variable(name, domain)
      AbsTerm(variable, body(_ => VarTerm(variable))(insert(variable, context)))
    }


}

import STLC._

test1(List())

test2(List())

test3(List())

test4(List())

test5(List())

test6(List())
