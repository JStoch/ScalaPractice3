import Maybe.{Just, Nothing}
import SolidFigure.{Cuboid, Sphere}
import WeekDay.{Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday}

@main
def main(): Unit = {
  val s = SolidFigure.Sphere(3.0)
  println(volume(s))
}

//product type representing a point
type point2D = (Float, Float)

//calculate the distance between two points
def distance(p1: point2D, p2: point2D): Double = {
  val (x1, y1) = p1
  val (x2, y2) = p2
  Math.sqrt(Math.pow(x1-x2,2)+Math.pow(y1-y2,2))
}

//a class representing a person
class person1(var name: String, var surname: String, var age: Int, var gender: String, var shoe: Int)

//a class representing a group of two people
class partnership1(var p1: person1, var p2: person1)

//return the younger person from the group
def younger1 (p: partnership1): person1 = {
  if p.p1.age < p.p2.age then p.p1 else p.p2
}

//same as previous block of code, but this time using tuples
type person2 = (String, String, Int, String, Int)
type partnership2 = (person2, person2)
def younger2(p: partnership2): person2 = {
  if p._1._3 < p._1._3 then p._1 else p._2
}

//enumeration representing days of the week
enum WeekDay:
  case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

//turn WeekDay to the name of the day in polish
def weekDayToString (d: WeekDay): String = {
  d match
    case Monday => "poniedziałek"
    case Tuesday => "wtorek"
    case Wednesday => "środa"
    case Thursday => "czwartek"
    case Friday => "piątek"
    case Saturday => "sobota"
    case Sunday => "niedziela"
}

//return the next day of the week
def nextDay (d: WeekDay): WeekDay = {
  d match
    case Monday => Tuesday
    case Tuesday => Wednesday
    case Wednesday => Thursday
    case Thursday => Friday
    case Friday => Saturday
    case Saturday => Sunday
    case Sunday => Monday
}

//own implementation of Option
enum Maybe:
  case Just[A](val a: A) extends Maybe
  case Nothing

//own implementation of list.headOption
def safeHead[A](l: List[A]): Maybe = {
  l match
    case Nil => Nothing
    case h::t => Just(h)
}

//enumeration representing different solid figures
enum SolidFigure:
  case Cuboid(var x: Float, var y: Float, var z: Float)
  case Cone(var r: Float, var z: Float)
  case Cylinder(var r: Float, var z: Float)
  case Sphere(var r: Float)

//calculate the volume of given solid figure
def volume(s: SolidFigure): Double = {
  s match
    case SolidFigure.Cuboid(x, y, z) => x * y * z
    case SolidFigure.Cone(r, z) => (1.0 / 3.0) * 3.14 * Math.pow(r, 2) * z
    case SolidFigure.Cylinder(r, z) => 3.14 * Math.pow(r, 2) * z
    case SolidFigure.Sphere(r) => (4.0/3.0)*3.14*Math.pow(r, 3)

}



