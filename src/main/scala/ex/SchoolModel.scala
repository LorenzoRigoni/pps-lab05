package ex

import util.Sequences.*

trait CourseModel:
  def name: String

trait TeacherModel:
  def name: String
  def courses: Sequence[CourseModel]

trait SchoolModel:
    /**
     * This method should return the list of courses
     * e.g.,
     * emptySchool.courses // => Nil()
     * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).courses // => Cons("Math", Nil())
     * emptySchool
     * .setTeacherToCourse(teacher("John"), course("Math"))
     * .setTeacherToCourse(teacher("John"), course("Italian")).courses // => Cons("Math", Cons("Italian", Nil()))
     * Note!! If there are duplicates, just return them once
     *
     * @return the list of courses
     */
    def courses: Sequence[String]
    /**
     * This method should return the list of teachers
     * e.g.,
     * emptySchool.teachers // => Nil()
     * emptySchool.setTeacherToCourse(teacher("John"), course("Math")).teachers // => Cons("John", Nil())
     * val john = teacher("John")
     * emptySchool
     * .setTeacherToCourse(john, course("Math"))
     * .setTeacherToCourse(john, course("Italian")).teachers // => Cons("John", Nil())
     * Note!! If there are duplicates, just return them once
     *
     * @return the list of teachers
     */
    def teachers: Sequence[String]
    /**
     * This method should return a new school with the teacher assigned to the course
     * e.g.,
     * emptySchool
     * .setTeacherToCourse(teacher("John"), course("Math")) // => School(courses = Cons("Math", Nil()), teachers = Cons("John", Nil()), teacherToCourses = Cons(("John", "Math"), Nil()))
     * */
    def setTeacherToCourse(teacher: TeacherModel, course: CourseModel): Unit
    /**
     * This method should return the list of courses assigned to a teacher
     * e.g.,
     * emptySchool.coursesOfATeacher(teacher("John")) // => Nil()
     * emptySchool
     * .setTeacherToCourse(teacher("John"), course("Math"))
     * .coursesOfATeacher(teacher("John")) // => Cons("Math", Nil())
     * emptySchool
     * .setTeacherToCourse(teacher("John"), course("Math"))
     * .setTeacherToCourse(teacher("John"), course("Italian"))
     * .coursesOfATeacher(teacher("John")) // => Cons("Math", Cons("Italian", Nil()))
     *
     * @return the list of courses assigned to a teacher
     */
    def coursesOfATeacher(teacher: TeacherModel): Sequence[CourseModel]
    /**
     * This method should return true if the teacher is present in the school
     * e.g.,
     * emptySchool.hasTeacher("John") // => false
     * emptySchool
     * .setTeacherToCourse(teacher("John"), course("Math"))
     * .hasTeacher("John") // => true
     *
     */
    def hasTeacher(name: String): Boolean
    /**
     * This method should return true if the course is present in the school
     * e.g.,
     * emptySchool.hasCourse("Math") // => false
     * emptySchool
     * .setTeacherToCourse(teacher("John"), course("Math"))
     * .hasCourse("Math") // => true
     *
     */
    def hasCourse(name: String): Boolean

object CourseModel:
  def apply(name: String): CourseModel = CourseModelImpl(name)

  private case class CourseModelImpl(override val name: String) extends CourseModel

object TeacherModel:
  def apply(name: String): TeacherModel = TeacherModelImpl(name)
  def apply(name: String, courses: Sequence[CourseModel]): TeacherModel = TeacherModelImpl(name, courses)

  private case class TeacherModelImpl(override val name: String, override val courses: Sequence[CourseModel] = Sequence.apply()) extends TeacherModel

object SchoolModel:
  def apply(): SchoolModel = SchoolModelImpl()

  private class SchoolModelImpl(private var teachersOfSchool: Sequence[TeacherModel] = Sequence.apply()) extends SchoolModel:

    def courses: Sequence[String] = teachersOfSchool.flatMap(_.courses).map(_.name)

    def teachers: Sequence[String] = teachersOfSchool.map(_.name)

    def setTeacherToCourse(teacher: TeacherModel, course: CourseModel): Unit =
      val updatedTeachers = teachersOfSchool.map(
        t => TeacherModel.apply(t.name, t.courses.concat(Sequence(course))))
      if teachersOfSchool.contains(teacher) then
        teachersOfSchool = updatedTeachers
      else
        teachersOfSchool = teachersOfSchool.concat(Sequence.apply(TeacherModel.apply(teacher.name, Sequence.apply(course))))

    def coursesOfATeacher(teacher: TeacherModel): Sequence[CourseModel] = teacher.courses

    def hasTeacher(name: String): Boolean = teachers.contains(name)

    def hasCourse(name: String): Boolean = courses.contains(name)

@main def examples(): Unit =
  import SchoolModel.*
  import TeacherModel.*
  import CourseModel.*

  val school = SchoolModel.apply()
  println(school.teachers) // Nil()
  println(school.courses) // Nil()
  println(school.hasTeacher("John")) // false
  println(school.hasCourse("Math")) // false
  val john = TeacherModel.apply("John")
  val math = CourseModel.apply("Math")
  val italian = CourseModel.apply("Italian")
  school.setTeacherToCourse(john, math)
  println(school.teachers) // Cons("John", Nil())
  println(school.courses) // Cons("Math", Nil())
  println(school.hasTeacher("John")) // true
  println(school.hasCourse("Math")) // true
  println(school.hasCourse("Italian")) // false
  school.setTeacherToCourse(john, italian)
  println(school.teachers) // Cons("John", Nil())
  println(school.courses) // Cons("Math", Cons("Italian", Nil()))
  println(school.hasTeacher("John")) // true
  println(school.hasCourse("Math")) // true
  println(school.hasCourse("Italian")) // true
  println(school.coursesOfATeacher(john)) // Cons("Math", Cons("Italian", Nil()))